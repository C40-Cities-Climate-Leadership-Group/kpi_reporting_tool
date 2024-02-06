
### Load data
# df_kpi_emissions <- readRDS(here("kpi/data/df_kpi_emissions.rds"))
# df_kpi_wide_emissions <- read_rds(here("kpi/data/df_kpi_wide_emissions.rds"))
# df_kpi_regional_emissions <- readRDS(here("kpi/data/df_kpi_regional_emissions.rds"))
# df_ghg_inventories <- readRDS(here("kpi/data/df_ghg_inventories.rds"))

# bind two df
df_kpi_emissions <- df_kpi_wide_emissions |> 
  bind_rows(df_kpi_regional_emissions) 

variables <- c("baseline_2020", 
               paste0("actual_", c(2015:2019, 2021:2024, 2030, 2050)),
               paste0("target_", c(2015:2019, 2021:2024, 2030, 2050)))

df_kpi_emissions <- df_kpi_emissions |> 
  mutate(across(.cols = c("actual_emissions_tco2e", "target_emissions_tco2e"), ~ as.numeric(.))) |> 
  #filter(year %in% c(2021:2024, 2030, 2050)) |> 
  select(!c(num_cities)) |>
  pivot_longer(cols = starts_with(c("actual", "target"))) |> 
  mutate(emission_type = case_when(str_detect(name, "actual") ~ "actual",
                                   str_detect(name, "target") ~ "target"),
         emission_indicator = case_when(str_detect(name, "per_capita") ~ "per capita",
                                        .default = "absolute")) |> 
  select(-name) |> 
  pivot_wider(names_from = c("emission_type", "year"), values_from = "value") |> 
  mutate(baseline_2020 = case_when(entity_type == "City" & emission_indicator == "absolute" ~ 2.59,
                                   entity_type == "City" & emission_indicator == "per capita" ~ 4.44,
                                   .default = NA)) |> 
  select(entity, entity_type, emission_indicator, all_of(variables)) |> 
  mutate(on_off_track_target23 = round((target_2023 - actual_2023) / target_2023, 4),
         #on_off_track_target22 = round((target_2022 - actual_2023) / target_2022, 4),
         id = row_number()) 


df_kpi_emissions <- df_kpi_emissions |> 
  filter(emission_indicator == "absolute") |> 
  mutate(across(.cols = c(starts_with(c("actual", "target", "mid_year"))), ~ round(./1000000000, digits = 3))) |> 
  rbind(df_kpi_emissions |> 
          filter(emission_indicator != "absolute")) |> 
  arrange(id)
  # mutate(entity = str_wrap(entity, 10),
  #        entity = factor(entity, 
  #                        level = c("C40 wide\ntotal", "Central\nEast Asia", "East,\nSoutheast\nAsia and\nOceania",
  #                                  "South and\nWest Asia", "Latin\nAmerica", "Europe", "North\nAmerica", "Africa" )))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              GPC INVENTORIES                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_ghg_inventories <- df_ghg_inventories |> 
  mutate(n_inventories_range = case_when(n_inventories == 0 ~ "No inventories",
                                         n_inventories == 1 ~ "1 inventory",
                                         n_inventories > 1 ~ "2 or more inventories"),
         n_inventories_range = factor(n_inventories_range, 
                                      levels = c("No inventories", "1 inventory", "2 or more inventories")))


tab_inventories <- df_ghg_inventories |> 
  count(n_inventories_range)

### For drilldown vizualization
by_subtype <- df_ghg_inventories |>
  mutate(last_inventory = case_when(last_inventory <= 2018 ~ "2018 or older",
                                    .default = as.character(last_inventory))) |> 
  group_by(n_inventories_range) |> 
  count(last_inventory) |> 
  ungroup() |> 
  #create nested data at parent level - workout type
  group_nest(n_inventories_range) |>
  mutate(
    #id should be set to parent level
    id = n_inventories_range,
    #type specifies chart type
    type = "column",
    #drilldown data should contain arguments for chart - use purrr to map
    data = purrr::map(data, mutate, name = last_inventory, y  = n),
    data = purrr::map(data, list_parse)
  )



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                 Mode share                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#........................clean col names.........................
for (i in c("full_distance_km", "emissions_factor_kg_co2e_per_liter", "on_foot",
            "fuel_efficiency_km_per_liter")) {
  colnames(df_transport_orig) <- str_replace(string = colnames(df_transport_orig), 
                                             pattern = i, 
                                             replacement = str_replace_all(i, "_", "."))
}

df_transport <- df_transport_orig |> 
  filter(feature_id %in% unique(df_metadata$feature_id))

#......................Re-structure dataset......................
df_transport <- df_transport |> 
  mutate(name = case_when(name == "New York" ~ "New York City",
                          name == "İstanbul" ~ "Istanbul",
                          name == "Delhi" ~ "Delhi NCT",
                          name == "Washington" ~ "Washington, DC",
                          name == "Durban" ~ "Durban (eThekwini)",
                          name == "City of Tshwane Metropolitan Municipality" ~ "Tshwane",
                          name == "Miami-Dade County" ~ "Miami",
                          name == "Medellín" ~ "Medellin",
                          .default = name),
         name = clean_text(name)) |> 
  pivot_longer(cols = starts_with(c("automobile", "bus", "cycling", "ferry", 
                                    "motorcycle", "on.foot", "rail", "subway",
                                    "tram")),
               names_to = "mode",
               values_to = "value") |> 
  relocate(mode, value, .after = state) |> 
  separate(col=mode, into=c('mode', 'travel_bounds',"xxx"), sep='_') |> 
  mutate(indicator = case_when(is.na(xxx) ~ travel_bounds,
                               .default = xxx),
         travel_bounds = case_when(str_detect(travel_bounds, "fuel|emissions") ~ "factors",
                                   .default = travel_bounds),
         value = round(value, 2),
         indicator = str_replace_all(indicator, "\\.", "_")) |> 
  select(-xxx) |> 
  pivot_wider(names_from = indicator,
              values_from = value) |> 
  arrange(name, year, mode) |> 
  mutate(full_distance_km = round(as.numeric(full_distance_km) / 1000000, 0),
         trips = round(trips / 1000000, 1)) |> 
  left_join(df_metadata |> 
              select(feature_id, city, c40_city, c40_country, c40_region), 
            by = "feature_id") |> 
  filter(travel_bounds != "factors") |> 
  left_join(df_dim_cities, by = c("c40_city" = "city")) |> 
  filter(current_member == TRUE)


df_modeshare <- df_transport |> 
  select(c40_city, c40_country, c40_region, year, mode, trips, full_distance_km) |> 
  mutate(mode_aggregated = case_when(mode %in% c("bus", "cycling", "on.foot", "rail", "subway", "tram") ~ "Mass transit, cycling and walking",
                                     .default = "Automobile, ferry and motorcycle")) |> 
  summarise(trips_n = sum(trips, na.rm = T),
            km_n = sum(full_distance_km, na.rm = T),
            .by = c("c40_region", "year", "mode_aggregated")) |> 
  group_by(c40_region, year) |> 
  mutate(trips_perc = trips_n / sum(trips_n) * 100,
         km_perc = km_n / sum(km_n) * 100) |> ungroup() |> 
  mutate(across(ends_with("perc"), round, 1)) |> 
  bind_rows(df_transport |> 
  select(c40_city, c40_country, c40_region, year, mode, trips, full_distance_km) |> 
  mutate(mode_aggregated = case_when(mode %in% c("bus", "cycling", "on.foot", "rail", "subway", "tram") ~ "Mass transit, cycling and walking",
                                     .default = "Automobile, ferry and motorcycle")) |> 
  summarise(trips_n = sum(trips, na.rm = T),
            km_n = sum(full_distance_km, na.rm = T),
            .by = c("year", "mode_aggregated")) |> 
  group_by(year) |> 
  mutate(trips_perc = trips_n / sum(trips_n) * 100,
         km_perc = km_n / sum(km_n) * 100) |> ungroup() |> 
  mutate(across(ends_with("perc"), round, 1),
         c40_region = "C40 city wide")) |> 
    filter(mode_aggregated == "Mass transit, cycling and walking")
