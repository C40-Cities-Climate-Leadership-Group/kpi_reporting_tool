

filter_year <- 2023

# World layer
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load EV data 
source(here('doc_pages/ev/ev_data.R'))

df_ev_dataset_orig <- df_ev_dataset_orig |> 
  janitor::clean_names()

### DWH connection
con <- c40tools::get_dw_connection()

# Load city names
df_city_boundaries <- tbl(con,
                          Id(schema = "public",
                             table = "dim_city_boundaries")) |>
  select(city_id, latitude, longitude) |> 
  collect()

### Boundaries
df_cities_bd <- df_city_boundaries |> 
  left_join(df_dim_cities) |> 
  distinct(city_id, .keep_all = TRUE) |> 
  collect()

### ev datasets per region
i_regions <- c("Europe", "North America")

for (i in i_regions) {
  
  assign(
    x = paste0(ifelse(i == "Europe", "eu_", "na_"), "data"), 
    value = df_ev_dataset_orig |> 
      filter(year == filter_year,
           region == i) |> 
      left_join(df_cities_bd, by =  "city")  |> 
      mutate(
      # charging_point_situation = case_when(charging_points_per_millon >= target_us_eu_2022 ~ "above target 2022",
      #                                      charging_points_per_millon < target_us_eu_2022 ~ "below target 2022"),
      city_abb = abbreviate(names.arg = stringr::str_remove(city, ","), minlength = 2, use.classes = FALSE),
      city_abb = str_to_upper(city_abb)) |> 
    filter(current_member == TRUE)
  )
  
}

# ### North America DATA
# na_data <-  df_ev_dataset_orig |> 
#   filter(year == filter_year,
#          region == "North America") |> 
#   left_join(df_cities_bd, by =  "city")  |> 
#   mutate(
#     # charging_point_situation = case_when(charging_points_per_millon >= target_us_eu_2022 ~ "above target 2022",
#     #                                      charging_points_per_millon < target_us_eu_2022 ~ "below target 2022"),
#     city_abb = abbreviate(names.arg = stringr::str_remove(city, ","), minlength = 2, use.classes = FALSE),
#     city_abb = str_to_upper(city_abb)) |> 
#   filter(current_member == TRUE)
# 
# ### Europe DATA
# eu_data <- df_ev_dataset_orig |> 
#   filter(year == filter_year,
#          region == "Europe") |> 
#   left_join(df_cities_bd, by =  "city")  |> 
#   mutate(
#     # charging_point_situation = case_when(charging_points_per_millon >= target_us_eu_2022 ~ "above target 2022",
#     #                                      charging_points_per_millon < target_us_eu_2022 ~ "below target 2022"),
#     city_abb = abbreviate(names.arg = stringr::str_remove(city, ","), minlength = 2, use.classes = FALSE),
#     city_abb = str_to_upper(city_abb)) |> 
#   filter(current_member == TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                NORTH AMERICA                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here('doc_pages/ev/plot_labels.R'))

na_plot <- 
  ggplot() +
  geom_sf_interactive(data = world, colour = "grey", fill = "white") +
  geom_point_interactive(data = na_data, 
             aes(x = longitude, y = latitude,
                 size = charging_points_per_millon_pop,
                 data_id = city, tooltip = city),
             #colour = charging_point_situation),
             alpha = 0.6) +
  scale_size_continuous(str_wrap("# of charging points per millon population", width = 20),) +
  # scale_colour_manual(str_wrap("Situation against targets 2022", width = 20), 
  #                     values = c("above target 2022" = "#02c245",
  #                                "below target 2022" = "#ff614a")) +
  west_labels_repel +
  south_labels_repel +
  north_labels_repel +
  east_labels_repel +
  coord_sf(xlim = c(-170, -50), ylim = c(15, 75), expand = FALSE) +
  # labs(title = "Charging points per millon population.",
  #      subtitle = "North American Cities. Year 2022") +
  theme_void() +
  theme(
    #plot.margin = margin(2, 2, 2, 2, "cm"),
    text = element_text(family = "Fira Mono"),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 15),
    legend.position = "none",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
    #plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )


na_table <- na_data |> 
  select(city, city_abb, charging_points_per_millon_pop) |> 
  mutate(
    tooltip_text = paste0(toupper(city), "\n", 
                          format(charging_points_per_millon_pop, big.mark = ","))) |> 
  arrange(city) |> 
  select(-city_abb) |> 
  ggplot() +
  geom_col_interactive(
    aes(
      x = charging_points_per_millon_pop, 
      y = reorder(city, charging_points_per_millon_pop),
      tooltip = tooltip_text, 
      data_id = city),
    fill = '#23BCED') +
  geom_vline(xintercept = 0) +
  labs(x = "Chargin points per millon population", y = "City") +
  theme_minimal()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   EUROPE                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eu_plot <- 
  ggplot() +
  geom_sf_interactive(data = world, colour = "grey", fill = "white") +
  geom_point_interactive(data = eu_data, 
                         aes(x = longitude, y = latitude,
                             size = charging_points_per_millon_pop,
                             data_id = city, tooltip = city),
                         # colour = charging_point_situation),
                         alpha = 0.6) +
  scale_size_continuous(str_wrap("# of charging points per millon population", width = 20),) +
  # scale_colour_manual(str_wrap("Situation against targets 2022", width = 20), 
  #                     values = c("above target 2022" = "#02c245",
  #                                "below target 2022" = "#ff614a")) +
  eu_center_labels_repel +
  eu_south_labels_repel +
  eu_north_labels_repel +
  # east_labels_repel +
  coord_sf(xlim = c(-10, 35), ylim = c(30, 77), expand = FALSE) +
  # labs(title = "Charging points per millon population.",
  #      subtitle = "European Cities. Year 2022") +
  theme_void() +
  theme(
    #plot.margin = margin(2, 2, 2, 2, "cm"),
    text = element_text(family = "Fira Mono"),
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(size = 15),
    legend.position = "none",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)
    #plot.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  )


eu_table <- eu_data |> 
  select(city, city_abb, charging_points_per_millon_pop) |> 
  mutate(
    #city = paste(city_abb, city, sep = " - "),
    tooltip_text = paste0(toupper(city), "\n", 
                          format(charging_points_per_millon_pop, big.mark = ","))) |> 
  arrange(city) |> 
  select(-city_abb) |> 
  ggplot() +
  geom_col_interactive(
    aes(
      x = charging_points_per_millon_pop, 
      y = reorder(city, charging_points_per_millon_pop),
      tooltip = tooltip_text, 
      data_id = city),
    fill = '#23BCED') +
  geom_vline(xintercept = 0) +
  labs(x = "Chargin points per millon population", y = "City") +
  theme_minimal()



