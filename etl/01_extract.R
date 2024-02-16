

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                GHG emissions                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
con <- get_dw_connection()
#get_dw_db_example()

# Get kpi_c40_wide_emissions View
df_kpi_wide_emissions <- tbl(con,
                             Id(schema = "public",
                                table = "kpi_c40_wide_emissions")) |> 
  collect()

df_kpi_wide_emissions <- df_kpi_wide_emissions |> 
  mutate(entity_type = "City")


# Get kpi_regional_emissions View
df_kpi_regional_emissions <- tbl(con,
                                 Id(schema = "public",
                                    table = "kpi_regional_emissions")) |> 
  collect()

# check similarity between columns names
#colnames(df_kpi_wide_emissions) %in% colnames(df_kpi_regional_emissions)

# Adapt first column name
df_kpi_regional_emissions <- df_kpi_regional_emissions |> 
  rename(entity = region) |> 
  mutate(entity_type = "Region")

# check similarity between columns names
#colnames(df_kpi_wide_emissions) %in% colnames(df_kpi_regional_emissions)


### Extract
### Access metadata in DW
# con <- get_dw_connection()
# 
# df_metadata_orig <- tbl(con,
#                         Id(schema = "public",
#                            table = "fact_eie_metadata")) |> 
#   collect()
# 
# 
# ### Transform
# df_metadata <- df_metadata_orig |> 
#   mutate(city = case_when(city == "New York" ~ "New York City",
#                           city == "İstanbul" ~ "Istanbul",
#                           city == "Delhi" ~ "Delhi NCT",
#                           city == "Washington" ~ "Washington, DC",
#                           city == "Durban" ~ "Durban (eThekwini)",
#                           city == "City of Tshwane Metropolitan Municipality" ~ "Tshwane",
#                           city == "Miami-Dade County" ~ "Miami",
#                           city == "Medellín" ~ "Medellin",
#                           .default = city),
#          city_clean = clean_text(city)) |> 
#   filter(city_clean != "bogotá",
#          (city_clean != "Miami" & area_km2 != 6309),
#          (city_clean != "ahmedabad" & area_km2 != 7037),
#          (city_clean != "medellin" & area_km2 != 113))
# 
# 
# 
# ### Get dim_cities table to get c40 cities region
# df_dim_cities <- tbl(con,
#                      Id(schema = "public",
#                         table = "dim_cities")) |>
#   filter(current_member == TRUE) |> 
#   select(city_id, c40_city = city, c40_country = country, c40_region = region) |> 
#   collect()
# 
# # Delete punctuation
# df_dim_cities <- df_dim_cities |> 
#   mutate(c40_city_clean = clean_text(c40_city),
#          c40_city_clean = case_when(c40_city_clean == "bogotá" ~ "bogota",
#                                     c40_city_clean == "montréal" ~ "montreal",
#                                     c40_city_clean == "medellín" ~ "medellin",
#                                     c40_city_clean == "cityoftshwanemetropolitanmunicipality" ~ "tshwane",
#                                     .default = c40_city_clean))
# 
# 
# ### Join between EIE Google's data and C40 one
# df_metadata <- df_metadata |> 
#   left_join(df_dim_cities, 
#             by = c("city_clean" = "c40_city_clean"),
#             keep = TRUE) |> 
#   mutate(c40_region = case_when(country == "United States" ~ "North America",
#                                 country == "United Kingdom" ~ "Europe",
#                                 country == "Philippines" ~ "East, Southeast Asia and Oceania",
#                                 .default = c40_region)) |> 
#   filter(!is.na(c40_city))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              GHG inventories                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# url <- "/Users/pablotiscornia/Library/CloudStorage/GoogleDrive-ptiscornia@c40.org/Shared drives/BPMI/Business Planning and Reporting/Monitoring & Evaluation/03_Data requests/03_Outputs/Nicole August 2023-Inventories by City/Inventories by city 09_08_23.xlsx"
# 
# df_ghg_inventories_test <- readxl::read_excel(url) |>
#   janitor::clean_names() |>
#   select(1:3)

df_ghg_inventories <- tbl(con,
          sql("
          SELECT DISTINCT t1.inventory_year, t1.city_id, city, protocol_id, use_in_dashboard, created_at
          FROM public.dim_city_inventories as t1
          LEFT JOIN dim_cities ON t1.city_id = dim_cities.city_id
          WHERE created_at = (SELECT MAX(created_at) FROM public.fact_city_sectoral_emissions) 
          and use_in_dashboard in ('true', 'not sharable')
          ORDER BY city ASC
              ")) |> 
  collect()

df_ghg_inventories <- df_ghg_inventories |> 
  filter(!is.na(inventory_year)) |> 
  group_by(city) |> 
  distinct(inventory_year, .keep_all = TRUE) |> 
  mutate(last_inventory = max(inventory_year)) |> ungroup() |> 
  summarise(n_inventories = n(),
            .by = c("city", "last_inventory")) |> 
  relocate(city, n_inventories, last_inventory)
  
df_dim_cities <- tbl(con,
                     Id(schema = "public",
                        
                        table = "dim_cities")) |> 
  select(city_id, city, current_member) |> 
  filter(current_member == TRUE) |> 
  collect()

df_ghg_inventories <- df_dim_cities |> 
  left_join(df_ghg_inventories, by = "city") |> 
  mutate(n_inventories = coalesce(n_inventories, 0))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Mode Share data                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(bigrquery)
# 
# ### Load c40 datawarehouse connection
# con <- c40tools::get_dw_connection()
# ### Available datasets in EIE Google Transport BigQuery system:
# tb_transport <- "`geo-eie-bq-users.eie_latest.transport-414538940`"
# # Set project to use BigQuery
# proj <- bq_test_project()
# # Set the dataset
# dataset <- tb_transport
# # Set sql query for accesing the dataset
# sql <- glue::glue("SELECT * FROM {dataset}")
# 
# # Access the dataset. An authentication is going to be requested
# tb <- bq_project_query(x = proj, 
#                        query = sql)
# 
# # Extract the dataset
# df_transport_orig <- bq_table_download(tb)


df_transport_orig <- readRDS(here("data/df_transport_orig.rds"))
df_metadata <- readRDS(here("data/df_metadata.rds"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                Save outputs                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#saveRDS(df_transport_orig, here("kpi/data/df_transport_orig.rds"))
#saveRDS(df_metadata, here("kpi/data/df_metadata.rds"))
# saveRDS(df_kpi_regional_emissions, here("kpi/data/df_kpi_regional_emissions.rds"))
# saveRDS(df_kpi_wide_emissions, here("kpi/data/df_kpi_wide_emissions.rds"))
# write_csv(df_ghg_inventories, here("kpi/data/df_ghg_inventories.csv"))
 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                  AQ data                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_aq <- tbl(con,
             sql("
          SELECT t1.city_id, city, country, region, current_member, year, population_weighted_pm25_ug_per_m3
          FROM public.fact_city_pm25_concentration t1
          LEFT JOIN dim_cities t2 ON t1.city_id = t2.city_id
          WHERE created_at = (SELECT MAX(created_at) FROM public.fact_city_pm25_concentration)
          and current_member = 'true'
              ")) |> 
  collect()
