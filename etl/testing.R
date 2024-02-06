
create_df <- function(df, variable, period){
  
  #sel_variables <- variable
  
  df_modeshare_bars <- df |> 
    filter(mode_aggregated == "Mass transit, cycling and walking",
           year == period) |> 
    select(c40_region, {{variable}}) |>
    rename("var1" = {{variable}}) |> 
    mutate(
      across(where(is.numeric), \(x) x/100),
      col_color = case_when(var1 > 0 & var1 < 0.30 ~ c40_colors("red"),
                            var1 > 0.30 & var1 < 0.60 ~ c40_colors("yellow"),
                            var1 > 0.60 ~ c40_colors("green")))
  
  return(df_modeshare_bars)
  
}

df1 <- create_df(df = df_modeshare, "trips_perc", 2022)
df1 <- create_df(df = df_modeshare, "km_perc", 2022)


create_table <- function(var){
  
  df <- df1 <- create_df(df = df_modeshare, var, 2022)
   
  reactable(
  df,
  pagination = FALSE,
  columns = list(
    var1 = colDef(cell = data_bars(df, 
                                         text_position = "inside-end", 
                                         number_fmt = scales::percent,
                                         max_value = 1,
                                         box_shadow = TRUE,
                                         #fill_color = c40_colors("blue"),
                                         fill_color_ref = "col_color")),
    col_color = colDef(show = FALSE)
  )
)
}

create_table("trips_perc")  

df <- create_df(df = df_modeshare, "trips_perc", 2022)

reactable(
  df,
  pagination = FALSE,
  columns = list(
    var1 = colDef(cell = data_bars(df, 
                                   text_position = "inside-end", 
                                   number_fmt = scales::percent,
                                   max_value = 1,
                                   box_shadow = TRUE,
                                   #fill_color = c40_colors("blue"),
                                   fill_color_ref = "col_color")),
    col_color = colDef(show = FALSE)
  )
)
df_table_trips_tot <- df_modeshare |> 
  #select(c40_region, year, trips_n) |> 
  summarise(trips_n = sum(trips_n),
            km_n = sum(km_n)
            .by = "year") |> 
  mutate(trips_perc = round(c40_city_wide / sum(c40_city_wide), 2),
         col_color = case_when(trips_perc > 0 & trips_perc < 0.30 ~ c40_colors("red"),
                               trips_perc > 0.30 & trips_perc < 0.60 ~ c40_colors("yellow"),
                               trips_perc > 0.60 ~ c40_colors("green")),
         c40_region = "C40 city wide") |> 
  select(-c40_city_wide) |> 
  pivot_wider(names_from = "year", values_from = c("trips_perc", "col_color"))

df_table_trips_reg <-   df_modeshare |> 
  select(c40_region, year, trips_perc) |> 
  mutate(trips_perc = trips_perc / 100,
         col_color = case_when(trips_perc > 0 & trips_perc < 0.30 ~ c40_colors("red"),
                               trips_perc > 0.30 & trips_perc < 0.60 ~ c40_colors("yellow"),
                               trips_perc > 0.60 ~ c40_colors("green"))) |> 
  pivot_wider(names_from = "year", values_from = c("trips_perc", "col_color"))
  
table_c40_wide <- reactable(
  df_table_trips_tot,
  pagination = FALSE,
  columns = list(
    # trips_perc_2018 = colDef(name = "2018",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2018")),
    # trips_perc_2019 = colDef(name = "2019",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2019")),
    # trips_perc_2020 = colDef(name = "2020",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2020")),
    # trips_perc_2021 = colDef(name = "2021",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2021")),
    trips_perc_2022 = colDef(name = "2022",
                             cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
                                              max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2022")),
    col_color_2018 = colDef(show = FALSE),
    col_color_2019 = colDef(show = FALSE),
    col_color_2020 = colDef(show = FALSE),
    col_color_2021 = colDef(show = FALSE),
    col_color_2022 = colDef(show = FALSE)
  )
)




#df_table_trips_tot <- 
  df_modeshare |> 
  summarise(trips_n = sum(trips_n),
            km_n = sum(km_n),
            .by = "year") |> 
  mutate(trips_perc = round(trips_n / sum(trips_n), 2),
         km_perc = round(km_n / sum(km_n), 2),
         color_trips = case_when(trips_perc > 0 & trips_perc < 0.30 ~ c40_colors("red"),
                               trips_perc > 0.30 & trips_perc < 0.60 ~ c40_colors("yellow"),
                               trips_perc > 0.60 ~ c40_colors("green")),
         color_km = case_when(km_perc > 0 & km_perc < 0.30 ~ c40_colors("red"),
                                  km_perc > 0.30 & km_perc < 0.60 ~ c40_colors("yellow"),
                                  km_perc > 0.60 ~ c40_colors("green")),
         c40_region = "C40 city wide") |> 
  pivot_wider(names_from = "year", values_from = c("trips_n", "trips_perc", "km_n", "km_perc", "color_trips", "color_km"))

  
df_table_trips_reg <-   df_modeshare |> 
  select(-mode_aggregated) |> 
  mutate(across(ends_with("perc"),  = trips_perc / 100,
         col_color = case_when(trips_perc > 0 & trips_perc < 0.30 ~ c40_colors("red"),
                               trips_perc > 0.30 & trips_perc < 0.60 ~ c40_colors("yellow"),
                               trips_perc > 0.60 ~ c40_colors("green"))) |> 
  pivot_wider(names_from = "year", values_from = c("trips_perc", "col_color"))

#table_c40_wide <- 
  reactable(
  df_table_trips_tot,
  pagination = FALSE,
  columns = list(
    # trips_perc_2018 = colDef(name = "2018",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2018")),
    # trips_perc_2019 = colDef(name = "2019",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2019")),
    # trips_perc_2020 = colDef(name = "2020",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2020")),
    # trips_perc_2021 = colDef(name = "2021",
    #                          cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
    #                                           max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2021")),
    trips_perc_2022 = colDef(name = "2022",
                             cell = data_bars(df_table_trips_tot, text_position = "inside-end",  number_fmt = scales::percent,
                                              max_value = 1, box_shadow = TRUE, fill_color_ref = "col_color_2022",
                                              img = 'https://www.pngkit.com/png/detail/54-544889_45-top-view-of-car-clipart-images-racecar.png',
                                              img_height = 20,
                                              img_width = 25)),
    col_color_2018 = colDef(show = FALSE),
    col_color_2019 = colDef(show = FALSE),
    col_color_2020 = colDef(show = FALSE),
    col_color_2021 = colDef(show = FALSE),
    col_color_2022 = colDef(show = FALSE)
  )
)

  
  
  library(dplyr)
  library(sparkline)
  
  df_sparkline_trips_perc <- df_modeshare %>%
    group_by(c40_region) %>%
    summarise(trips_perc = list(trips_perc))
  
  reactable(data, columns = list(
    weight = colDef(cell = function(values) {
      sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(df_modeshare$trips_perc))
    }),
    boxplot = colDef(cell = function(value, index) {
      sparkline(data$weight[[index]], type = "box")
    }),
    sparkline = colDef(cell = function(value, index) {
      sparkline(data$weight[[index]])
    })
  ))