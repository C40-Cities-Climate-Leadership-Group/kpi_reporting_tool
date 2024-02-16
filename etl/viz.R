# install.packages("remotes")
#remotes::install_github("nsgrantham/ggbraid")

### graphs functions
source(here::here("etl/functions.R"))
#source(here::here("etl/function_viz.R"))




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Emissions vs Target viz                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- df_kpi_emissions |> 
  mutate(entity = str_wrap(entity, 10),
         entity = factor(entity,
                         level = c("C40 wide\ntotal", "Central\nEast Asia", "East,\nSoutheast\nAsia and\nOceania",
                                   "South and\nWest Asia", "Latin\nAmerica", "Europe", "North\nAmerica", "Africa" )))

for (i in c("absolute", "per capita")) {
  assign(paste0("viz_emissions_vs_target_", str_remove(i, " ")),
         df |>
           filter(emission_indicator == i) |>
           select(entity, actual_2023, target_2023) |>
           pivot_longer(-entity, names_to = "variable", values_to = "values") |>
           mutate(values = round(values, 2),
                  facet = case_when(entity == "C40 wide\ntotal" ~ "C40 wide total",
                                    .default = "Regions")) |>
           ggplot(aes(x = entity, y = values, label = values)) +
           # geom_col_interactive(aes(fill = variable, tooltip = values, data_id = entity),
           #                      color = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", "black", "darkgrey"),
           #                      alpha = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", 1, 0.8),
           #                      position = position_dodge(width = -1)) +
           geom_col(aes(fill = variable),
                    color = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", "black", "darkgrey"),
                    alpha = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", 1, 0.8),
                    position = position_dodge(width = -1)) +
           geom_text(aes(group = variable),
                     position = position_dodge(width = -1), vjust = -0.5) +
           geom_hline(yintercept = 0, color = "grey") +
           facet_wrap("facet", scales = ifelse(i == "absolute", "free", "free_x")) +
           scale_fill_manual(name = "",
                             labels = c("actual_2023" = glue::glue("Estimated {i} emissions"),
                                        "target_2023" = glue::glue("Target {i} emissions")),
                             values = c(c40_colors("blue"), c40_colors("green"))) +
           labs(x = "Entity", y = "CO2e gigatones in 2023") +
           theme_minimal() +
           theme(
             legend.position = "top",
             panel.grid.major.x = element_line(colour = NA),
             panel.grid.minor.x = element_line(colour = NA))

  )

}

list_viz_emissions_vs_target <- list(viz_emissions_vs_target_absolute,
                                     viz_emissions_vs_target_percapita)


viz_type <- c("absolute", "percapita")

for (i in seq_along(viz_type)) {
  assign(glue("viz_emissions_vs_target_{viz_type[i]}_giraf"),

         girafe(ggobj = list_viz_emissions_vs_target[[i]],
                width_svg = 14, height_svg = 7) |>
           girafe_options(
             opts_toolbar(
               position = "topright",
               delay_mouseout = 2000),
             #     pngname = "city_tot"),
             #   # opts_tooltip(
             #   #   opacity = 0.8,
             #   #   use_fill = TRUE,
             #   #   use_stroke = FALSE,
             #   #   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
             opts_zoom(max = 1),
             #   # opts_hover(
             #   #   css = girafe_css(
             #   #     css = "fill:'#000000';opacity:0.4",
             #   #     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
             #   #   )),
             opts_sizing(rescale = FALSE, width = 1)
           )
         )
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  GHG total historical by region - GtCO₂e                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (i in c("absolute", "per capita")) {
  assign(paste0("viz_emissions_history_", str_remove(i, " ")),
         viz_emissions_target_history(df_kpi_wide_emissions, 
                                      per_capita = ifelse(i == "per capita", TRUE, FALSE))
  )
}

list_viz_history <- list(viz_emissions_history_absolute, 
                         viz_emissions_history_percapita)

viz_type <- c("absolute", "percapita")

for (i in seq_along(viz_type)) {
  assign(glue("viz_emissions_history_{viz_type[i]}_giraf"),
         
         girafe(ggobj = list_viz_history[[i]], 
                width_svg = 14, height_svg = 7) |> 
           girafe_options(
             opts_toolbar(
               position = "topright",
               delay_mouseout = 2000),
             #     pngname = "city_tot"),
             #   # opts_tooltip(
             #   #   opacity = 0.8,
             #   #   use_fill = TRUE,
             #   #   use_stroke = FALSE,
             #   #   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
             opts_zoom(max = 1),
             #   # opts_hover(
             #   #   css = girafe_css(
             #   #     css = "fill:'#000000';opacity:0.4",
             #   #     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
             #   #   )),
             opts_sizing(rescale = FALSE, width = 1)
           )
  )
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Comparison between years: Wide emissions                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_hist_total_absolute <- viz_emissions_target_history(df_kpi_wide_emissions,
                                                  per_capita = FALSE)

viz_hist_total_percapita <- viz_emissions_target_history(df_kpi_wide_emissions,
                                                    per_capita = TRUE)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  ABSOLUTE                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Bar trend plots                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_list_hist_absolute <- list()
for (reg_val in seq_along(unique(df_kpi_regional_emissions$entity))) {
  
  reg_name <- unique(df_kpi_regional_emissions$entity)[reg_val]
  
  viz <- glue::glue("{tolower(str_replace_all(reg_name, ',', ''))}_absolute")
  viz <- glue::glue("viz_hist_{tolower(str_replace_all(viz, ' ', '_'))}")
  
  viz_list_hist_absolute[[reg_val]] <- viz_emissions_target_history(df_kpi_regional_emissions %>%
                                                                  filter(entity == reg_name),
                                                                plot_title = reg_name,
                                                                per_capita = FALSE)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                 PER CAPITA                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Bar trend plots                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_list_hist_percapita <- list()
for (reg_val in seq_along(unique(df_kpi_regional_emissions$entity))) {
  
  reg_name <- unique(df_kpi_regional_emissions$entity)[reg_val]
  
  viz <- glue::glue("{tolower(str_replace_all(reg_name, ',', ''))}_percapita")
  viz <- glue::glue("viz_hist_{tolower(str_replace_all(viz, ' ', '_'))}")
  
  viz_list_hist_percapita[[reg_val]] <- viz_emissions_target_history(df_kpi_regional_emissions %>%
                                                                   filter(entity == reg_name),
                                                                 plot_title = reg_name,
                                                                 per_capita = TRUE)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              ggiraph section                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

width_graph <- 14
heigh_graph <- 7

viz_city_tot_absolute <-
  girafe(ggobj = viz_hist_total_absolute,
         pointsize = 32,
         width_svg = width_graph,
         height_svg = heigh_graph
  ) |>
  girafe_options(
    opts_toolbar(
      position = "topright",
      delay_mouseout = 2000),
    #     pngname = "city_tot"),
    #   # opts_tooltip(
    #   #   opacity = 0.8,
    #   #   use_fill = TRUE,
    #   #   use_stroke = FALSE,
    #   #   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
    opts_zoom(max = 1),
    #   # opts_hover(
    #   #   css = girafe_css(
    #   #     css = "fill:'#000000';opacity:0.4",
    #   #     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
    #   #   )),
    opts_sizing(rescale = FALSE, width = 1)
  )


viz_city_tot_percapita <-
  girafe(ggobj = viz_hist_total_percapita,
         pointsize = 32,
         width_svg = width_graph,
         height_svg = heigh_graph
  ) |>
  girafe_options(
    opts_toolbar(
      position = "topright",
      delay_mouseout = 2000),
    #     pngname = "city_tot"),
    #   # opts_tooltip(
    #   #   opacity = 0.8,
    #   #   use_fill = TRUE,
    #   #   use_stroke = FALSE,
    #   #   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
    opts_zoom(max = 1),
    #   # opts_hover(
    #   #   css = girafe_css(
    #   #     css = "fill:'#000000';opacity:0.4",
    #   #     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
    #   #   )),
    opts_sizing(rescale = FALSE, width = 1)
  )


for (i in seq_along(1:length(viz_list_hist_absolute))) {
  assign(paste0("viz_", "reg_", i, "absolute"),
         girafe(ggobj = viz_list_hist_absolute[[i]],
                pointsize = 32,
                width_svg = width_graph,
                height_svg = heigh_graph
         ) |>
           girafe_options(
               opts_toolbar(
                 position = "topright",
                 delay_mouseout = 2000),
             #     pngname = "city_tot"),
             #   # opts_tooltip(
             #   #   opacity = 0.8,
             #   #   use_fill = TRUE,
             #   #   use_stroke = FALSE,
             #   #   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
             opts_zoom(max = 1),
             #   # opts_hover(
             #   #   css = girafe_css(
             #   #     css = "fill:'#000000';opacity:0.4",
             #   #     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
             #   #   )),
             opts_sizing(rescale = FALSE, width = 1)
           )
  )

}



for (i in seq_along(1:length(viz_list_hist_percapita))) {
  assign(paste0("viz_", "reg_", i, "percapita"),
         girafe(ggobj = viz_list_hist_percapita[[i]],
                pointsize = 32,
                width_svg = width_graph,
                height_svg = heigh_graph
         ) |>
           girafe_options(
             opts_toolbar(
               position = "topright",
               delay_mouseout = 2000),
             #     pngname = "city_tot"),
             #   # opts_tooltip(
             #   #   opacity = 0.8,
             #   #   use_fill = TRUE,
             #   #   use_stroke = FALSE,
             #   #   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
             opts_zoom(max = 1),
             #   # opts_hover(
             #   #   css = girafe_css(
             #   #     css = "fill:'#000000';opacity:0.4",
             #   #     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
             #   #   )),
             opts_sizing(rescale = FALSE, width = 1)
           )
  )
  
}

