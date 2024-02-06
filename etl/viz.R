# install.packages("remotes")
#remotes::install_github("nsgrantham/ggbraid")

### graphs functions
source(here::here("etl/functions.R"))
#source(here::here("etl/function_viz.R"))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                   GHG total breakdown by region - GtCO₂e                 ----
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
           mutate(values = round(values, 2)) |> 
           ggplot(aes(x = entity, y = values, label = values)) +
           geom_col_interactive(aes(fill = variable, tooltip = values, data_id = entity),
                                color = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", "black", "darkgrey"),
                                alpha = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", 1, 0.8),
                                position = position_dodge(width = -1)) +
           geom_text(aes(group = variable),
                     position = position_dodge(width = -1), vjust = -0.5) +
           geom_hline(yintercept = 0, color = "grey") +
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
                width_svg = 12, height_svg = 5) |> 
           girafe_options(opts_toolbar(position = "topright", 
                                       delay_mouseout = 3000
           ),
           opts_tooltip(
             opacity = 0.8, 
             use_fill = TRUE,
             use_stroke = FALSE, 
             css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:black"),
           opts_zoom(max = 1),
           opts_hover(
             css = girafe_css(
               css = "fill:'#000000';opacity:0.4",
               text = "stroke:none;fill:'#000000';fill-opacity:1;"
             )),
           opts_sizing(rescale = FALSE, width = 1)
           )
         )
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  GHG total historical by region - GtCO₂e                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


for (i in c("absolute", "per capita")) {
  assign(paste0("viz_emissions_history_", str_remove(i, " ")),
         df |> 
           filter(emission_indicator == i) |> 
           select(entity, actual_2015, actual_2023) |> 
           pivot_longer(-entity, names_to = "variable", values_to = "values") |> 
           mutate(values = round(values, 2),
                  variable = factor(variable, level = c("actual_2023", "actual_2015"))) |> 
           ggplot(aes(x = entity, y = values, label = values)) +
           geom_col_interactive(aes(fill = variable, tooltip = values, data_id = entity),
                                color = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", "black", "darkgrey"),
                                alpha = ifelse(df_kpi_emissions$entity == "C40 wide\ntotal", 1, 0.8),
                                position = position_dodge(width = -1)) +
           geom_text(aes(group = variable),
                     position = position_dodge(width = -1), vjust = -0.5) +
           geom_hline(yintercept = 0, color = "grey") +
           scale_fill_manual(name = "",
                             limits = c("actual_2015", "actual_2023"),
                             values = c(c40_colors("green"), c40_colors("blue")),
                             labels = c("actual_2023" = glue::glue("2023"),
                                        "actual_2015" = glue::glue("2015")),
           ) +
           labs(x = "Year", y = "CO2e gigatones") +
           theme_minimal() +
           theme(
             legend.position = "top",
             panel.grid.major.x = element_line(colour = NA),
             panel.grid.minor.x = element_line(colour = NA))
  )
}


list_viz_history <- list(viz_emissions_history_absolute, 
                         viz_emissions_history_percapita)

viz_type <- c("absolute", "percapita")

for (i in seq_along(viz_type)) {
  assign(glue("viz_emissions_history_{viz_type[i]}_giraf"),
         
         girafe(ggobj = list_viz_history[[i]], 
                width_svg = 12, height_svg = 5) |> 
           girafe_options(opts_toolbar(position = "topright", 
                                       delay_mouseout = 3000
           ),
           opts_tooltip(
             opacity = 0.8, 
             use_fill = TRUE,
             use_stroke = FALSE, 
             css = "padding:5pt;font-family: Open Sans;font-size:1rem;color:black"),
           opts_zoom(max = 1),
           opts_hover(
             css = girafe_css(
               css = "fill:'#000000';opacity:0.4",
               text = "stroke:none;fill:'#000000';fill-opacity:1;"
             )),
           opts_sizing(rescale = FALSE, width = 1)
           )
  )
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Comparison between years: Wide emissions                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_line_total_absolute <- deviation_ribbon_plot2(df_kpi_wide_emissions,
                                                  per_capita = FALSE)

viz_bar_total_absolute <- target_deviation_barplot2(df_kpi_wide_emissions,
                                                    per_capita = FALSE)

viz_line_total_percapita <- deviation_ribbon_plot2(df_kpi_wide_emissions,
                                                   per_capita = TRUE)

viz_bar_total_percapita <- target_deviation_barplot2(df_kpi_wide_emissions,
                                                     per_capita = TRUE)

##Deviation from target
difference_percentage_wide<-df_kpi_wide_emissions%>%
  select("year","actual_emissions_tco2e","target_emissions_tco2e")%>%
  mutate(difference=(actual_emissions_tco2e/target_emissions_tco2e-1)*100)%>%
  mutate(fill=ifelse(difference<=0,0,1))%>%
  mutate(fill=ifelse(difference>10,2,fill))%>%
  mutate(fill=as.factor(fill))%>%
  drop_na()%>%
  ggplot()+
  geom_bar(aes(x=year, y=difference, fill=fill), stat = "identity")+
  geom_text(aes(x=year, y=difference, label = paste0(round(difference,1), "%"), vjust = ifelse(difference<0, 1.5, -0.5)), size=3.5)+
  scale_fill_manual(values=c("#4ED47C","#FEE474","#FF9080"), labels=c("On target", "Close to target" ,"Off target"), name="")+
  ylab("Deviation from the target (%)")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(2015, 2023,1))+
  scale_y_continuous(breaks = seq(-15,15,3), limits = c(-10,15), labels = function(x) paste0(x,"%"))+
  theme_bw()


#ggarrange(deviation_trend_wide, difference_percentage_wide, ncol=1, nrow = 2)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  ABSOLUTE                                ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Line trend plots                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_list_line_absolute <- list()
for (reg_val in seq_along(unique(df_kpi_regional_emissions$entity))) {
  
  reg_name <- unique(df_kpi_regional_emissions$entity)[reg_val]
  
  viz <- glue::glue("{tolower(str_replace_all(reg_name, ',', ''))}_absolute")
  viz <- glue::glue("viz_line_{tolower(str_replace_all(viz, ' ', '_'))}_absolute")
  
  viz_list_line_absolute[[reg_val]] <- deviation_ribbon_plot2(df_kpi_regional_emissions %>%
                                                                filter(entity == reg_name),
                                                              plot_title = reg_name,
                                                              per_capita = FALSE)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Bar trend plots                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_list_bar_absolute <- list()
for (reg_val in seq_along(unique(df_kpi_regional_emissions$entity))) {
  
  reg_name <- unique(df_kpi_regional_emissions$entity)[reg_val]
  
  viz <- glue::glue("{tolower(str_replace_all(reg_name, ',', ''))}_absolute")
  viz <- glue::glue("viz_bar_{tolower(str_replace_all(viz, ' ', '_'))}")
  
  viz_list_bar_absolute[[reg_val]] <- target_deviation_barplot2(df_kpi_regional_emissions %>%
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
##                              Line trend plots                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_list_line_percapita <- list()
for (reg_val in seq_along(unique(df_kpi_regional_emissions$entity))) {
  
  reg_name <- unique(df_kpi_regional_emissions$entity)[reg_val]
  
  viz <- glue::glue("{tolower(str_replace_all(reg_name, ',', ''))}_percapita")
  viz <- glue::glue("viz_line_{tolower(str_replace_all(viz, ' ', '_'))}")
  
  viz_list_line_percapita[[reg_val]] <- deviation_ribbon_plot2(df_kpi_regional_emissions %>%
                                                                 filter(entity == reg_name),
                                                               plot_title = reg_name,
                                                               per_capita = TRUE)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Bar trend plots                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
viz_list_bar_percapita <- list()
for (reg_val in seq_along(unique(df_kpi_regional_emissions$entity))) {
  
  reg_name <- unique(df_kpi_regional_emissions$entity)[reg_val]
  
  viz <- glue::glue("{tolower(str_replace_all(reg_name, ',', ''))}_percapita")
  viz <- glue::glue("viz_bar_{tolower(str_replace_all(viz, ' ', '_'))}")
  
  viz_list_bar_percapita[[reg_val]] <- target_deviation_barplot2(df_kpi_regional_emissions %>%
                                                                   filter(entity == reg_name),
                                                                 plot_title = reg_name,
                                                                 per_capita = TRUE)
}



viz_city_tot_absolute <- 
  girafe(ggobj = viz_line_total_absolute + viz_bar_total_absolute,
         #pointsize = 12,
         width_svg = 12,
         height_svg = 5
  ) |> 
  girafe_options(opts_toolbar(position = "topright", 
                              delay_mouseout = 3000, 
                              pngname = "city_tot"),
                 opts_tooltip(
                   opacity = 0.8, 
                   use_fill = TRUE,
                   use_stroke = FALSE, 
                   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
                 opts_zoom(max = 1),
                 opts_hover(
                   css = girafe_css(
                     css = "fill:'#000000';opacity:0.4",
                     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
                   )),
                 opts_sizing(rescale = FALSE, width = 1)
  )

viz_city_tot_percapita <- 
  girafe(ggobj = viz_line_total_percapita + viz_bar_total_percapita,
         #pointsize = 12,
         width_svg = 12,
         height_svg = 5
  ) |> 
  girafe_options(opts_toolbar(position = "topright", 
                              delay_mouseout = 3000, 
                              pngname = "city_tot"),
                 opts_tooltip(
                   opacity = 0.8, 
                   use_fill = TRUE,
                   use_stroke = FALSE, 
                   css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
                 opts_zoom(max = 1),
                 opts_hover(
                   css = girafe_css(
                     css = "fill:'#000000';opacity:0.4",
                     text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
                   )),
                 opts_sizing(rescale = FALSE, width = 1)
  )


for (i in seq_along(1:length(viz_list_line_absolute))) {
  assign(paste0("viz_", "reg_", i, "absolute"),
         girafe(ggobj = viz_list_line_absolute[[i]] + viz_list_bar_absolute[[i]],
                #pointsize = 12,
                width_svg = 12,
                height_svg = 5
         ) |> 
           girafe_options(opts_toolbar(position = "topright", 
                                       delay_mouseout = 3000, 
                                       pngname = glue("{viz_list_bar_absolute[[i]]$labels$title}")),
                          opts_tooltip(
                            opacity = 0.8, 
                            use_fill = TRUE,
                            use_stroke = FALSE, 
                            css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
                          opts_zoom(max = 1),
                          opts_hover(
                            css = girafe_css(
                              css = "fill:'#000000';opacity:0.4",
                              text = "stroke:none;fill:'#FFFFFF';fill-opacity:1;"
                            )),
                          opts_sizing(rescale = FALSE, width = 1)
           )
  )
  
}



for (i in seq_along(1:length(viz_list_line_percapita))) {
  assign(paste0("viz_", "reg_", i, "percapita"),
         girafe(ggobj = viz_list_line_percapita[[i]] + viz_list_bar_percapita[[i]],
                #pointsize = 12,
                width_svg = 12,
                height_svg = 5
         ) |> 
           girafe_options(opts_toolbar(position = "topright", 
                                       delay_mouseout = 3000, 
                                       pngname = glue("{viz_list_bar_percapita[[i]]$labels$title}")),
                          opts_tooltip(
                            opacity = 0.8, 
                            use_fill = TRUE,
                            use_stroke = FALSE, 
                            css = "padding:5pt;font-family: Montserrat;font-size:1rem;color:white"),
                          opts_zoom(max = 1),
                          opts_hover(
                            css = girafe_css(
                              css = "fill:'#000000';opacity:0.4",
                              text = "stroke:none;fill:'#FFFFFF'';fill-opacity:1;"
                            )),
                          opts_sizing(rescale = FALSE, width = 1)
           )
  )
  
}
