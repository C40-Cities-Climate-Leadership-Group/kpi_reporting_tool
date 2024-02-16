viz_emissions_target_history <- function(df, per_capita = FALSE, 
                                   plot_title="", 
                                   year_min = 2015, 
                                   year_max = 2023){
  
  if(per_capita == FALSE){
    
    selected_vars <- c("year", 
                       "actual_emissions_tco2e", 
                       "target_emissions_tco2e")
    
    year_var<-selected_vars[1]
    emissions_var<-selected_vars[2]
    target_var<-selected_vars[3]
    
    df_viz <- df |> 
      select(all_of(selected_vars)) |> 
      filter(year %in% year_min:year_max) |> 
      mutate(
        across(any_of(c(emissions_var, target_var)), ~ ./ 1000000000),
        across(where(is.numeric), as.double),
        across(where(is.numeric), round,3),
        diff =round((eval(parse(text=emissions_var))/eval(parse(text=target_var))-1)*100,2),
        diff_color=ifelse(diff <= 10, 0, 
                          ifelse(diff > 10 & diff <= 20, 1, 2)),
        #diff_color=ifelse(diff > 10, 2, diff_color),
        diff_color = as.factor(diff_color),
        label_position = ifelse(diff*1 >= 0, -0.5, 1.2)) |> 
      rename("actual" = "actual_emissions_tco2e",
             "target" = "target_emissions_tco2e")
    
  } else {
    selected_vars <- c("year", "actual_emissions_per_capita_tco2e", "target_emissions_per_capita_tco2e")
    
    year_var<-selected_vars[1]
    emissions_var<-selected_vars[2]
    target_var<-selected_vars[3]
    
    df_viz <- df |> 
      select(all_of(selected_vars)) |> 
      filter(year %in% year_min:year_max) |> 
      mutate(
        across(where(is.numeric), as.double),
        across(where(is.numeric), round,3),
        diff =round((eval(parse(text=emissions_var))/eval(parse(text=target_var))-1)*100,2),
        diff_color=ifelse(diff <= 10, 0, 
                          ifelse(diff > 10 & diff <= 20, 1, 2)),
        #diff_color=ifelse(diff > 10, 2, diff_color),
        diff_color = as.factor(diff_color),
        label_position = ifelse(diff*1 >= 0, -0.5, 1.2)) |> 
      rename("actual" = "actual_emissions_per_capita_tco2e",
             "target" = "target_emissions_per_capita_tco2e")
  }
  
  # df_viz <- df_viz |> 
  #   pivot_longer(cols = starts_with(c("actual", "target")), names_to = "variable", values_to = "value") |> 
    
  library(ggtext)
  df_viz |> 
    mutate(diff_label = glue::glue('<b style="font-size:12pt;">*{diff}%*</b><br>*from target*')) |> 
    ggplot(aes(x = year)) +
    geom_col(aes(y = actual, fill = diff_color),
             color = "darkgrey") +
    geom_point(aes(y = target),
               colour = "black", size = 1) +
    # geom_line(aes(y = target), 
    #           color = c40_colors("blue")) +
    geom_textline(aes(y = target),
                  label = "GHG targets", size = 4, hjust = 0.9, vjust = 1.5,
                  linewidth = 1, 
                  linecolor = c40_colors("blue"), 
                  linetype = 5, 
                  color = "black") +
    geom_text(aes(y = 2.14, label = round(actual, 2))) +
    geom_text(aes(y = target, label = round(target, 2)),
              vjust = -1) +
    geom_text(aes(y = min(actual) - (min(actual) * 0.1), x = min(year), label = "GHG emission"), 
              angle = 90, vjust = 5) +
    geom_richtext(aes(y = max(df_viz$actual) + sd(df_viz$actual) * 2, 
                      label = diff_label,
                      fill = diff_color),
                  size = 3.5, 
                  lineheight = .8,
                  show.legend = FALSE) +
    geom_hline(yintercept = 2.118) +
    coord_cartesian(ylim = c(min(df_viz$actual) - sd(df_viz$actual) * 5,
                             max(df_viz$actual) + sd(df_viz$actual) * 5)) +
    scale_fill_manual(values = c("0" = "#4ED47C", 
                                 "1" = "#FEE474", 
                                 "2" = "#FF9080"), 
                      labels = c("0" = "On target", 
                                 "1" = "Close to target",
                                 "2" = "Off target"),
                      name = "") +
    scale_color_manual(values = c("0" = "#4ED47C", 
                                 "1" = "#FEE474", 
                                 "2" = "#FF9080")) +
    scale_x_continuous(breaks = c(seq(min(df_viz$year), max(df_viz$year)))) +
    labs(title = "**Actual and Target emissions per year (absolute values)**",
       subtitle =glue::glue("Actual deviation from target. <span style='color:{c40_colors('green')}'>**On target**</span>,
                            <span style='color:{c40_colors('yellow')}'>**Close to target**</span>,
                            <span style='color:{c40_colors('red')}'>**Off target**</span>"),
       x = "",
       y = "") +
    theme_minimal() + 
    theme(
      plot.title = element_markdown(),
      plot.subtitle = element_markdown(),
      legend.position = "bottom",
          legend.text = element_text(size = 9),
          title = element_text(size = 11),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          panel.grid.minor.x = element_blank())
}

fnt_viz_ghg_target_dev(df = df_kpi_wide_emissions, per_capita = TRUE)
