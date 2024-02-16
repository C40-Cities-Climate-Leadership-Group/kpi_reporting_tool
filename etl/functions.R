library(ggbraid)
library(ggpubr)
library(tidyverse)
library(gt)
library(gtExtras)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        #Function to output ribbon plots with emissions trajectories:     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
deviation_ribbon_plot <- function(df, per_capita=FALSE, plot_title="", xmin=2020){
  
  ##First, select the columns to plot depending on per_capita or absolute
  
  if(per_capita==FALSE){
    cols_select<-c("year","actual_emissions_tco2e","target_emissions_tco2e")
    year_var<-cols_select[1]
    emissions_var<-cols_select[2]
    target_var<-cols_select[3]
    
    ylabel<-"Emissions (CO2 Gigatonnes)"
    
    df<-df%>%
      select(cols_select)%>%
      mutate(across(c({{emissions_var}}, {{target_var}}), ~round(./(10^9), 2)))
  }else{
    cols_select<-c("year","actual_emissions_per_capita_tco2e","target_emissions_per_capita_tco2e")
    year_var<-cols_select[1]
    emissions_var<-cols_select[2]
    target_var<-cols_select[3]
    
    ylabel<-"Emissions Per Capita (CO2 Tonnes)"
    
    df<-df%>%
      select(cols_select)%>%
      mutate(across(c({{emissions_var}}, {{target_var}}), ~round(., 2)))
  }
  
  ##Create ggplot object with ribbons and lines
  
  #Df
  
  #The next section fixes the data to shade correctly the ribbons and braids
  df<-df%>%
    mutate(difference=(eval(parse(text=emissions_var))/eval(parse(text=target_var))-1)*100) #Deviation from target will be reflected in the colors.
  
  #We are only interested in the red and yellow colors. Everything below the target is on target.
  #ggbraid will exclude the red fractions. Those will be covered by geom_ribbon
  
  years_red<-as.vector(df%>%subset(difference>10)%>%select("year"))$year
  
  if(!any(is.na(years_red)) & !identical(years_red, integer(0))){
    years_pre_red<-years_red-1 #Red shade starting the year before
    years_shade_red<-c(years_pre_red, years_red)
    omit_red<-FALSE
  }else{
    years_shade_red<-NA
    omit_red<-TRUE
  }
  
  df<-df%>%
    mutate(fill=eval(parse(text=emissions_var))<eval(parse(text=target_var)))%>%
    drop_na()%>%
    mutate(fill=ifelse(fill==T, "on", "close"))
  
  if(omit_red==F){
    df<-df%>%
      mutate(fill1=ifelse(eval(parse(text=year_var))%in%years_shade_red,eval(parse(text=emissions_var)),NA)) #Fill1 is the red ribbon for geom_ribbon
  }else{
    df$fill1<-NA
  }
  
  #Df for geom_braid and yelow ribbon. This was the only way to avoid gaps.
  
  df_yellow<-df%>%
    mutate(across(c({{emissions_var}}, {{target_var}}, fill), ~ifelse(year%in%years_red, NA, .)))
  
  #omit_yellow (only red graphs)
  omit_yellow <- sum(is.na(df_yellow[emissions_var]))==dim(df_yellow)[1]
  
  #Now identify if there are gaps in yellow
  years_yelllow<-df_yellow%>%drop_na(any_of(c(target_var, emissions_var)))
  years_yelllow<-years_yelllow$year
  
  possible_gaps<-sort(df$year)[2:length(df$year)-1]
  
  gap_years<-possible_gaps[!possible_gaps%in%years_yelllow]
  
  #Have to check gap_years so that is not the middle of a red interval
  real_gaps<-c()
  for(i in gap_years){
    t<-i+1
    if(!t%in%years_shade_red){
      real_gaps<-c(real_gaps,i)
    }
  }
  
  if(length(real_gaps)>0){
    years_ribbon_yellow<-c(real_gaps, real_gaps+1)
    df<-df%>%
      mutate(fill2=ifelse(eval(parse(text=year_var))%in%years_ribbon_yellow, eval(parse(text=emissions_var)),NA))
  }
  
  #Text on plots  
  df<-df%>%
    mutate(relevant_labels_actual=ifelse(eval(parse(text=emissions_var))%in%c(max(eval(parse(text=emissions_var))), min(eval(parse(text=emissions_var))), eval(parse(text=emissions_var))[1], eval(parse(text=emissions_var))[length(eval(parse(text=emissions_var)))]), 
                                         eval(parse(text=emissions_var)),NA))%>%
    mutate(relevant_labels_target=ifelse(is.na(relevant_labels_actual), NA, eval(parse(text=target_var))))
  
  df<-df%>%
    mutate(fill1_label=ifelse(!is.na(fill1), "off", fill1))
  
  #Fix limits on y axis
  min_lim<-ifelse(min(df[emissions_var])<min(df[target_var]), min(df[emissions_var]), min(df[target_var]))
  min_lim<-min_lim*0.5
  
  max_lim<-ifelse(max(df[emissions_var])<max(df[target_var]), max(df[emissions_var]), max(df[target_var]))
  max_lim<-max_lim+max_lim*0.5
  
  #Plot
  plt<-ggplot(data=df)+
    geom_line(aes(x=eval(parse(text=year_var)), y=eval(parse(text=emissions_var)), color=emissions_var))+
    geom_line(aes(x=eval(parse(text=year_var)), y=eval(parse(text=target_var)), color=target_var))+
    geom_point(aes(x=eval(parse(text=year_var)), y=eval(parse(text=emissions_var))))+
    geom_point(aes(x=eval(parse(text=year_var)), y=eval(parse(text=target_var))), color="#23BCED")+
    geom_text(aes(x=eval(parse(text=year_var)), y=relevant_labels_actual, label=relevant_labels_actual), vjust = ifelse(df[emissions_var]<df[target_var], 1.5, -0.5), size=3.5)+
    geom_text(aes(x=eval(parse(text=year_var)), y=relevant_labels_target, label=relevant_labels_target), vjust = ifelse(df[emissions_var]>df[target_var], 1.5, -0.5), size=3.5, color="#23BCED")+ #Exclude red years
    #geom_braid(data = yellow_ribbons, aes(x=eval(parse(text=year_var)), ymax = eval(parse(text=emissions_var)), ymin = eval(parse(text=target_var)), fill="FEE474"), alpha=0.5)+
    scale_color_manual(name="", breaks = c(emissions_var, target_var),values=c("black","#23BCED"), labels=c("Actual Emissions", "Target Emissions"))+
    ylab(ylabel)+
    xlab("Year")+
    scale_y_continuous(limits = c(min_lim, max_lim))+
    scale_x_continuous(breaks = seq(xmin, max(df$year),1), limits = c(xmin, NA))+
    ggtitle(plot_title)+
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  ##Plot conditional on the red/yellow shades
  
  if (omit_yellow==FALSE){
    
    plt <- plt + geom_braid(data = df_yellow,
                            aes(eval(parse(text = year_var)), 
                                ymin = eval(parse(text=emissions_var)), 
                                ymax = eval(parse(text=target_var)), fill = fill), 
                            alpha=0.5, na.rm = F)
  }
  
  
  if(omit_red==FALSE){
    plt<-plt+geom_ribbon(aes(x=eval(parse(text=year_var)), ymax = fill1, ymin = eval(parse(text=target_var)), fill=fill1_label), alpha=0.5)+
      scale_fill_manual(values=c("close"="#FEE474", "off"="#FF9080", "on"="#4ED47C"), labels=c("close"="Close to target", "off"="Off target", "on"="On target"), name="")
    
  }else{
    plt<-plt+scale_fill_manual(values=c("close"="#FEE474","on"="#4ED47C"), labels=c("close"="Close to target", "on"="On target"), name="")
  }
  
  #Add extra yellow ribbons if necessary
  if(length(real_gaps>0)){
    plt<-plt+geom_ribbon(aes(x=eval(parse(text=year_var)), ymax = fill2, ymin = eval(parse(text=target_var))), fill="#FEE474", alpha=0.5)
  }
  
  return(plt)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##          #Function to output plots with deviations from target:          ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
target_deviation_barplot<-function(df, per_capita = FALSE,  xmin = 2020){
  
  if(per_capita==FALSE){
    cols_select<-c("year","actual_emissions_tco2e","target_emissions_tco2e")
    year_var<-cols_select[1]
    emissions_var<-cols_select[2]
    target_var<-cols_select[3]
    
  } else {
    cols_select<-c("year","actual_emissions_per_capita_tco2e","target_emissions_per_capita_tco2e")
    year_var<-cols_select[1]
    emissions_var<-cols_select[2]
    target_var<-cols_select[3]
  }
  
  #DF
  df<-df%>%
    mutate(difference=(eval(parse(text=emissions_var))/eval(parse(text=target_var))-1)*100)%>%
    mutate(fill=ifelse(difference<=0,0,1))%>%
    mutate(fill=ifelse(difference>10,2,fill))%>%
    mutate(fill=as.factor(fill))%>%
    drop_na()
  
  #Fix limits on y axis
  min_lim<-round(min(df$difference),0)
  min_lim<-ifelse(min(df$difference)<0, min_lim-5, 0)
  
  max_lim<-round(max(df$difference), 0)
  max_lim<-max_lim+3
  
  #Plt
  plt<-ggplot(data=df)+
    geom_bar(aes(x=year, y=difference, fill=fill), stat = "identity")+
    geom_text(aes(x=year, y=difference, label = paste0(round(difference,1), "%"), vjust = ifelse(difference<0, 1.5, -0.5)), size=3.5)+
    scale_fill_manual(breaks = c("0", "1", "2"), values=c("#4ED47C","#FEE474","#FF9080"), labels=c("On target", "Close to target" ,"Off target"), name="")+
    ylab("Deviation from the target (%)")+
    xlab("Year")+
    scale_x_continuous(breaks = seq(xmin, max(df$year),1), limits = c(xmin-0.5, NA))+
    scale_y_continuous(breaks = seq(min_lim-3,max_lim+3,3), limits = c(min_lim,max_lim), labels = function(x) paste0(x,"%"))+
    geom_hline(yintercept = 0, color="grey") +
    ggtitle(plot_title)+
    theme_bw()
  
  return(plt)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Creating tables                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ghg_table <- function(df, indicator){
  
  # assertthat::assert_that(indicator %in% c("absolute", "per capita"),
  #                         msg = "Valid values: 'absolute' and 'per capita'")
  
  ### Prepare spark graphs
  df_plot <- df_kpi_regional_emissions |>
    bind_rows(df_kpi_wide_emissions) |> 
    filter(!is.na(actual_emissions_tco2e)) |> 
    select(entity, year, actual_emissions_tco2e, actual_emissions_per_capita_tco2e) |> 
    mutate(across(.cols = where(is.numeric), ~ as.double(.)),
           actual_emissions_tco2e = round(actual_emissions_tco2e / 1000000000, 3)) |> 
    pivot_longer(cols = starts_with("actual"), 
                 names_to = "emission_indicator",
                 values_to = "ghg_emissions") |> 
    mutate(emission_indicator = case_when(emission_indicator == "actual_emissions_tco2e" ~ "absolute",
                                          emission_indicator == "actual_emissions_per_capita_tco2e" ~ "per capita"))
  
  df_plot <- df_plot |> 
    filter(emission_indicator == indicator) |> 
    group_by(entity) |> 
    summarize(data = list(ghg_emissions), .groups = "drop")
  
  
  # TABLE
  table <- df |> 
    dplyr::filter(emission_indicator == indicator) |> 
    select(entity, 
           #baseline_2020, 
           actual_2020:actual_2023, 
           target_2020:target_2024, target_2030, target_2050,
           on_off_track_target23) |> 
    left_join(df_plot, by = c("entity")) |> 
    gt(id = "one") |> 
    tab_header(md("**GHG Emissions Report - 2023**")) |> 
    tab_source_note(source_note = md(
      "Data is "
    )) |> 
    tab_spanner(label = md(ifelse(indicator == "absolute", 
                                  "**Emissions (gtCO2)**",
                                  "**Emissions (tCO2)**")),
                columns = c(
                  #"baseline_2020", 
                  "actual_2021", "actual_2022", "actual_2023")) |>  
    tab_spanner(label = md(ifelse(indicator == "absolute",
                                  "**Targets (gtCO2)**",
                                  "**Targets (tCO2)**")),
                columns = starts_with("target")) |>  
    tab_spanner(label = md("**On/Off track**"), columns = starts_with("on_off")) |> 
    cols_label(
      entity = "**Entity**",
      #baseline_2020 = "**2020**",
      #actual_2023 = "**Mid year 2023**",
      actual_2020 = "**2020**",
      actual_2021 = "**2021**",
      actual_2022 = "**2022**",
      actual_2023 = "**2023**",
      target_2020 = "**2020**",
      target_2021 = "**2021**",
      target_2022 = "**2022**",
      target_2023 = "**2023**",
      target_2024 = "**2024**",
      target_2030 = "**2030**",
      target_2050 = "**2050**",
      #on_off_track_target22 = "**Against 2022 emissions target**",
      on_off_track_target23 = "**Against 2023 emissions target**",
      data = "**GHG emissions evolution 2020-2023**",
      .fn = md
    ) |> 
    cols_align( align = c("center"), columns = everything()) |>  
    cols_align( align = c("left"), columns = entity) |> 
    fmt_number(columns = c(
      #baseline_2020, 
      actual_2020:actual_2023, starts_with("target")), decimals = 2) |> 
    fmt_percent(columns = starts_with("on_off"), decimals = 1) |> 
    gt_plt_sparkline(data, same_limit = FALSE, fig_dim = c(5,40),
                     palette = c("lightgrey", "black", "#03c245", "#ff614a", "black")) |> 

    tab_style(
      style = cell_fill(color = "#C9F2D7"),
      locations = cells_body(
        columns = on_off_track_target23,
        rows = on_off_track_target23 >= -0.1
      )
    ) |> 
    tab_style(
      style = cell_fill(color = "#FEF4C7"),
      locations = cells_body(
        columns = on_off_track_target23,
        rows = on_off_track_target23 < -0.1 & on_off_track_target23 >= -0.2
      )
    ) |>
    tab_style(
      style = cell_fill(color = "#FFD2CC"),
      locations = cells_body(
        columns = on_off_track_target23,
        rows = on_off_track_target23 < -0.2
      )
    ) |>  
    tab_style(
      style = list(
        cell_fill(color = "#e5e5e5")
      ),
      locations = cells_body(
        columns = c(1:11),
        rows = entity == "C40 wide total"
      )
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = "#e5e5e5")
      ),
      locations = cells_body(
        columns = c(actual_2023, target_2023),
        rows = entity == "C40 wide total"
      )
    ) |> 
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "black",
          weight = px(2)),
        cell_text(
          weight = "bold"
        )
      ),
      locations = cells_body(
        columns = everything(),
        rows = entity == "C40 wide total"
      )
    ) |> 
    tab_style(
      style = list(
        cell_text(
          weight = "bold"
        )
      ),
      locations = cells_body(
        columns = c(actual_2023, target_2023),
        rows = everything()
      )
    ) |> 
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "-"
    ) |> 
    opt_css(
      css = "
    #one .gt_table {
      font-family: Fira mono;
      font-size: 13px;
    }
    "
    ) 
  
  
  return(table)
  
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                        Sending files to Google Drive                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
copy_files <- function(folder_input = NA, file = NA, file_format, folder_output = NA,
                       remove_prev_file = FALSE){
  
  googledrive_path <- "~/Google Drive/Shared drives/BPMI/Business Planning and Reporting/Monitoring & Evaluation/06_Funder & C40 reporting/02_Mid year reporting/Mid year reporting 2023/GHG update/Outputs/"
  
  if(is.na(folder_input) | (!is.na(folder_input) & folder_input == "kpi")){
    folder_path <- paste0(here::here(), "/kpi/")
  } else if(!is.na(folder_input) & folder_input != "kpi"){
    folder_path <- paste0(here::here(), "/kpi", "/", folder_input, "/")
  }
  
  my_files <- list.files(folder_path)
  
  if(!is.na(file)){
    
    my_file <- str_remove(
      string = my_files[str_detect(
        string = my_files, 
        pattern = paste0(file, file_format))], 
      pattern = file_format)
    
  } else {
    
    my_file <- str_remove(
      string = my_files[str_detect(
        string = my_files, 
        pattern = file_format)], 
      pattern = file_format)
  }
  
  if(is.na(folder_output)){
    output_path <- googledrive_path
  }
  
  if(!is.na(folder_output)){
    output_path <- paste0(googledrive_path, folder_output, "/")
  }
  
  # Remove files if wanted
  if(remove_prev_file){
    files_to_remove <- list.files(output_path)
    file.remove(paste0(output_path, files_to_remove))
  }
  
  file.copy(from = paste0(folder_path, my_file, file_format),
            to = paste0(output_path,  my_file, "_",
                        lubridate::today(), file_format),
            overwrite = TRUE,
            copy.date = TRUE)
}





deviation_ribbon_plot2 <- function(df, per_capita=FALSE, plot_title="", 
                                   year_min = 2020, year_max = 2023){
  
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
        across(where(is.numeric), round,3))
    
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
        across(where(is.numeric), round,3))
  }
  
  df_viz <- df_viz |> 
    pivot_longer(cols = starts_with(c("actual", "target")), names_to = "variable", values_to = "value")
  
  theme_set(
    theme_bw() + 
      theme(legend.position = "bottom",
            legend.text = element_text(size = 9),
            title = element_text(size = 11),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9),
            panel.grid.minor.x = element_blank()))
  
  df_viz |> 
    ggplot(aes(x = year, y = value, color = variable)) +
    ylim(min(df_viz$value) - sd(df_viz$value) * 5,
         max(df_viz$value) + sd(df_viz$value) * 5) +
    xlim(year_min, year_max) +
    geom_line(alpha = 0.5) +
    geom_point_interactive(aes(x = year, y = value, color = variable,
                               tooltip = value, data_id = year),
                           size = 1, hover_nearest = TRUE) +
    scale_color_manual(values = c("black",c40_colors("blue")),
                       labels = c("Actual emissions",
                                  "Target emissions"),
                       name = "") +
    labs(title = "Actual and Target emissions per year (absolute values)",
         x = "Year",
         y = "Emissions (Gigatons)")
}



# target_deviation_barplot2 <- function(df, per_capita=FALSE, plot_title="",
#                                       year_min = 2020, year_max = 2023){
#   
#   if(per_capita == FALSE){
#     
#     selected_vars <- c("year", 
#                        "actual_emissions_tco2e", 
#                        "target_emissions_tco2e")
#     
#     year_var<-selected_vars[1]
#     emissions_var<-selected_vars[2]
#     target_var<-selected_vars[3]
#     
#     df_viz <- df |> 
#       select(all_of(selected_vars)) |> 
#       filter(year %in% year_min:year_max) |> 
#       mutate(
#         across(any_of(c(emissions_var, target_var)), ~ ./ 1000000000),
#         across(where(is.numeric), as.double),
#         across(where(is.numeric), round,3))
#     
#   } else {
#     selected_vars <- c("year", 
#                        "actual_emissions_per_capita_tco2e",
#                        "target_emissions_per_capita_tco2e")
#     
#     year_var<-selected_vars[1]
#     emissions_var<-selected_vars[2]
#     target_var<-selected_vars[3]
#     
#     df_viz <- df |> 
#       select(all_of(selected_vars)) |> 
#       filter(year %in% year_min:year_max) |> 
#       mutate(
#         across(where(is.numeric), as.double),
#         across(where(is.numeric), round,3))
#   }
#   
#   df_viz <- df_viz |>
#     mutate(diff =round((eval(parse(text=emissions_var))/eval(parse(text=target_var))-1)*100,2),
#            diff_color=ifelse(diff <= 10, 0, 
#                              ifelse(diff > 10 & diff <= 20, 1, 2)),
#            #diff_color=ifelse(diff > 10, 2, diff_color),
#            diff_color = as.factor(diff_color),
#            label_position = ifelse(diff*1 >= 0, -0.5, 1.2))
#   
#   y_min <- ifelse(min(df_viz$diff) < 0, min(df_viz$diff) - sd(df_viz$diff), 0)
#   y_max <- max(df_viz$diff) + sd(df_viz$diff)
#   #y_max <- 0
# 
#   gg2 <- df_viz |> 
#     ggplot() +
#      # ylim(ifelse(min(df_viz$diff) < 0, min(df_viz$diff) - sd(df_viz$diff), 0),
#      #      max(df_viz$diff) + sd(df_viz$diff)) +
#     ylim(y_min, ifelse(y_max <= 0, 2, y_max)) +
#     xlim(year_min - 0.5, year_max + 0.5) +
#     geom_col_interactive(aes(x = year, y = diff, fill = diff_color,
#                              tooltip = paste(round(diff, 1), "%"), data_id = year)) +
#     geom_hline(yintercept = 0, color = "grey") +
#     geom_text(aes(x = year, y = diff, label = paste0(round(diff, 1), "%"), vjust = label_position)) +
#     scale_fill_manual(values = c("0" = "#4ED47C", 
#                                  "1" = "#FEE474", 
#                                  "2" = "#FF9080"), 
#                       labels = c("0" = "On target", 
#                                  "1" = "Close to target",
#                                   "2" = "Off target"),
#                       name = "") +
#     labs(title = "Deviation from the target (%)",
#          y = "Deviation (%)",
#          x = "Year")
#   
#   gg2
# }
# 
# 
# modeshare_box <- function(df, region){
#   
#   df_sparkline <- 
#     df_modeshare |> 
#       filter(c40_region == region) |> 
#     select(year, trips_perc)
#   
#   sparkline <- plot_ly(df_sparkline) %>%
#     add_lines(
#       x = ~year, y = ~trips_perc,
#       color = I("white"), span = I(1),
#       fill = 'tozeroy', alpha = 0.2
#     ) %>%
#     layout(
#       xaxis = list(visible = F, showgrid = F, title = ""),
#       yaxis = list(visible = F, showgrid = F, title = ""),
#       hovermode = "x",
#       margin = list(t = 0, r = 0, l = 0, b = 0),
#       font = list(color = "white"),
#       paper_bgcolor = "transparent",
#       plot_bgcolor = "transparent"
#     ) %>%
#     config(displayModeBar = F) %>%
#     htmlwidgets::onRender(
#       "function(el) {
#       var ro = new ResizeObserver(function() {
#          var visible = el.offsetHeight > 200;
#          Plotly.relayout(el, {'xaxis.visible': visible});
#       });
#       ro.observe(el);
#     }"
#     )
#   
#   max_year <- max(df_transport$year)
#   min_year <- min(df_transport$year)
#   max_year_value <- df_sparkline |> filter(year == max_year) |> pull(trips_perc)
#   min_year_value <- df_sparkline |> filter(year == min_year) |> pull(trips_perc)
#   trips_average <- mean(df_sparkline$trips_perc)
#   peaked_trips <- df_sparkline |> slice_max(trips_perc)
#   
#   
#   value_box(
#     title = region,
#     value = glue("{max_year}: {max_year_value}%"),
#     p(glue("Started at {min_year_value}% ({min_year})")),
#     p(glue("Averaged {trips_average}% over {min_year}-{max_year}")),
#     p(glue("Peaked {peaked_trips$trips_perc}% in {peaked_trips$year}")),
#     showcase = sparkline,
#     full_screen = TRUE,
#     theme = "success"
#   )
# }

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
  
  
  df_viz |> 
    mutate(diff_label = glue::glue('<b style="font-size:12pt;">*{round(diff,1)}%*</b><br>*from<br>target*')) |> 
    ggplot(aes(x = year)) +
    # geom_col_interactive(aes(y = actual, fill = diff_color,
    #                          tooltip = actual, data_id = year),
    #          color = "darkgrey") +
    geom_col(aes(y = actual, fill = diff_color), color = "darkgrey") +
    geom_textline(aes(y = target),
                  label = "GHG targets", size = 4, hjust = 0.9, vjust = 1.3,
                  linewidth = 1, 
                  linecolor = c40_colors("blue"), 
                  linetype = 5, 
                  color = "black") +
    # geom_point_interactive(aes(y = target,tooltip = actual, data_id = year),
    #                        colour = "black", size = 1) +
    geom_point(aes(y = target), colour = "black", size = 1) +
    geom_text(aes(y = min(df_viz$actual) - sd(df_viz$actual) * 5,
                        label = round(actual, 2))) +
    geom_text(aes(y = target, label = round(target, 2)),
              vjust = -1) +
    geom_text(aes(y = min(df_viz$actual) - sd(df_viz$actual) * 2.5, 
                  x = min(year), angle = 90, vjust = 5.4,
                  label = "GHG emission")) +
    geom_richtext(aes(y = max(df_viz$actual) + sd(df_viz$actual) * 3, 
                      label = diff_label,
                      fill = diff_color),
                  size = 2.5, 
                  lineheight = .8,
                  show.legend = FALSE) +
    coord_cartesian(ylim = c(min(df_viz$actual) - sd(df_viz$actual) * 5,
                             max(df_viz$actual) + sd(df_viz$actual) * 5)) +
    scale_fill_manual(values = c("0" = "#4ED47C", 
                                 "1" = "#FEE474", 
                                 "2" = "#FF9080"), 
                      labels = c("0" = "On target", 
                                 "1" = "Close to target",
                                 "2" = "Off target"),
                      name = "") +
    scale_x_continuous(breaks = c(seq(min(df_viz$year), max(df_viz$year)))) +
    labs(x = "", y = "") +
    theme_minimal() + 
    theme(
      #plot.title = element_markdown(),
      #plot.subtitle = element_markdown(),
      legend.position = "none",
          legend.text = element_text(size = 9),
          title = element_text(size = 11),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 9),
          panel.grid.minor.x = element_blank())
}



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

# df1 <- create_df(df = df_modeshare, "trips_perc", 2022)
# df1 <- create_df(df = df_modeshare, "km_perc", 2022)


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


modeshare_table <- function(df, c40_city_wide = TRUE) {
  
  if(c40_city_wide){
    df_filtered <- df |> 
      filter(c40_region == "C40 city wide")
  } else {
    df_filtered <- df |> 
      filter(c40_region != "C40 city wide")
  }
  
  df_filtered |> 
    relocate(c40_region, starts_with("trips"), starts_with("km")) |> 
  reactable(
    theme = clean(centered = TRUE),
    defaultColDef = colDef(align = 'center'),
    columns = list(
      trips_sparkline = colDef(
        name = "Mode share (%) - Serie 2018-2022",
        cell = react_sparkbar(df_filtered,
                              height = 70,
                              min_value = 0,
                              max_value = 100,
                              labels = c("all"),
                              label_size = "0.8em",
                              tooltip = FALSE,
                              highlight_bars =  highlight_bars(min = c40_colors("red"), max = c40_colors("green")))
      ),
      km_sparkline = colDef(
        name = "Mode share (%) - Serie 2018-2022",
        cell = react_sparkbar(df_filtered,
                              height = 70,
                              min_value = 0,
                              max_value = 100,
                              labels = c("all"),
                              label_size = "0.8em",
                              tooltip = FALSE,
                              highlight_bars =  highlight_bars(min = c40_colors("red"), max = c40_colors("green")))
      ),
      c40_region = colDef(name = "Region", 
                          show = TRUE, filterable = TRUE, align = "left"),
      trips_n = colDef(name = "Amount",
                       align = "center",
                       cell = bubble_grid(
                         data = df_filtered,
                         number_fmt = scales::comma,
                         min_value = 0,
                         max_value = max(df_filtered$trips_n),
                         box_shadow = TRUE,
                         colors = c(c40_colors("blue")))),
      km_n = colDef(name = "Amount",
                    align = "center",
                    cell = bubble_grid(
                      data = df_filtered,
                      number_fmt = scales::comma,
                      min_value = 0,
                      max_value = max(df_filtered$km_n),
                      box_shadow = TRUE,
                      colors = c(c40_colors("violet")))),
      trips_perc = colDef(
        name = "Mode share",
        cell = gauge_chart(
          data = df_filtered,
          size = 2,
          min_value = 0,
          max_value = 1,
          fill_color_ref = "col_color_trips",
          #fill_color = c('#D7191C','#FFFFBF','#1A9641'),
          bias = 1,
          number_fmt = scales::percent,
          bold_text = TRUE,
          text_size = 14,
          show_min_max = TRUE, 
          animation = "margin-right 4s"
        )
      ),
      km_perc = colDef(
        name = "Mode share",
        cell = gauge_chart(
          data = df_filtered,
          size = 2,
          min_value = 0,
          max_value = 1,
          fill_color_ref = "col_color_km",
          #fill_color = c('#D7191C','#FFFFBF','#1A9641'),
          bias = 1,
          number_fmt = scales::percent,
          bold_text = TRUE,
          text_size = 14,
          show_min_max = TRUE, 
          animation = "margin-right 4s"
        )
      ),
      col_color_trips = colDef(show = FALSE),
      col_color_km = colDef(show = FALSE)
    ),
    columnGroups = list(
      colGroup(name = "Trips (year 2022)",
               columns = c("trips_n", "trips_perc")),
      colGroup(name = "Km travelled  (year 2022)",
               columns = c("km_n", "km_perc"))
    ),
    rowStyle = group_border_sort("Division"),
    
    # style = list(height = "450px",
    #              width = "1125px"),
    fullWidth = TRUE, 
    borderless = FALSE,
    striped = FALSE,
    highlight = TRUE,
    outlined = TRUE,
    wrap = TRUE,
    resizable = FALSE,
    filterable = FALSE,
    sortable = TRUE,
    showSortable = TRUE
    ) %>% 
    # add_title(title = "Mode share: Mass transit, cycling and walking",
    #           margin = reactablefmtr::margin(t=10,r=0,b=35,l=0)) |>
    add_title(
      title = ifelse(c40_city_wide == TRUE, "C40 city wide", "C40 Regions")) |> 
      #title = reactablefmtr::html(glue::glue("<img src={img_src} width='60' height='55'> Mode share: Mass transit, cycling and walking"))) |> 
    add_source(
      source = 'Source: Google Environmental Insights Explorer (EIE)',
      font_color = '#666666',
      font_size = 12
    )
}
