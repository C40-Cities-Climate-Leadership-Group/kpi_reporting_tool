library(htmltools)
library(reactable)
library(reactablefmtr)
library(glue)
library(plotly)


modeshare_box(df_transport, "Latin America")




df_modeshare %>%  
  filter(year == 2022) %>%  
  select(-mode_aggregated, -year) %>%  
  mutate(across(ends_with("perc"), ~ ./100)) %>%  
  relocate(c40_region, trips_n, trips_perc, km_n, km_perc) %>% 
  reactable(
    fullWidth = FALSE,
    #defaultSorted = 'mpg',
    #defaultSortOrder = 'desc',
    theme = clean(centered = TRUE),
    defaultColDef = colDef(align = 'left'),
    columns = list(
      #model = colDef(minWidth = 120),
      c40_region = colDef(name = "Region", show = TRUE),
      trips_n = colDef(name = "Number of trips"),
      km_n = colDef(name = "Number of km travelled"),
      trips_perc = colDef(
        name = "Trips (%)",
        cell = gauge_chart(
          min_value = 0,
          max_value = 1,
          data = .,
          fill_color = c('#D7191C','#FDAE61','#FFFFBF','#A6D96A','#1A9641'),
          number_fmt = scales::percent,
          bold_text = TRUE,
          text_size = 14,
          show_min_max = TRUE
        )
      ),
      km_perc = colDef(
        name = "Km travelled (%)",
        cell = gauge_chart(
          min_value = 0,
          max_value = 1,
          data = .,
          fill_color = c('#D7191C','#FDAE61','#FFFFBF','#A6D96A','#1A9641'),
          number_fmt = scales::percent,
          bold_text = TRUE,
          text_size = 14,
          show_min_max = TRUE
        )
      )
    ),
    style = list(height = "450px",
                 width = "1125px"),
    borderless = FALSE,
    striped = FALSE,
    #bordered = TRUE,
    highlight = TRUE,
    outlined = TRUE,
    wrap = TRUE,
    resizable = TRUE,
    #fullWidth = FALSE,
    #height = 1000,
    #groupBy = "city",
    # Filter
    filterable = FALSE,
    # Sort
    sortable = TRUE,
    showSortable = TRUE,
    #defaultSorted = c("country", "city"),
    # Page configuration
    pagination = FALSE
    #showPageSizeOptions = FALSE,
    #pageSizeOptions = c(4, 8, 12),
    #defaultPageSize = 20,
    # theme = reactableTheme(
    #   headerStyle = list(
    #     "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
    #     "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
    #     borderColor = "#555")
    ) |> 
  add_title("Mode share: Mass transit, cycling and walking")
