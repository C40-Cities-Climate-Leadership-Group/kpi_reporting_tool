
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                NORTH AMERICA                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
na_size = 4
west_cities <- c("Vancouver", "Seattle", "Portland", "San Francisco", "Los Angeles",
                 "Phoenix")
west_labels_repel <- geom_text_repel(data = na_data |> 
                                       filter(city %in% c(west_cities)), 
                                     aes(x = longitude, y = latitude, label = city_abb),
                                     nudge_x = -3, 
                                     nudge_y = -2, 
                                     hjust = "left", 
                                     direction = "x",
                                     box.padding = .5,
                                     max.overlaps = Inf, size = na_size
)


south_cities <- c("Austin", "Houston", "Miami", "New Orleans")
south_labels_repel <- geom_text_repel(data = na_data |> 
                                        filter(city %in% c(south_cities)), 
                                      aes(x = longitude, y = latitude, label = city_abb),
                                      #nudge_x = -5, 
                                      nudge_y = -2, 
                                      hjust = "left", 
                                      direction = "x",
                                      box.padding = .5,
                                      max.overlaps = Inf, size = na_size
)


north_cities <- c("Montréal", "Toronto", "Chicago")
north_labels_repel <- geom_text_repel(data = na_data |> 
                                        filter(city %in% c(north_cities)), 
                                      aes(x = longitude, y = latitude, label = city_abb),
                                      #nudge_x = -5, 
                                      #nudge_y = 0.5, 
                                      hjust = "left", 
                                      direction = "x",
                                      box.padding = .5,
                                      max.overlaps = Inf, size = na_size
)




east_labels_repel <- geom_text_repel(data = na_data |> 
                                       filter(!city %in% c(north_cities, west_cities, south_cities)), 
                                     aes(x = longitude, y = latitude, label = city_abb),
                                     nudge_x = 5, 
                                     nudge_y = -3, 
                                     hjust = "left", 
                                     direction = "x",
                                     box.padding = .5,
                                     max.overlaps = Inf, size = na_size)





##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   EUROPE                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

eu_south_cities <- c("Lisbon", "Madrid", "Barcelona", "Rome", "Milan", "Paris", "Berlin", "Heidelberg", "Athens")
eu_south_labels_repel <- geom_text_repel(data = eu_data |> 
                                           filter(city %in% c(eu_south_cities)), 
                                         aes(x = longitude, y = latitude, label = city_abb),
                                         # nudge_x = -20, 
                                         nudge_y = 0, 
                                         hjust = "left", 
                                         direction = "x",
                                         box.padding = .5,
                                         max.overlaps = Inf, size = na_size
)


eu_north_cities <- c("London", "Copenhagen", "Stockholm", "Warsaw", "Oslo")
eu_north_labels_repel <- geom_text_repel(data = eu_data |> 
                                           filter(city %in% c(eu_north_cities)), 
                                         aes(x = longitude, y = latitude, label = city_abb),
                                         # nudge_x = 1, 
                                         # nudge_y = 2, 
                                         hjust = "left", 
                                         direction = "x",
                                         box.padding = 0.5,
                                         max.overlaps = Inf, size = na_size
)




eu_center_labels_repel <- geom_text_repel(data = eu_data |> 
                                            filter(!city %in% c(eu_north_cities, eu_south_cities)), 
                                          aes(x = longitude, y = latitude, label = city_abb),
                                          nudge_x = -3, 
                                          nudge_y = 1, 
                                          hjust = "left", 
                                          direction = "x",
                                          box.padding = .5,
                                          max.overlaps = Inf, size = na_size)



