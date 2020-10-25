#' Visualize Airport Delays
#' 
#' This function plots the Mean Delay Time of Flights in USA Airports.
#' The function does not take any parameters.
#' 
#' @return The function returns a graph.
#' 
#' @example visualize_airport_delays()
#' 
#' @import dplyr
#' @import ggplot2
#' @import nycflights13
#' @importFrom stats model.matrix var
#' @export

visualize_airport_delays = function(){
  lon = lat = dest = arr_delay = Mean = name <- NULL
  options(dplyr.summarise.inform = FALSE)
  delay_flights = nycflights13::flights %>%
    group_by(dest) %>%
    summarize(Mean = mean(arr_delay, na.rm = TRUE)) %>%
    left_join(nycflights13::airports, by = c("dest" = "faa")) %>%
    filter(!is.na(name))
  
  plot = ggplot(delay_flights, aes(x = lon, y = lat, color = Mean)) +
    geom_point() + 
    labs(title = "Mean Flight Delays", x = "Longitude", y ="Latitude")
  
  return(plot)
}
