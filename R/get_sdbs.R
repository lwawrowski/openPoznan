library(jsonlite)
library(tidyverse)
library(ggmap)
library(sp)
library(rgeos)
library(geosphere)
library(gmapsdistance)
library(dplyr)
#' get_sdbs Function
#'
#' This function download data from NextBike and and calculates the shortest distance between stations
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import jsonlite, tidyverse, ggmap, sp, rgeos, geosphere, gmapsdistance, dplyr
#' @format 
#' \describe{
#' \item{station}{factor; station name.}
#' \item{longitude}{factor; longitude}
#' \item{latitude}{factor; latitude.}
#' \item{station}{factor; station name.}
#' \item{longitude}{factor; longitude}
#' \item{latitude}{factor; latitude.}
#' \item{straight_distance}{factor; the shortest straight distance between stations.}
#' }
#' @examples
#' get_sdbs
get_sdbs <- function(){
  
sdbs <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=stacje_rowerowe")
sdbs_coords <- data.frame(matrix(unlist(sdbs$features$geometry$coordinates), 
                              nrow = nrow(sdbs$features), byrow = T))
names(sdbs_coords) <- c(c("lon", "lat"))

sdbs_prop <- sdbs$features$properties

bike_station <- cbind(sdbs_prop, sdbs_coords)



get_poznan <- get_map(c(16.917, 52.41), zoom = 14)
poznan <- ggmap(get_poznan)

poznan + geom_point(data=bike_station, aes(x=lon, y=lat)) +
  scale_color_manual(values=c("#F80500", "#F4C604", "#07D500")) +
  ylab("") + xlab("")


bike_station <- bike_station %>%
  mutate(id=1:nrow(bike_station))

bike_station_sp <- bike_station
coordinates(bike_station_sp) <- ~lon+lat

dist <- distm(bike_station_sp)
min_dist <- apply(dist, 1, function(x) order(x, decreasing=F)[2])

bike_station_dist <- cbind(bike_station, bike_station[min_dist,], 
                       apply(dist, 1, function(x) sort(x, decreasing=F)[2]))

colnames(bike_station_dist) <- c(colnames(bike_station), 
                             paste0("n_", colnames(bike_station)), "straight_distance")
bike_station_dist_2 <- sqldf("select * from bike_station_dist order by straight_distance")


sdbs_final <- data.frame(cbind(bike_station_dist_2$label,
                               bike_station_dist_2$lon,
                               bike_station_dist_2$lat,
                               bike_station_dist_2$n_label,
                               bike_station_dist_2$n_lon,
                               bike_station_dist_2$n_lat,
                               bike_station_dist_2$straight_distance,
                               bike_station_dist_2$updated))

colnames(sdbs_final)<-c("station",
                        "longitude",
                        "latitude",
                        "station",
                        "longitude",
                        "latitude",
                        "straight_distance",
                        "updated")

result <- sdbs_final

if(arg == TRUE){
  
  result <- TRUE
  
} else {
  
  result <- FALSE
}

return(result)


}
