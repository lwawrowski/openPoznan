#' bike_stations Function
#'
#' This function download statistic about bike station from NextBike
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{id}{factor; station id.}
#' \item{Longitude}{numericr; Longitude.}
#' \item{Latitude}{numeric; Latitude.}
#' \item{name}{factor; station name.}
#' \item{bike_racks}{factor; bike racks.}
#' \item{free_racks}{factor; free racks.}
#' \item{bike_types}{factor; bike types.}
#' \item{bikes}{factor; number of bikes.}
#' \item{update}{factor; last update.}
#' }
#' @examples
#' bike_stations()
bike_stations <- function(){
  
  NextBike <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=stacje_rowerowe')
  
  NextBike_features <- NextBike$features
  
  NextBike_coord <- data.frame(matrix(unlist(NextBike_features$geometry$coordinates),
                                      nrow = nrow(NextBike_features), byrow = T))
  
  NextBike_final <- data.frame(id =NextBike_features$id,
                               longitude= NextBike_coord$X1,
                               latitude= NextBike_coord$X2,
                               station= NextBike_features$properties$label,
                               bike_racks=  NextBike_features$properties$bike_racks,
                               free_racks=  NextBike_features$properties$free_racks,
                               bikes=  NextBike_features$properties$bikes,
                               updated=NextBike_features$properties$updated)
  
  colnames(NextBike_final)<-c("id",
                              "Longitude",
                              "Latitude",
                              "station",
                              "bike_racks",
                              "free_racks",
                              "bikes",
                              "updated")
  
return(NextBike_final)

  
}

                     