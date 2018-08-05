#' get_bike_place Function
#'
#' This function download statistic about bike station from NextBike
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import jsonlite
#' @format 
#' \describe{
#' \item{id}{factor; station id.}
#' \item{longitude}{factor; longitude.}
#' \item{latitude}{factor; latitude.}
#' \item{name}{factor; station name.}
#' \item{bike_racks}{factor; bike racks.}
#' \item{free_racks}{factor; free racks.}
#' \item{bike_types}{factor; bike types.}
#' \item{bikes}{factor; number of bikes.}
#' \item{update}{factor; last update.}
#' }
#' @examples
#' get_bike_plac()
get_bike_place <- function(){
  
  NextBike <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=stacje_rowerowe')
  
  NextBike_features <- NextBike$features
  
  NextBike_coord <- data.frame(matrix(unlist(NextBike_features$geometry$coordinates),
                                      nrow = nrow(NextBike_features), byrow = T))
  
  NextBike_final <- data.frame(cbind(NextBike_features$id,
                                     NextBike_coord$X1,
                                     NextBike_coord$X2,
                                     NextBike_features$properties$label,
                                     NextBike_features$properties$bike_racks,
                                     NextBike_features$properties$free_racks,
                                     NextBike_features$properties$bikes,
                                     NextBike_features$properties$updated))
  
  colnames(NextBike_final)<-c("id",
                              "longitude",
                              "latitude",
                              "station",
                              "bike_racks",
                              "free_racks",
                              "bikes",
                              "updated")
  
return(NextBike_final)

  
}

                     