library(jsonlite)

isBikePlace <- function(coords = F){
  
  NextBike <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=stacje_rowerowe')
  
  NextBike_features <- NextBike$features
  
  NextBike_coord <- data.frame(matrix(unlist(NextBike_features$geometry$coordinates),
                                      nrow = nrow(NextBike_features), byrow = T))
  
  NextBike_final <- data.frame(cbind(NextBike_features$id,
                                     NextBike_coord$Longitude,
                                     NextBike_coord$Latitude,
                                     NextBike_features$properties$label,
                                     NextBike_features$properties$bike_racks,
                                     NextBike_features$properties$free_racks,
                                     NextBike_features$properties$bikes,
                                     NextBike_features$properties$updated))
  
  colnames(NextBike_final)<-c("id","longitude","latitude","nb_station","nb_bike_racks","nb_free_racks","nb_bikes","nb_updated")
  
  result <- NextBike_final
  
  if(arg == TRUE){
    
    result <- TRUE
    
  } else {
    
    result <- FALSE
  }
  
  return(result)

  
}