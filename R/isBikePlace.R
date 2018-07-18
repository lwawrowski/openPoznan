isBikePlace <- function(coords = F){
  
  NextBike <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=stacje_rowerowe')
  
  NextBike_features <- NextBike$features
  
  NextBike_coord <- data.frame(matrix(unlist(NextBike_features$geometry$coordinates),
                                      nrow = nrow(NextBike_features), byrow = T))
  colnames(NextBike_coord)[(names(NextBike_coord) == "X1")] <- "Longitude" 
  colnames (NextBike_coord)[(names(NextBike_coord) == "X2")] <- "Latitude"

  NextBike_final <- data.frame(cbind(NextBike_features$id,
                                     NextBike_coord$Longitude,
                                     NextBike_coord$Latitude,
                                     NextBike_features$properties$label,
                                     NextBike_features$properties$bike_racks,
                                     NextBike_features$properties$free_racks,
                                     NextBike_features$properties$bikes,
                                     NextBike_features$properties$updated))
  
  colnames(NextBike_final)[(names(NextBike_final) == "X1")] <- "id"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X2")] <- "longitude"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X3")] <- "latitude"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X4")] <- "nb_station"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X5")] <- "nb_bike_racks"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X6")] <- "nb_free_racks"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X7")] <- "nb_bikes"
  
  colnames(NextBike_final)[(names(NextBike_final) == "X8")] <- "nb_updated"
  
  result <- NextBike_final
  
  if(arg == TRUE){
    result <- TRUE
  } else {
    result <- FALSE
  }
  
  return(result)

  
}