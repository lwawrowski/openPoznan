#Geolokalizacja z google na temat dokładnej lokalizacji kościoła. 

#Parafie Calosc

getParish <- function (coords = F) {
  
  #Wstepna analiza
  
  Parish_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=parishes&co=parishes")
  
  Parish_features <- Parish_blank$features
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Parish_basic_info <- data.frame(cbind(Parish_features$id,
                                        Parish_features$properties$name))
  
  colnames(Parish_basic_info)[(names(Parish_basic_info) == "X1")] <- "ID"
  colnames(Parish_basic_info)[(names(Parish_basic_info) == "X2")] <- "Parish_Name"
  
  
  
  Parish_coord <- Parish_features$geometry$coordinates
  
  Parish_coord_2d <- map(Parish_coord, drop)
  
  Parish_coord_df <- map(Parish_coord_2d, as.data.frame)
  
  Parish_coord_id <- map2_df(Parish_coord_df,Parish_features$id, ~mutate(.x, id=.y))
  
  colnames(Parish_coord_id)[(names(Parish_coord_id) == "V1")] <- "Longitude"
  colnames(Parish_coord_id)[(names(Parish_coord_id) == "V2")] <- "Latitude"
  
  if (coords == T) {
    result <- list(Parishes=Parish_basic_info,
                   Coords = Parish_coord_id)
  }
  else {
    return(Parish_basic_info)
  }
  
}

#examples

Parish_basic <- getParish()

Parish_with_coord <- getParish(TRUE)