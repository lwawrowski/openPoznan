#Geolokalizacja z google na temat dokładnej lokalizacji kościoła. 

#Parafie Calosc

parish <- function (coords = F) {
  
  
  #Wstepna analiza
  
  Parish_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=parishes&co=parishes")
  
  Parish_features <- Parish_blank$features
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Parish_basic_info <- data.frame(cbind(Parish_features$id,
                                        Parish_features$properties$name))
  
  colnames(Parish_basic_info) <- c("ID",
                                   "Parish_Name")
  
  Parish_coord <- Parish_features$geometry$coordinates
  
  Parish_coord_2d <- map(Parish_coord, drop)
  
  # for (i in 1:nrow(Parish_features)){
  #   
  #    if (is.list(Parish_coord[[i]]) == F) {
  #    
  #    } else {
  #      
  #      print("Pętla działa, tu pisz kod")
  #    }
  # }
  
  Parish_coord_df <- map(Parish_coord_2d, as.data.frame)
  
  Parish_coord_id <- map2_df(Parish_coord_df,Parish_features$id, ~mutate(.x, id=.y))
  
  colnames(Parish_coord_id) <- c("Longitude",
                                 "Latitude",
                                 "ID")
  
  
  if (coords == T) {
    result <- list(Parishes=Parish_basic_info,
                   Coords = Parish_coord_id)
  }
  else {
    return(Parish_basic_info)
  }
  
}

#examples

parish_basic <- parish()

parish_with_coord <- parish(TRUE)