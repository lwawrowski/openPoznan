library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getDistrict <- function(Coord = F) {
  # samorzady pomocnicze 
  
  # wczytanie danych samorzadow pomocniczych 
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  s <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=local_gov&co=osiedla")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  district <- s$features
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  district_basic_info <- data.frame(cbind(district$id,
                                          district$properties$name,
                                          district$type))
  
  colnames(district_basic_info)<-c("ID","Name","Type")
  
  # z??czenie wszystkich kolumn
  
  district_final <- cbind(district_basic_info)
  
  districtcoord <- district$geometry$coordinates
  
  districtcoord2d <- map(districtcoord, drop)
  
  districtcoord_df <- map(districtcoord2d, as.data.frame)
  
  districtcoord_id <- map2_df(districtcoord_df, district$id, ~mutate(.x, id=.y))
  
  if(Coord == T) {
    result <- districtcoord_id
  }else{
    return(district_basic_info)
  }
  # prawie dobrze 
} 