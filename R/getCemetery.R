library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getCemetery <- function(Coord = F) {
  # wyszukiwarka cmentarzy 
  
  # wczytanie danych cmentarzy 
  c <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/cmentarze/all.json")
  cemetery <- c$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  cemetery_basic_info <- data.frame(cbind(cemetery$id,
                                          cemetery$properties$cm_name,
                                          cemetery$properties$cm_type))
  
  colnames(district_basic_info)<-c("ID","Cemetery_Name","Cemetery_Type")
  
  # z??czenie wszystkich kolumn
  
  cementary_final <- cemetery_basic_info
  
  cemeterycoord <- cementary$geometry$coordinates
  
  cemeterycoord2d <- map(cemeterycoord, drop)
  
  cemeterycoord_df <- map(cemeterycoord2d, as.data.frame)
  
  cemeterycoord_id <- map2_df(cemeterycoord_df, cemetery$id, ~mutate(.x, id=.y))
  
  if(Coord == T){
    result <- cemetery_coord
  } else {
    return(cemetery_basic_info)
  }
}