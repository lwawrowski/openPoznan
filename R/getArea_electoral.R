library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getArea_electoral <- function(Coord = F){
  # dane wybory samorzadowe obwody wyborcze 
  
  # wczytanie danych o wyborach samorzadowych
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  ob2 <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_obwody_wgs/")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  oblast2 <- ob2$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  oblast2coord <- oblast2$geometry$coordinates
  
  oblast2coord2d <- map(oblast2coord, drop)
  
  oblast2coord_df <- map(oblast2coord2d, as.data.frame)
  
  oblast2coord_id <- map2_df(oblast2coord_df, oblast2$id, ~mutate(.x, id=.y))
  
  
  oblast2_basic_info <- data.frame(cbind(oblast2$id,
                                         oblast2$properties$okreg))
  
  colnames(oblast2_basic_info)<-c("District_no")
  
  
  # z??czenie wszystkich kolumn
  
  oblast2_final <- cbind(oblast2coord_id)
  if(Coord == T){
     result <- oblast2_coord
    } else {
      return(oblast2_final)  
    }
}