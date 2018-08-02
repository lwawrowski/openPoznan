
library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getOblast <- function(Coord = F){
  # obwody rad osiedli
  
  # wczytanie obwod?w rad osiedli
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  ob <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_okregi/")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  oblast <- ob$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  oblast_basic_info <- data.frame(cbind(oblast$id,
                                        oblast$properties$okreg))
  colnames(oblast_basic_info)<-c("ID","Name")
  
  # z??czenie wszystkich kolumn
  
  oblast_final <- cbind(oblast_basic_info)
  
  oblastcoord <- oblast$geometry$coordinates
  
  oblastcoord2d <- map(oblastcoord, drop)
  
  oblastcoord_df <- map(oblastcoord2d, as.data.frame)
  
  oblastcoord_id <- map2_df(oblastcoord_df, oblast$id, ~mutate(.x, id=.y))
  
  if(Coord == T){
    result <- oblastcoord_id
  } else {
    return(oblast_basic_info)
  }
}