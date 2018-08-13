library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

# wyszukiwarka cmentarzy 
getCemetery <- function(Coord = F) {
  
  # wczytanie danych cmentarzy 
  result <- tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    c <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/cmentarze/all.json")
  }, error = function(err) {
    
    print(paste("brak internetu lub zły link"))
    
  })
  
  cemetery <- c$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  cemetery_basic_info <- data.frame(cbind(cemetery$id,
                                          cemetery$properties$cm_name,
                                          cemetery$properties$cm_type))
  
  colnames(cemetery_basic_info)<-c("ID","Cementary_Name","Cementary_Type")
  
  # z??czenie wszystkich kolumn
  
  cemetery_final <- cemetery_basic_info
  
  cemeterycoord <- cemetery$geometry$coordinates
  
  cemeterycoord2d <- map(cemeterycoord, drop)
  
  cemeterycoord_df <- map(cemeterycoord2d, as.data.frame)
  
  cemeterycoord_id <- map2_df(cemeterycoord_df, cemetery$id, ~mutate(.x, id=.y)) %>% distinct()
  
  if(Coord == T){
    result <- cemeterycoord_id
  } else {
    return(cemetery_basic_info)
  }
}