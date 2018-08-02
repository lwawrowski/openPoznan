library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

get_local_government <- function(Coord = F){
  # dane wyborzcze samorzadowe 
  
  # wczytanie danych o wyborach samorzadowych
  
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  go <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_lokale_wgs/")
  
  }, error = function(err) {
    
      print(paste("check the internet connection"))
    
  })
  
  gov <- go$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  gov_coord <- data.frame(matrix(unlist(go$features$geometry$coordinates),
                                 nrow = nrow(go$features), byrow = T))
  colnames(gov_coord)[(names(gov_coord)=="X1")] <- "V1"
  colnames(gov_coord)[(names(gov_coord)=="X2")] <- "V2"
  
  gov_basic_info <- data.frame(cbind(gov$id,
                                     gov$properties$przystosowanie,
                                     gov$properties$nr_obwodu,
                                     gov$properties$siedziba))
  colnames(gov_basic_info)<-c("ID","AFDP","District_no","Residence")
  
  # z??czenie wszystkich kolumn
  
  gov_final <- cbind(gov_basic_info, gov_coord)
  
  if(Coord == T){
      result <- gov_coord
    } else {
      return(gov_basic_info)
    }
  
}