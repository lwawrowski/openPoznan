library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getEstate <- function(Coord = F) {
  # dane wyborcze rad osiedli 
  
  # wczytanie danych rad osiedli
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  e <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_lokale/")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  estate <- e$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  estate_coord <- data.frame(matrix(unlist(e$features$geometry$coordinates),
                                    nrow = nrow(e$features), byrow = T))
  colnames(estate_coord)[(names(estate_coord)=="X1")] <- "V1"
  colnames(estate_coord)[(names(estate_coord)=="X2")] <- "V2"
  
  estate_basic_info <- data.frame(cbind(estate$id,
                                        estate$type,
                                        estate$properties$adres,
                                        estate$properties$siedziba,
                                        estate$properties$okreg,
                                        estate$properties$przystosowanie,
                                        estate$properties$nr_lokalu,
                                        estate$properties$nr_obwodu))
  colnames(estate_basic_info)<-c("ID","Type","Address","Residence","Electoral_District","AFDP","Venue_no","District_no")
  
  # z??czenie wszystkich kolumn
  
  estate_final <- cbind(estate_basic_info, estate_coord)
  
  if(Coord == T){
    result <- estate_coord
  } else {
    return(estate_basic_info)
  }
  
}