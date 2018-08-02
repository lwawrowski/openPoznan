
library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getRelict <- function(Coord = F){
  # turystyka 
  # zabytki
  
  # wczytanie danych o zabytkach
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  r <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=2572")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  relikt <- r$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  relikt_coord <- data.frame(matrix(unlist(r$features$geometry$coordinates),
                                    nrow = nrow(r$features), byrow = T))
  colnames(relikt_coord)[(names(relikt_coord)=="X1")] <- "V1"
  colnames(relikt_coord)[(names(relikt_coord)=="X2")] <- "V2"
  
  
  relikt_basic_info <- data.frame(cbind(relikt$id,
                                        relikt$properties$kod,
                                        relikt$properties$nazwa,
                                        relikt$properties$opis_klasy,
                                        relikt$properties$miasto,
                                        relikt$properties$lang,
                                        relikt$properties$adres,
                                        relikt$properties$id_klasy,
                                        relikt$properties$opis))
  
  colnames(relikt_basic_info)<-c("ID","Code","Name","Category_builiding","City","Lang","Address","ID_monument","Description")
  
  # z??czenie wszystkich kolumn
  
  relikt_final <- cbind(relikt_basic_info,relikt_coord)
  if(Coord == T){
    result <- relikt_coord
  } else {
    return(relikt_final)
  }
}