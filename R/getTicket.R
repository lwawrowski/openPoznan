library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getTicket <- function(Coord = F) {
  # sprzeda biletow
  
  # wczytanie danych biletow 
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
  tick <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=4803")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  ticket <- tick$features 
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  ticket_coord <- data.frame(matrix(unlist(tick$features$geometry$coordinates),
                                    nrow = nrow(tick$features), byrow = T))
  colnames(ticket_coord)[(names(ticket_coord)=="X1")] <- "V1"
  colnames(ticket_coord)[(names(ticket_coord)=="X2")] <- "V2"
  
  
  ticket_basic_info <- data.frame(cbind(ticket$properties$y_4308_godziny_otwar,
                                        ticket$properties$y_4310_godziny_otwar,
                                        ticket$properties$y_4309_godziny_otwar,
                                        ticket$properties$y_4311_forma_sprzeda,
                                        ticket$properties$y_4326_bilety_jednor,
                                        ticket$properties$nazwa,
                                        ticket$properties$adres,
                                        ticket$properties$miasto,
                                        ticket$properties$lang,
                                        ticket$properties$y_4327_peka__karta_n,
                                        ticket$properties$id_klasy,
                                        ticket$properties$opis,
                                        ticket$properties$opis_klasy))
  
  colnames(ticket_basic_info)<-c("Opening_Hours","Opening_Hours","Opening_Hours","Sales_form","Ticket","Name","Address","City","Lang","PEKA_Card","ID_Class","Description","POS")
  
  # z??czenie wszystkich kolumn
  
  ticket_final <- cbind(ticket_basic_info, ticket_coord)
  
  if(Coord == T){
    result <- ticket_coord
    
  } else {
    return(ticket_basic_info)
  }

}