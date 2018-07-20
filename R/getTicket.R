# sprzedaż biletów 
getTicket <- function(arg = TRUE)

tick <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=4803")
ticket <- tick$features 
# utworzenie koordynatów 

ticket_coord <- data.frame(matrix(unlist(tick$features$geometry$coordinates),
                                  nrow = nrow(tick$features), byrow = T))
colnames(ticket_coord)[(names(ticket_coord)=="X1")] <- "Longitude"
colnames(ticket_coord)[(names(ticket_coord)=="X2")] <- "Latitude"
# oczyszczenie danych i zmina nazw 

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

colnames(ticket_basic_info)<-c("Opening_Hours","Opening_Hours","Opening_Hours","Sales_form","ticket","Name","Address","City","Lang","PEKA_Card","ID_Class","Description","POS")

# złączenie wszystkich kolumn

ticket_final <- cbind(ticket_basic_info, ticket_coord)

result <- ticket_final

if(arg == TRUE){
  +    result <- TRUE
  +  } else {
    +    result <- FALSE
    +  }
+  
  +  return(ticket_final)
+}