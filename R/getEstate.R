getEstate <- function(coords = T) {
# wczytanie danych wyborów do rad osiedli 
  
  #wstępna analiza 
e <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_lokale/")
estate <- e$features

 #Oczyszczenie danych z niepotrzebnych informacji + nazwanie
estate_coord <- data.frame(matrix(unlist(e$features$geometry$coordinates),
                                  nrow = nrow(e$features), byrow = T))
colnames(estate_coord)[(names(estate_coord)=="X1")] <- "Longitude"
colnames(estate_coord)[(names(estate_coord)=="X2")] <- "Latitude"

estate_basic_info <- data.frame(cbind(estate$id,
                                      estate$type,
                                      estate$properties$adres,
                                      estate$properties$siedziba,
                                      estate$properties$okreg,
                                      estate$properties$przystosowanie,
                                      estate$properties$nr_lokalu,
                                      estate$properties$nr_obwodu))
colnames(estate_basic_info)<-c("ID","Type","Address","Residence","Electoral_District","AFDP","Venue_nr","District_nr")

# złączenie wszystkich kolumn

estate_final <- cbind(estate_basic_info, estate_coord)

if(coords == F){
      result <- list(District=estate_basic_info,
                      Coords = estate_coord)
    } else {
    return(estate_basic_info)
      
    }
}



