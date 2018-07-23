
get_local_government <- function(Coord = F){
  # dane wyborzcze samorzadowe 
  
  # wczytanie danych o wyborach samorzadowych
  
  go <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_lokale_wgs/")
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
  
  if(Coord == F){
      result <- gov_coord
    } else {
      return(gov_basic_info)
    }
  
}