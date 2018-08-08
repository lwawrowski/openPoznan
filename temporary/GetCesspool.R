cesspool <- function() {

  #Szamba 
  
  #Wstepna analiza
  
  Cesspool_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=environment&co=omnc01%20")
  
  Cesspool_feautures <- Cesspool_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  Cesspool_coord <- data.frame(matrix(unlist(Cesspool_feautures$geometry$coordinates),
                                      nrow = nrow(Cesspool_feautures), 
                                      byrow = T))
  
  colnames(Cesspool_coord) <- c("Longitude",
                                "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Cesspool_basic_info <- data.frame(cbind(Cesspool_feautures$id,
                                          Cesspool_feautures$properties$no,
                                          Cesspool_feautures$properties$street,
                                          Cesspool_feautures$properties$dzialka))
  
  colnames(Cesspool_basic_info) <- c("ID",
                                     "Cesspool_Nr",
                                     "Cesspool_Street",
                                     "Cesspool_Plot")
  
  # Ostateczne polaczenie 
  
  Cesspool_Final <- cbind(Cesspool_basic_info,Cesspool_coord)


return (Cesspool_Final)

}

#examples 

Cesspools <- cesspool()