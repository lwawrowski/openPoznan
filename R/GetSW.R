getSW <- function() {


#Scieki Calosc

#Wstepna analiza

SW_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=environment&co=omnc02")

SW_feautures <- SW_blank$features

# Utworzenie koordynatow + nazwanie 

SW_coord <- data.frame(matrix(unlist(SW_feautures$geometry$coordinates),
                              nrow = nrow(SW_feautures), byrow = T))

colnames(SW_coord)[(names(SW_coord) == "X1")] <- "Longitude" 
colnames (SW_coord)[(names(SW_coord) == "X2")] <- "Latitude"


#Oczyszczenie danych z niepotrzebnych informacji + nazwanie 

SW_basic_info <- data.frame(cbind(SW_feautures$id,
                                  SW_feautures$properties$no,
                                  SW_feautures$properties$street,
                                  SW_feautures$properties$dzialka))

colnames(SW_basic_info)[(names(SW_basic_info) == "X1")] <- "ID"
colnames(SW_basic_info)[(names(SW_basic_info) == "X2")] <- "SW_Nr"
colnames(SW_basic_info)[(names(SW_basic_info) == "X3")] <- "SW_Street "
colnames(SW_basic_info)[(names(SW_basic_info) == "X4")] <- "SW_Plot"

# Ostateczne polaczenie 

SW_final <- cbind (SW_basic_info,SW_coord)

return (SW_final)

}


SWs <- getSW()