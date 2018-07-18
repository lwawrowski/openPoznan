
getPM <- function () {

#Parkomaty calosc 

#Wstepna analiza

PM_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=parking_meters")

PM_features <- PM_blank$features

# Utworzenie koordynatow + nazwanie 

PM_coord <- data.frame(matrix(unlist(PM_features$geometry$coordinates),
                              nrow = nrow(PM_features), byrow = T))

colnames(PM_coord)[(names(PM_coord) == "X1")] <- "Longitude"
colnames(PM_coord)[(names(PM_coord) == "X2")] <- "Latitude"

#Oczyszczenie danych z niepotrzebnych informacji + nazwanie 

PM_basic_info <- data.frame (cbind(PM_features$id, PM_features$properties$zone, PM_features$properties$street))

colnames(PM_basic_info)[(names(PM_basic_info) == "X1")] <- "ID"
colnames(PM_basic_info)[(names(PM_basic_info) == "X2")] <- "PM_Zone"
colnames(PM_basic_info)[(names(PM_basic_info) == "X3")] <- "PM_Street"

# Ostateczne polaczenie 

PM_final <- cbind(PM_basic_info,PM_coord)

return(PM_final)
}

#example

PMs <- getPM()
