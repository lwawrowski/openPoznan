
getStops <- function () {


#Przystanki ZTM calosc

#Wstepna analiza

Stops_blank <- fromJSON ("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=cluster")

Stops_features <- Stops_blank$features

# Utworzenie koordynatow + nazwanie

Stops_coord <- data.frame(matrix(unlist(Stops_features$geometry$coordinates),
                                 nrow = nrow(Stops_features), byrow = T))

colnames (Stops_coord)[(names(Stops_coord) == "X1")] <- "Longitude"
colnames (Stops_coord)[(names(Stops_coord) == "X2")] <- "Latitude"

#Oczyszczenie danych z niepotrzebnych informacji + nazwanie 

Stops_basic_info <- data.frame(cbind(Stops_features$id,
                                     Stops_features$properties$zone,
                                     Stops_features$properties$route_type,
                                     Stops_features$properties$headsigns,
                                     Stops_features$properties$stop_name))

colnames (Stops_basic_info)[(names(Stops_basic_info) == "X1")] <- "ID"
colnames (Stops_basic_info)[(names(Stops_basic_info) == "X2")] <- "Stop_Zone"
colnames (Stops_basic_info)[(names(Stops_basic_info) == "X3")] <- "Route_Type "
colnames (Stops_basic_info)[(names(Stops_basic_info) == "X4")] <- "Stop_Headsigns"
colnames (Stops_basic_info)[(names(Stops_basic_info) == "X5")] <- "Stop_Name"

# Ostateczne polaczenie 

Stops_final <- cbind(Stops_basic_info,Stops_coord)

return(Stops_final)

}

#example 

Stops <- getStops() 