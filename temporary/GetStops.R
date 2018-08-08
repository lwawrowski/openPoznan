
stop <- function () {

  #Przystanki ZTM calosc
  
  #Wstepna analiza
  
  Stops_blank <- fromJSON ("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=cluster")
  
  Stops_features <- Stops_blank$features
  
  # Utworzenie koordynatow + nazwanie
  
  Stops_coord <- data.frame(matrix(unlist(Stops_features$geometry$coordinates),
                                   nrow = nrow(Stops_features), byrow = T))
  
  colnames (Stops_coord) <- c("Longitude",
                              "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Stops_basic_info <- data.frame(cbind(Stops_features$id,
                                       Stops_features$properties$zone,
                                       Stops_features$properties$route_type,
                                       Stops_features$properties$headsigns,
                                       Stops_features$properties$stop_name))
  
  colnames (Stops_basic_info) <- c("ID",
                                   "Stop_Zone",
                                   "Route_Type",
                                   "Stop_Headsigns",
                                   "Stop_Name")
  
  # Ostateczne polaczenie 
  
  Stops_final <- cbind(Stops_basic_info,Stops_coord)
  
  # Tworzenie mapy punktowej na wykresie 
  
  ggplot(data = Stops_final,
         aes(x= Longitude,
             y= Latitude,
             group=ID)) +
    geom_point(colour = "blue")
  
  # Mapa Leaflet
  
  Stops_Icon <- icons(iconUrl = ifelse(Stops_final$Route_Type == 3,"https://d30y9cdsu7xlg0.cloudfront.net/png/19259-200.png","http://icons.iconarchive.com/icons/icons8/android/512/Transport-Tram-icon.png"),
                      iconWidth = 25, 
                      iconHeight = 30,
                      iconAnchorX = 15, 
                      iconAnchorY = 25)
  
  Poznan_with_Stops <- leaflet() %>%
    addTiles() %>%  
    addMarkers(lat = Stops_final$Latitude, 
               lng = Stops_final$Longitude, 
               popup = Stops_final$ID,
               icon = Stops_Icon[Stops_final$Route_Type],
               clusterOptions = markerClusterOptions())
  Poznan_with_Stops

return(Stops_final)

}

#example 

Stops <- stop() 