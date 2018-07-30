
pm <- function () {

  #Parkomaty calosc 
  
  #Wstepna analiza
  
  PM_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=parking_meters")
  
  PM_features <- PM_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  PM_coord <- data.frame(matrix(unlist(PM_features$geometry$coordinates),
                                nrow = nrow(PM_features), 
                                byrow = T))
  
  colnames(PM_coord)<- c("Longitude",
                         "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  PM_basic_info <- data.frame (cbind(PM_features$id, PM_features$properties$zone, PM_features$properties$street))
  
  colnames(PM_basic_info) <- c("ID",
                               "PM_Zone", 
                               "PM_Street")
  # Ostateczne polaczenie 
  
  PM_final <- cbind(PM_basic_info,PM_coord)
  
  # Tworzenie mapy punktowej na wykresie 
  
  ggplot(data = PM_final,
         aes(x= Longitude,
             y= Latitude,
             group=ID)) +
    geom_point(colour = "blue")
  
  # Mapa Leaflet
  
  PM_Icon <- makeIcon(iconUrl = "https://image.flaticon.com/icons/svg/34/34783.svg",
                      iconWidth = 25, 
                      iconHeight = 30,
                      iconAnchorX = 15, 
                      iconAnchorY = 25)
  
  Poznan_with_PM <- leaflet() %>%
    addTiles() %>%  
    addMarkers(lat = PM_final$Latitude, 
               lng = PM_final$Longitude, 
               popup = PM_final$ID,
               icon = PM_Icon,
               clusterOptions = markerClusterOptions())
  
  

return(PM_final)
}

#example

pms <- pm()
