sw <- function() {

  #Scieki Calosc
  
  #Wstepna analiza
  
  SW_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=environment&co=omnc02")
  
  SW_feautures <- SW_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  SW_coord <- data.frame(matrix(unlist(SW_feautures$geometry$coordinates),
                                nrow = nrow(SW_feautures), 
                                byrow = T))
  
  colnames(SW_coord) <- c("Longitude", "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  SW_basic_info <- data.frame(cbind(SW_feautures$id,
                                    SW_feautures$properties$no,
                                    SW_feautures$properties$street,
                                    SW_feautures$properties$dzialka))
  
  colnames(SW_basic_info) <- c("ID",
                               "SW_Nr",
                               "SW_Street",
                               "SW_Plot")
  
  # Ostateczne polaczenie 
  
  SW_final <- cbind (SW_basic_info,
                     SW_coord)
  
  # Tworzenie mapy punktowej na wykresie 
  
  ggplot(data = SW_final,
         aes(x= Longitude,
             y= Latitude,
             group=ID)) +
    geom_point(colour = "blue")
  
  # Mapa Leaflet
  
  SW_Icon <- makeIcon(iconUrl = "",
                      iconWidth = 25, 
                      iconHeight = 30,
                      iconAnchorX = 15, 
                      iconAnchorY = 25)
  
  Poznan_with_SW <- leaflet() %>%
    addTiles() %>%  
    addMarkers(lat = SW_final$Latitude, 
               lng = SW_final$Longitude, 
               popup = SW_final$ID,
               icon = SW_Icon,
               clusterOptions = markerClusterOptions())
  
  

return (SW_final)

}


SWs <- sw()