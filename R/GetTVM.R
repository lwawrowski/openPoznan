

tvm <- function () {

  #Biletomaty Calosc 
  
  #Wstepna analiza
  
  TVM_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=4000")
  
  TVM_features <- TVM_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  TVM_coord <- data.frame(matrix(unlist(TVM_features$geometry$coordinates),
                                 nrow = nrow(TVM_features),
                                 byrow = T))
  
  colnames(TVM_coord) <- c("Longitude",
                           "Latitude")
  
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  TVM_basic_info <- data.frame(cbind(TVM_features$id,
                                     TVM_features$properties$nazwa,
                                     TVM_features$properties$y_4345_obs_uga_syste,
                                     TVM_features$properties$y_4346_karty_p_atnic,
                                     TVM_features$properties$kolejnosc,
                                     TVM_features$properties$opis))
  
  colnames(TVM_basic_info)<- c("ID",
                               "TVM",
                               "PEKA",
                               "Store_Card",
                               "TVM_Order",
                               "TVM_Description")

  # Ostateczne polaczenie 
  
  TVM_final <- cbind(TVM_basic_info,TVM_coord)
  
  # Tworzenie mapy punktowej na wykresie 
  
  TVM_points <- geom_point(data = TVM_final,
                           aes(x= Longitude,
                               y= Latitude,
                               group=ID), colour = "blue")
  
  # Pobranie mapy poznania 
  
  get_poznan <- get_map(c(16.916, 52.42), zoom = 11) 
  poznan <- ggmap(get_poznan)
  
  Poznan_with_TVM <- poznan + TVM_points
  
  plot(Poznan_with_TVM)
  
  # Mapa Leaflet
  
  TVM_Icon <- makeIcon(iconUrl = "https://d30y9cdsu7xlg0.cloudfront.net/png/44651-200.png",
                       iconWidth = 25, 
                       iconHeight = 30,
                       iconAnchorX = 15, 
                       iconAnchorY = 25)
  
  
  Poznan_with_TVM2 <- leaflet() %>%
    addTiles() %>%  
    addMarkers(lat = TVM_final$Latitude, 
               lng = TVM_final$Longitude, 
               popup = TVM_final$ID,
               icon = TVM_Icon,
               clusterOptions = markerClusterOptions())
  Poznan_with_TVM2
  
  # Przydatny poradnik:  https://rstudio.github.io/leaflet/shapes.html
  
return(TVM_final)

}


#examples 


tvms <- tvm()


