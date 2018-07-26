#Geolokalizacja z google na temat dokładnej lokalizacji kościoła. 

#Parafie Calosc

parish <- function (coords = F) {
  
    #Wstepna analiza
    
    Parish_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=parishes&co=parishes")
    
    Parish_features <- Parish_blank$features
    
    #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
    
    Parish_basic_info <- data.frame(cbind(Parish_features$id,
                                          Parish_features$properties$name))
    
    colnames(Parish_basic_info) <- c("ID",
                                     "Parish_Name")
    
    Parish_coord <- Parish_features$geometry$coordinates
    
    Parish_coord_2d <- map(Parish_coord, drop)
    
    Parish_check <- map(Parish_coord_2d,  is.list)
    
    if (any(Parish_check == T)) {   
      
      Parish_coord_unlist <- list()
      Parish_coord_list <- list()
      V1 <- list()
      V2 <- list()
      Data_frame_multipolygon <- list()
      
      for (i in 1:nrow(Parish_features)){
        
        if (is.list(Parish_coord[[i]]) == T) {
          
          name <- paste('Parish_coord',i,sep='_')
          
          Parish_coord_unlist[[name]] <- unlist(Parish_coord_2d[[i]]) 
          
          Parish_coord_list[[name]] <- data.frame (Parish_coord_unlist[[name]])
          
          V1[[name]] <- (Parish_coord_list[[name]] [Parish_coord_list[[name]] <18])
          V2[[name]] <- (Parish_coord_list[[name]] [Parish_coord_list[[name]] >48])
          
          Data_frame_multipolygon[[name]] <- data.frame(V1[[name]],V2[[name]])
          
          Parish_coord_2d[[i]] <- Data_frame_multipolygon[[name]]
        }
      }
    } else {
      Parish_coord_df <- map(Parish_coord_2d,
                             as.data.frame)
    }
    
    if (exists("Parish_coord_df") == F) {
      
      Parish_coord_df <- map(Parish_coord_2d,
                             as.data.frame)
    } else {
      Parish_coord_id <- map2_df(Parish_coord_df,
                                 Parish_features$id,
                                 ~mutate(.x, id=.y))
    }
    colnames(Parish_coord_id) <- c("Longitude",
                                   "Latitude",
                                   "ID")
    
    # Tworzenie mapy punktowej na wykresie 
    
    ggplot(data = Parish_coord_id,
           aes(x= Longitude,
               y= Latitude,
               group=ID)) +
      geom_polygon(colour = "blue")
    
    #Function spatial lines
    
    # https://stackoverflow.com/questions/45237646/r-leaflet-addpolygons-by-group <- problem z leaf let i rozw
    
    Parish_split_data = lapply(unique(Parish_coord_id$ID), function(x) {
      df = as.matrix(Parish_coord_id[Parish_coord_id$ID == x, c("Longitude", "Latitude") ])
      polys = Polygons(list(Polygon(df)), ID = x)
      return(polys)
    })
    
    Parish_data_lines = SpatialPolygons(Parish_split_data)
    
    #Leaflet - ladna mapka
    
    labels <- sprintf("<strong>%s</strong><br/>",
                      Parish_basic_info$Parish_Name) %>% 
                      lapply(htmltools::HTML)
    
    Parish_leaflet_map <- leaflet() %>%
      addTiles() %>%  
      addPolygons(data = Parish_data_lines,
                  weight = 2, 
                  opacity = 1,
                  dashArray = "3",
                  color = "white",
                  smoothFactor = 0.5,
                  fillOpacity = 0.5, 
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
    Parish_leaflet_map
    
    #https://www.jessesadler.com/post/geocoding-with-r/  <- do szukania pkt parafii
    
    Church <- distinct(Parish_basic_info, Parish_Name)
    
    Church_df <- as.data.frame(Church)
    
    Church_df$Parish_Name <- paste("ko?ci??? ", Church_df$Parish_Name)
    
    Church_df$Parish_Name <- ifelse(grepl(" w ", Church_df$Parish_Name), 
                                    Church_df$Parish_Name,
                                    paste(Church_df$Parish_Name, " w Poznaniu"))
    

  if (coords == T) {
    result <- list(Parishes=Parish_basic_info,
                   Coords = Parish_coord_id)
  }
  else {
    return(Parish_basic_info)
  }
  
}

#examples

parish_basic <- parish()

parish_with_coord <- parish(TRUE)