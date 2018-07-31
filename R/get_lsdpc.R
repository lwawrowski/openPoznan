library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)
library(leaflet)
library(sp)

## Local Spatial Development Plans - Called

get_lsdpc <- function (coords = F) {
  
  
  lsdpc <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=urban_planning&co=mpzpw')
  
  lsdpc_features <- lsdpc$features
  
  lsdpc_basic_info <- data.frame(cbind(lsdpc_features$id,
                                  lsdpc_features$properties$nazwa,
                                  lsdpc_features$properties$uchw_zmiana,
                                  lsdpc_features$properties$uchw_przyst,
                                  lsdpc_features$properties$data_przyst))
  
  
  colnames(lsdpc_basic_info)<-c("id",
                               "lsdpc_location_name",
                               "lsdpc_adopted_change",
                               "lsdpc_adaptive_resolution",
                               "lsdpc_date_oAR")
  
  lsdpc_coord <- lsdpc_features$geometry$coordinates
  
  lsdpc_coord_2d <- map(lsdpc_coord, drop)
  
  lsdpc_check <- map(lsdpc_coord_2d,  is.list) 
  
  if (any(lsdpc_check == T)) {   
    
    lsdpc_coord_unlist <- list()
    lsdpc_coord_list <- list()
    V1 <- list()
    V2 <- list()
    Data_frame_multipolygon <- list()
    
    for (i in 1:nrow(lsdpc_features)){
      
      if (is.list(lsdpc_coord_2d[[i]]) == T) {
        
        name <- paste('lsdpc_coord',i,sep='_')
        
        lsdpc_coord_unlist[[name]] <- unlist(lsdpc_coord_2d[[i]]) 
        
        lsdpc_coord_list[[name]] <- data.frame (lsdpc_coord_unlist[[name]])
        
        V1[[name]] <- (lsdpc_coord_list[[name]] [lsdpc_coord_list[[name]] <18])
        V2[[name]] <- (lsdpc_coord_list[[name]] [lsdpc_coord_list[[name]] >48])
        
        Data_frame_multipolygon[[name]] <- data.frame(V1[[name]],V2[[name]])
        
        lsdpc_coord_2d[[i]] <- Data_frame_multipolygon[[name]]
        
        
      } 
    }
  }else {
    lsdpc_coord_df <- map(lsdpc_coord_2d, 
                         as.data.frame)
  }
  
  if (exists("lsdpc_coord_df") == F) {
    lsdpc_coord_df <- map(lsdpc_coord_2d,
                         as.data.frame)
  }
  
  lsdpc_coord_id <- map2_df(lsdpc_coord_df,
                           lsdpc_features$id,
                           ~mutate(.x, id=.y))
  
  
  colnames(lsdpc_coord_id) <- c("Longitude",
                               "Latitude",
                               "ID",
                               "Added_1",
                               "Added_2")
  
  lsdpc_coord_id$Longitude <-ifelse(is.na(lsdpc_coord_id$Longitude),
                                   lsdpc_coord_id$Added_1,
                                   lsdpc_coord_id$Longitude)
  
  lsdpc_coord_id$Latitude <- ifelse(is.na(lsdpc_coord_id$Latitude),
                                   lsdpc_coord_id$Added_2,
                                   lsdpc_coord_id$Latitude)
  
  lsdpc_coord_id <- subset(lsdpc_coord_id, select = -c(Added_1,
                                                     Added_2))
  
  
  # Tworzenie mapy punktowej na wykresie 
  
  ggplot(data = lsdpc_coord_id,
         aes(x= Longitude,
             y= Latitude,
             group=ID)) +
    geom_polygon(colour = "blue")
  
  
  lsdpc_split_data = lapply(unique(lsdpc_coord_id$ID), function(x) {
    df = as.matrix(lsdpc_coord_id[lsdpc_coord_id$ID == x, c("Longitude", "Latitude") ])
    polys = Polygons(list(Polygon(df)), ID = x)
    return(polys)
  })
  
  lsdpc_data_lines = SpatialPolygons(lsdpc_split_data)
  
  #Leaflet - ladna mapka
  
  
  
  lsdpc_leaflet_map <- leaflet() %>%
    addTiles() %>%  
    addPolygons(data = lsdpc_data_lines,
                weight = 2, 
                opacity = 1,
                dashArray = "3",
                color = "green",
                smoothFactor = 0.5,
                fillOpacity = 0.5, 
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
  
  lsdpc_leaflet_map
  
  
  
  if (coords == T) 
  {
    result <- list(lsdpcp=lsdpcc_basic_info,
                   Coords = lsdpcc_coord_id)
    
  } else {
    return(lsdpc_basic_info)
  }
  
}


