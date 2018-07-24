library(jsonlite)
library(dplyr)
library(purrr)

## Local Spatial Development Plans - Called

get_lsdpc <- function (coords = F) {
  
  
  lsdpc <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=urban_planning&co=mpzp')
  
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
  
  lsdpc_coord_unlist <- list()
  
  lsdpc_coord_list <- list()
  
  V1 <- list()
  
  V2 <- list()
  
  Data_frame_multipolygon <- list()
  
  if (any(lsdpc_check == T)) {   
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
  
  
  lsdpc_coord_df <- map(lsdpc_coord_2d,
                       as.data.frame)
  
  
  lsdpc_coord_id <- map2_df(lsdpc_coord_df,
                           lsdpc_features$id,
                           ~mutate(.x, id=.y))
  
  colnames(lsdpc_coord_id) <- c("Longitude",
                               "Latitude",
                               "ID")
  
  
  
  if (coords == T) 
  {
    result <- list(lsdpcp=lsdpc_basic_info,
                   Coords = lsdpc_coord_id)
    
  } else {
    return(lsdpc_basic_info)
  }
  
}

#example 

lsdpc_basic <- get_lsdpc () 

lsdpc_with_coord <- get_lsdpc (TRUE)
