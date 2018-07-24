#Rejony szkol

area <- function (coords = F) {

  #Rejony szkol
  
  #Wstepna analiza
  
  Areas_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=education&co=podstawowe201516")
  
  Areas_features <- Areas_blank$features
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Areas_basic_info <- data.frame(cbind(Areas_features$id,
                                       Areas_features$properties$nr,
                                       Areas_features$properties$shape,
                                       Areas_features$properties$pk, 
                                       Areas_features$properties$nazwa))
  
  colnames(Areas_basic_info)<- c("ID", 
                                 "School_Nr", 
                                 "Shape",
                                 "PK",
                                 "School_Name")
  
  #Koordynaty dla pola z wartosciami Polygon 
  
  Area_coord <- Areas_features$geometry$coordinates
  
  Area_coord_2d <- map(Area_coord, drop)
  
  Area_check <- map(Area_coord_2d,  is.list) 
  
  if (any(Area_check == T)) {   
    
    Area_coord_unlist <- list()
    Area_coord_list <- list()
    V1 <- list()
    V2 <- list()
    Data_frame_multipolygon <- list()
    
      for (i in 1:nrow(Areas_features)){
        
        if (is.list(Area_coord_2d[[i]]) == T) {
          
          name <- paste('Area_coord',i,sep='_')
          
          Area_coord_unlist[[name]] <- unlist(Area_coord_2d[[i]]) 
          
          Area_coord_list[[name]] <- data.frame (Area_coord_unlist[[name]])
          
          V1[[name]] <- (Area_coord_list[[name]] [Area_coord_list[[name]] <18])
          V2[[name]] <- (Area_coord_list[[name]] [Area_coord_list[[name]] >48])
          
          Data_frame_multipolygon[[name]] <- data.frame(V1[[name]],V2[[name]])
          
          Area_coord_2d[[i]] <- Data_frame_multipolygon[[name]]
          
          
        } 
      }
  }else {
    Area_coord_df <- map(Area_coord_2d, 
                         as.data.frame)
  }
  
  if (exists("Area_coord_df") == F) {
  Area_coord_df <- map(Area_coord_2d,
                       as.data.frame)
  }
  
  
  Area_coord_id <- map2_df(Area_coord_df,
                           Areas_features$id,
                           ~mutate(.x, id=.y))
  
  colnames(Area_coord_id) <- c("Longitude",
                               "Latitude",
                               "ID")

  
  
if (coords == T) {
  result <- list(Areas=Areas_basic_info,
                 Coords = Area_coord_id)
  
} else {
  return(Areas_basic_info)
}

}

#example 

Area_basic <- area () 

Area_with_coord <- area (TRUE)

