library(jsonlite)
library(dplyr)
library(purrr)

## Local Spatial Development Plans - Passed

get_lsdp <- function (coords = F) {
  

lsdp <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=urban_planning&co=mpzp')

lsdp_features <- lsdp$features

lsdp_basic_info<- data.frame(cbind(lsdp_features$id,
                                   lsdp_features$properties$nazwa,
                                   lsdp_features$properties$uchw_przyst,
                                   lsdp_features$properties$data_przyst,
                                   lsdp_features$properties$uchw_zatw,
                                   lsdp_features$properties$data_zatw,
                                   lsdp_features$properties$publ_dz_urz,
                                   lsdp_features$properties$data_dz_urz))



colnames(lsdp_basic_info)<-c("id",
                             "lsdp_location_name",
                             "lsdp_adaptive_resolution",
                             "lsdp_date_of_AR",
                             "lsdp_approval_of_the_resolution",
                             "lsdp_date_of_the_AR",
                             "lsdp_official_journal",
                             "lsdp_date_of_OJ")

lsdp_coord <- lsdp_features$geometry$coordinates

lsdp_coord_2d <- map(lsdp_coord, drop)

lsdp_check <- map(lsdp_coord_2d,  is.list) 

lsdp_coord_unlist <- list()

lsdp_coord_list <- list()

V1 <- list()

V2 <- list()

Data_frame_multipolygon <- list()

if (any(lsdp_check == T)) {   
  for (i in 1:nrow(lsdp_features)){
    
    if (is.list(lsdp_coord_2d[[i]]) == T) {
      
      name <- paste('lsdp_coord',i,sep='_')
      
      lsdp_coord_unlist[[name]] <- unlist(lsdp_coord_2d[[i]]) 
      
      lsdp_coord_list[[name]] <- data.frame (lsdp_coord_unlist[[name]])
      
      V1[[name]] <- (lsdp_coord_list[[name]] [lsdp_coord_list[[name]] <18])
      
      V2[[name]] <- (lsdp_coord_list[[name]] [lsdp_coord_list[[name]] >48])
      
      Data_frame_multipolygon[[name]] <- data.frame(V1[[name]],V2[[name]])
      
      lsdp_coord_2d[[i]] <- Data_frame_multipolygon[[name]]
      
      
    } 
  }
}else {
  
  lsdp_coord_df <- map(lsdp_coord_2d, 
                       as.data.frame)
}


lsdp_coord_df <- map(lsdp_coord_2d,
                     as.data.frame)


lsdp_coord_id <- map2_df(lsdp_coord_df,
                         lsdp_features$id,
                         ~mutate(.x, id=.y))

colnames(lsdp_coord_id) <- c("Longitude",
                             "Latitude",
                             "ID")



if (coords == T) 
  {
  result <- list(lsdpp=lsdp_basic_info,
                 Coords = lsdp_coord_id)
  
} else {
  return(lsdp_basic_info)
}

}

#example 

lsdp_basic <- get_lsdp () 

lsdp_with_coord <- get_lsdp (TRUE)
