#' local_spatial_dev_plans_called Function
#'
#' This function download data about Called Local Spatial Development Plans 
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom dplyr mutate
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{id}{factor; id.}
#' \item{lsdpc_location_name}{factor; location name.}
#' \item{lsdpc_adopted_change}{factor; adopted change.}
#' \item{lsdpc_adaptive_resolution}{factor; adaptive resolution.}
#' \item{lsdpc_date_oAR}{factor; date of adaptive resolution.}
#' }
#' @examples
#' local_spatial_dev_plans_called()

local_spatial_dev_plans_called <- function (basic = TRUE) {
  
  
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
  
  
  if(basic == TRUE){
    return(lsdpc_basic_info)
  }
  else{
    return(lsdpc_coord_id)
  }
  
}


