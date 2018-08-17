#' parishes Function
#'
#' This function download data about parishes in Poznań
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @param coords shows basic_data about parishes. When set TRUE shows coords of Parishes
#' @importFrom jsonlite fromJSON 
#' @importFrom dplyr mutate
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{ID}{character; ID of parish.}
#' \item{Parish_Name}{factor; Number of sewage work.}
#' \item{Longitude}{numeric; Longitude of parish site.}
#' \item{Latitude}{numeric; Latitude of parish site.}
#' }
#' @examples
#' 
#' parish_basic <- parishes()
#'
#' parish_with_coord <- parishes(TRUE)
#' 

parishes <- function (coords = F) {
  
    #Wstepna analiza
    
    if(have_ip() == T) {
    
      tryCatch({
      
    Parish_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=parishes&co=parishes")
    
      },error = function(e) {
        warning("You used bad link or didn't load jsonlite package")
      })
      
    } else {
      warning("You lost connection to internet!")
    }
  
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
    
  if (coords == T) {
    result <- list(Parishes=Parish_basic_info,
                   Coords = Parish_coord_id)
  }
  else {
    return(Parish_basic_info)
  }
  
}
