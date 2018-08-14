#'Grave All  Function
#'
#' This function download data about Grave All in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom dplyr filter mutate
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{Cemetery_ID}{factor; ID of  Cemetery in Poznan.}
#' \item{Cemetery_No}{factor; Cemetery number.}
#' \item{Date_birth}{factor; Date birth pearson.}
#' \item{Date_death}{factor; Date death pearson.}
#' \item{Date_Burial}{factor; Date burial pearson.}
#' \item{Row_No}{factor; Row.}
#' \item{Grave_Name}{factor; Name.}
#' \item{Grave_Surname}{factor; Surname.}
#' \item{Grave_Name_Surname}{factor; Name and Surname.}
#' \item{Field_No}{factor; Field number in cemetery.}
#' \item{Place_No}{factor; Place number in cemetery.}
#' \item{Paid}{factor; Paid.}
#' }
#' @examples
#' Grave_All <- getGrave_All(Coord = F)
#' Grave_All_coord <- getGrave_All(Coord = T)


# pobieranie wszystkich grobów
getGrave_all <- function(Coord =F){
  
  cemeterycoord_id <- getCemetery(Coord = T)
  
  
  #na wsp??rz?dnych
  # zbieranie wszystkich danych 
  
  cm_unique_id <-  unique(cemeterycoord_id$id)
  
  
  
  # cm nr 1 
  for(i in 1:length(unique(cemeterycoord_id$id))){
    
    cm_i <- cemeterycoord_id %>% filter(id == cm_unique_id[i])
    
    
    min_x1 <- min(cm_i$V1)
    max_x1 <- max(cm_i$V1)
    
    min_y1 <- min(cm_i$V2) 
    max_y1 <- max(cm_i$V2) 
    
    a <- expand.grid(x1=seq(min_x1,max_x1,length.out = 10),
                     y1=seq(min_y1,max_y1,length.out = 10))
    b <- a + 0.0015
    names(b) <- c("x2", "y2")
    
    c <- cbind(a,b)
    
    
    grave_basic_info_all <- data.frame()
    
    grave_coord_all <- data.frame()
    
    for(i in 1:nrow(c)){
      link <- paste0("http://www.poznan.pl/featureserver/featureserver.cgi/groby/all.json?maxfeatures=9000&bbox=",c$x1[i],",",c$y1[i],",",c$x2[i],",",c$y2[i])
      
      d <- fromJSON(link)
      
      
      grave_basic_info_ <- d$features$properties
      
      grave_coord_raw <- d$features$geometry$coordinates
      
      grave_coord_t <- map(grave_coord_raw, t)
      
      grave_coord_df <- map(grave_coord_t, data.frame)
      
      grave_coord <- map2_df(grave_coord_df, grave_basic_info_$cm_id, ~ mutate(.x, id= .y))
      
      grave_basic_info_all <- rbind(grave_basic_info_all, grave_basic_info_)
      
      grave_coord_all <- rbind(grave_coord_all, grave_coord)
      
      cat("Obs: ", nrow(d$features), " Iter: ", i, "cm id: " ,unique(grave_coord$id), "\n")
    }
    
  }
  
  
  if(Coord == T) {
    result2 <- grave_coord_all
  }else{
    result <- grave_basic_info_all
  }
  return(result)
}
