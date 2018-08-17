#' graves_all  Function
#'
#' This function download data about all graves in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about all graves in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom dplyr filter mutate
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{Cemetery_ID}{numeric; ID of  cemetery in Poznan.}
#' \item{Cemetery_No}{numeric; Cemetery number.}
#' \item{Date_birth}{factor; Date birth pearson.}
#' \item{Date_death}{factor; Date death pearson.}
#' \item{Date_Burial}{factor; Date burial pearson.}
#' \item{Row_No}{factor; Row.}
#' \item{Grave_Name}{factor; Name.}
#' \item{Grave_Surname}{factor; Surname.}
#' \item{Grave_Name_Surname}{factor; Name and Surname.}
#' \item{Field_No}{factor; Field number in cemetery.}
#' \item{Place_No}{factor; Place number in cemetery.}
#' \item{Paid}{numeric; Paid.}
#' }
#' @examples
#' Grave_All <- graves_all(coords = F)
#' Grave_All_coord <- graves_all(coords = T)


graves_all <- function(coords =F){
  
  cemeterycoord_id <- cemeteries(coords = T)
  
  
  coords <-cemeterycoord_id %>% mutate(id2=ifelse(id %in% (c(7,8,9,10,11,13,14)), 25, id)) %>%
    arrange(id2)
  #na wsp??rz?dnych
  # zbieranie wszystkich danych 
  
  cm_unique_id <-  unique(coords$id2)
  
  
  grave_basic_info_all <- data.frame()
  
  grave_coord_all <- data.frame()
  
  # cm nr 1 
  for(i in 1:length(unique(coords$id2))){
    
    cm_i <- coords %>% filter(id2 == cm_unique_id[i])
    
    
    min_x1 <- min(cm_i$Longitude)
    max_x1 <- max(cm_i$Longitude)
    
    min_y1 <- min(cm_i$Latitude) 
    max_y1 <- max(cm_i$Latitude) 
    
    a <- expand.grid(x1=seq(min_x1,max_x1,length.out = 10),
                     y1=seq(min_y1,max_y1,length.out = 10))
    b <- a + 0.0015
    names(b) <- c("x2", "y2")
    
    c <- cbind(a,b)
    
    for(j in 1:nrow(c)){
      
      if(have_ip() == T){
        
        tryCatch({ # w przypadku baraku internetu wywoła wyjątek
          
      link <- paste0("http://www.poznan.pl/featureserver/featureserver.cgi/groby/all.json?maxfeatures=9000&bbox=",c$x1[j],",",c$y1[j],",",c$x2[j],",",c$y2[j])
      
        },error = function(err) {
          
          warning("You used bad link!")
        })
        
      }else{
        
        warning("You lost connection to internet!")
      }
      
      d <- fromJSON(link)
      
      
      grave_basic_info_ <- d$features$properties
      
      grave_coord_raw <- d$features$geometry$coordinates
      
      grave_coord_t <- map(grave_coord_raw, t)
      
      grave_coord_df <- map(grave_coord_t, data.frame)
      
      grave_coord <- map2_df(grave_coord_df, grave_basic_info_$cm_id, ~ mutate(.x, id= .y))
      
      grave_basic_info_all <- rbind(grave_basic_info_all, grave_basic_info_)
      
      grave_coord_all <- rbind(grave_coord_all, grave_coord)
      
      cat("Obs: ", nrow(d$features), " Iter: ", j, "cm id: " ,unique(grave_coord$id), "\n")
    }
    
  }
  
  grave_basic_info_final <- data.frame(Cemetery_ID=grave_basic_info_all$cm_id,
                                       Cemetery_No=grave_basic_info_all$cm_nr,
                                       Date_birth=grave_basic_info_all$g_date_birth,
                                       Date_death=grave_basic_info_all$g_date_death,
                                       Date_burial=grave_basic_info_all$g_date_burial,
                                       Row_no=grave_basic_info_all$g_row,
                                       Grave_Name=grave_basic_info_all$g_name,
                                       Grave_Surname=grave_basic_info_all$g_surname,
                                       Grave_Name_Surname=grave_basic_info_all$print_surname_name,
                                       Field_No=grave_basic_info_all$g_field,
                                       Place_No=grave_basic_info_all$g_place,
                                       Paid=grave_basic_info_all$paid)
  
  grave_coord_all <- data.frame(Longitude=grave_coord_all$X1,
                                Latitude=grave_coord_all$X2,
                                id=grave_coord_all$id)
  
  if(coords == T) {
    result2 <- grave_coord_all
  }else{
    return(grave_basic_info_all)
  }
  
}
