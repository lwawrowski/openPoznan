#' graves  Function
#'
#' This function download data about graves in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about graves in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{Cemetery_ID}{numeric; ID of  cemetery in Poznan.}
#' \item{Cemetery_No}{numeric; cemetery number.}
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
#' Grave <- graves(coords = F)
#' Grave_coord <- graves(coords = T)


graves <- function(coords = F){ 
  # wczytanie danych groboW
  
  cementary_final <- cemeteries()
  
  grave_basic_info_all <- data.frame()
  
  grave_coord_all <- data.frame()
  
  # wybieranie 1000 grob?w z wszstkich cmentarzy i podzielenie na dwa zbiory 
  
  for(i in 1:length(cementary_final$ID)){
    
    cat("Cemetery of",i," from ",length(cementary_final$ID), "\n")
    
    id <- cementary_final$ID[i]
    
    if(have_ip() == T) {
      
    tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    new_link <- paste0("http://www.poznan.pl/featureserver/featureserver.cgi/groby?maxFeatures=1000&queryable=cm_id&cm_id=",id)
    }, error = function(err) {
      
      warning("You used bad link!")
    })
      
    }else{
      
      warning("You lost cinnection to internet!")
      
    }      
    n <- fromJSON(new_link)
    
    grave_basic_info <- n$features$properties
    
    grave_coord_raw <- n$features$geometry$coordinates
    
    grave_coord_t <- map(grave_coord_raw, t)
    
    grave_coord_df <- map(grave_coord_t, data.frame)
    
    grave_coord <- map2_df(grave_coord_df, grave_basic_info$cm_id, ~ mutate(.x, id= .y))
    
    grave_basic_info_all <- rbind(grave_basic_info_all, grave_basic_info)
    
    grave_coord_all <- rbind(grave_coord_all, grave_coord)
    
    
  } 
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
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

  grave_coord_all_fine <- cbind(grave_coord_all, grave_basic_info_final)
  if(coords == T){
    result <- grave_coord_all_fine
  } else {
    result <- grave_basic_info_final
  }
  return(result)
}
