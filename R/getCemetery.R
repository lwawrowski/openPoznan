#' Cemetery Function
#'
#' This function download data about Cemetery in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{factor; ID of Cemetery in Poznan.}
#' \item{Cemetery_Name}{factor; Name of cemetery in Poznan.}
#' \item{Cemetery_Type}{factor; Type of cemetery in Poznan.}
#' }
#' @examples
#' Cemetery <- getCemetery(Coord = F)
#' Cemetery_coords <- getCemetery(Coord = T)

# wyszukiwarka cmentarzy 

getCemetery <- function(Coord = F) {
  
  # wczytanie danych cmentarzy 
  
  if(havingIP() == T) {
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
      c <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/cmentarze/all.json")
      }, error = function(err) {
    
      warning("You used bad link!")
      })
    
  }else{
    
      warning("You lost connection to internet!")
    
  }  
  
  
  cemetery <- c$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  cemetery_basic_info <- data.frame(ID=cemetery$id,
                                          Cemetery_Name=cemetery$properties$cm_name,
                                          tery_Type=Cemecemetery$properties$cm_type)
  
  # z??czenie wszystkich kolumn
  
  cemetery_final <- cemetery_basic_info
  
  cemeterycoord <- cemetery$geometry$coordinates
  
  cemeterycoord2d <- map(cemeterycoord, drop)
  
  cemeterycoord_df <- map(cemeterycoord2d, as.data.frame)
  
  cemeterycoord_id <- map2_df(cemeterycoord_df, cemetery$id, ~mutate(.x, id=.y)) %>% distinct()
  
  if(Coord == T){
    result <- cemeterycoord_id
  } else {
    result <- cemetery_basic_info
  }
  return(result)
}
