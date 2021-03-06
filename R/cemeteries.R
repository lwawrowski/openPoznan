#' cemeteries Function
#'
#' This function download data about Cemetery in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about cemetery in Poznań. When set TRUE shows coords of cemetery.
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate distinct
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of Cemetery in Poznan.}
#' \item{Cemetery_Name}{factor; Name of cemetery in Poznan.}
#' \item{Cemetery_Type}{factor; Type of cemetery in Poznan.}
#' \item{Longitude}{numeric; Longitude of cemetery site.}
#' \item{Latitude}{numeric; Latitude of cemetery site.}
#' }
#' @examples
#' Cemetery <- cemeteries(coords = F)
#' Cemetery_coords <- cemeteries(coords = T)
#' coords <- Cemetery_coords$coord

# wyszukiwarka cmentarzy 

cemeteries <- function(coords = F) {
  
  # wczytanie danych cmentarzy 
  
  if(have_ip() == T) {
  
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
                                          Cemetery_Type=cemetery$properties$cm_type)
  
  # z??czenie wszystkich kolumn
  
  cemetery_final <- cemetery_basic_info
  
  cemeterycoord <- cemetery$geometry$coordinates
  
  cemeterycoord2d <- map(cemeterycoord, drop)
  
  cemeterycoord_df <- map(cemeterycoord2d, as.data.frame)
  
  cemeterycoord_id <- map2_df(cemeterycoord_df, cemetery$id, ~mutate(.x, id=.y)) %>% distinct()
  
  cemeterycoord_id <- data.frame(Longitude=cemeterycoord_id$V1,
                                 Latitude=cemeterycoord_id$V2,
                                 id=cemeterycoord_id$id)
  
  if(coords == T){
    result <- list(Area=cemetery_basic_info,
                  coord=cemeterycoord_id)
    
  } else {
  return(cemetery_basic_info)
  }
}
