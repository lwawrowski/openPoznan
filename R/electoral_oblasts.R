#' electoral_oblasts Function
#'
#' This function download data about electoral Oblast in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about electoral Oblast in Poznań. When set TRUE shows coords of schools area.
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of Oblast.}
#' \item{Name}{factor; Name of Oblast in Poznan.}
#' \item{Longitude}{numeric; Longitude of Oblast.}
#' \item{Latitude}{numeric; Latitude of Oblast.}
#' }
#' @examples
#' oblast <- electoral_oblasts(coords = F)
#' oblast_coord <- electoral_oblasts(coords = T)


electoral_oblasts <- function(coords = F){
  # obwody rad osiedli
  
  # wczytanie obwod?w rad osiedli
  if(have_ip() == T) {
  
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
        
      ob <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_okregi/")
      
      }, error = function(err) {
    
      warning("You used bad link!")
      })
    
  }else{
    
      warning("You lost connection to internet!")
    
  }    
  oblast <- ob$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  oblast_basic_info <- data.frame(ID=oblast$id,
                                        Name=oblast$properties$okreg)
  
  # z??czenie wszystkich kolumn
  
  oblast_final <- cbind(oblast_basic_info)
  
  oblastcoord <- oblast$geometry$coordinates
  
  oblastcoord2d <- map(oblastcoord, drop)
  
  oblastcoord_df <- map(oblastcoord2d, as.data.frame)
  
  oblastcoord_id <- map2_df(oblastcoord_df, oblast$id, ~mutate(.x, id=.y))
  
  oblastcoord_id <- data.frame(Longitude=oblastcoord_id$V1,
                               Latitude=oblastcoord_id$V2,
                               id=oblastcoord_id$id)
  
  if(coords == T){
    result <- oblastcoord_id
  } else {
   result <- oblast_basic_info
  }
  return(result)
}
