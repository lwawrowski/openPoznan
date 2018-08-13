#'Oblast Function
#'
#' This function download data about Oblast in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of Oblast.}
#' \item{Name}{factor; Name of Oblast in Poznan.}
#' }
#' @examples
#' oblast <- getOblast(Coord = F)
#' oblast_coord <- getOblast(Coord = T)


getOblast <- function(Coord = F){
  # obwody rad osiedli
  
  # wczytanie obwod?w rad osiedli
  if(havingIP() == T) {
  
  
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
  
  if(Coord == T){
    result <- oblastcoord_id
  } else {
   result <- oblast_basic_info
  }
  return(result)
}