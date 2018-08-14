#' Area Electoral Function
#'
#' This function download data about Area Electoral in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{ID}{factor; ID of area electoral.}
#' \item{District_no}{factor; Number of District.}
#' }
#' @examples
#' Area_Electoral <- getArea_electoral(Coord = F)
#' Area_Electoral_coords <- getArea_electoral(Coord = T)


getArea_electoral <- function(Coord = F){
  # dane wybory samorzadowe obwody wyborcze 
  
  # wczytanie danych o wyborach samorzadowych
  
  if(havingIP() == T){
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
    
      ob2 <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_obwody_wgs/")
      },error = function(err) {
    
      warning("You used bad link!")
      })
  
  }else{
    
      warning("You lost connection to internet!")
    
  }  
    
  oblast2 <- ob2$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  oblast2coord <- oblast2$geometry$coordinates
  
  oblast2coord2d <- map(oblast2coord, drop)
  
  oblast2coord_df <- map(oblast2coord2d, as.data.frame)
  
  oblast2coord_id <- map2_df(oblast2coord_df, oblast2$id, ~mutate(.x, id=.y))
  
  
  oblast2_basic_info <- data.frame(ID=oblast2$id,
                                         District_no=oblast2$properties$okreg)
  
  # oblast2_coord <- data.frame(oblast2coord_i
  # z??czenie wszystkich kolumn
  
  oblast2_final <- cbind(oblast2coord_id)
  if(Coord == T){
     result <- oblast2_coord
    } else {
      result <- oblast2_final
    }
  return(result)
}
