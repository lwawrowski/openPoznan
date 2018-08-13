#' Circle electoral  Function
#'
#' This function download data about Circle electoral in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of circle.}
#' \item{Name}{factor; Name of circle.}
#' }
#' @examples
#' Circle_electoral <- getCircle(Coord = F)
#' Circle_coord <- getCircle(Coord = T)

getCircle <- function(Coord = F){
  # okragi wyborow rad osiedli
  
  # wczytanie danych rad osiedli
  if(havingIP() == T) {
  
  
    tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
    co <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_okregi/")
  
    }, error = function(err) {
    
    warning("You used bad link!")
    })
    
  }else{
    
    warning("You lost connection to internet!")
    
  }    
    
    
  circle <- co$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  circle_basic_info <- data.frame(ID=circle$id,
                                        Name=circle$properties$okreg)
  
  # z??czenie wszystkich kolumn
  
  circle_final <- cbind(circle_basic_info)
  
  circlecoord <- circle$geometry$coordinates
  
  circlecoord2d <- map(circlecoord, drop)
  
  circlecoord_df <- map(circlecoord2d, as.data.frame)
  
  circlecoord_id <- map2_df(circlecoord_df, circle$id, ~mutate(.x, id=.y))
  
  if(Coord == T){
    result <- circlecoord_id
  } else {
    result <- circle_final  
  }  
  return(result)
}  