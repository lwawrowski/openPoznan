#' electoral_circles  Function
#'
#' This function download data about electoral circlesl in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about electoral circles of local government in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of circle.}
#' \item{Name}{factor; Name of circle.}
#' }
#' @examples
#' Circle_electoral <- electoral_circles(coords = F)
#' Circle_coord <- electoral_circles(coords = T)

electoral_circles <- function(coords = F){
  # okragi wyborow rad osiedli
  
  # wczytanie danych rad osiedli
  if(have_ip() == T) {
  
  
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
  
  circlecoord_id <- data.frame(Longitude=circlecoord_id$V1,
                               Latitude=circlecoord_id$V2,
                               id=circlecoord_id$id)
  
  if(coords == T){
    result <- circlecoord_id
  } else {
    result <- circle_final  
  }  
  return(result)
}  
