#' District  Function
#'
#' This function download data about District  in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{factor; ID of District.}
#' \item{Name}{factor; Name of District.}
#' \item{Type}{factor; Type of District.}
#' }
#' @examples
#' District <- getDistrict(Coord = F)
#' District_coord <- getDistrict(Coord = T)

getDistrict <- function(Coord = F) {
  # samorzady pomocnicze 
  
  # wczytanie danych samorzadow pomocniczych 
  if(havingIP() == T) {
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
      s <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=local_gov&co=osiedla")
      }, error = function(err) {
    
      warning("You used bad link!")
      })
    
  }else{
    
      warning("You lost connection to internet!")
  }      
    
  district <- s$features
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  district_basic_info <- data.frame(ID=district$id,
                                          Name=district$properties$name,
                                          Type=district$type)
  
  # z??czenie wszystkich kolumn
  
  district_final <- cbind(district_basic_info)
  
  districtcoord <- district$geometry$coordinates
  
  districtcoord2d <- map(districtcoord, drop)
  
  districtcoord_df <- map(districtcoord2d, as.data.frame)
  
  districtcoord_id <- map2_df(districtcoord_df, district$id, ~mutate(.x, id=.y))
  
  if(Coord == T) {
    result <- districtcoord_id
  }else{
    result <- district_basic_info
  }
  return(result)
} 