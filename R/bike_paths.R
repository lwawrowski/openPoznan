#' bike_paths Function
#'
#' This function download data about Bike Paths in Poznan.
#' @keywords keyword
#' @export
#' @param Coord show basic_data about Bike paths in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{ID}{factor; ID of all bike paths in Poznan.}
#' \item{Name}{factor; Name of bike paths.}
#' \item{Lenght}{factor; Lenght of bike paths}
#' \item{Description}{factor; Description of bike paths}
#' }
#' @examples
#' Bike_Paths <- getBike_Paths(Coord = F)
#' Area_Electoral_coords <- getBike_Paths(Coord = T)

bike_paths <- function(Coord = F){
  # szlaki rowerowe
  
  if(have_ip() == T) {
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
    
      b <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=cycling_routes")
      },error = function(err) {
    
      warning("You used bad link!")
      })
    
  }else{
    
      warning("You lost connection to internet!")
  }  
    
  bikepaths <- b$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  bcoord <- bikepaths$geometry$coordinates
  
  bcoord_df <- map(bcoord, as.data.frame)
  
  bcoord_id <- map2_df(bcoord_df, bikepaths$properties$name,  ~mutate(.x, id2=.y))
  
  bikepaths_basic_info <- data.frame(ID=bikepaths$id,
                                           Name=bikepaths$properties$name,
                                           Lenght=bikepaths$properties$length,
                                           Description=bikepaths$properties$desc)

  
  # z??czenie wszystkich kolumn
  
  bikepaths_final <- cbind(bikepaths_basic_info)
  
  if(Coord == T){
    result <- bcoord_id
  } else {
    result <- bikepaths_final
  }
  return(result)
}
