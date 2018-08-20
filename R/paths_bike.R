#' paths_bike Function
#'
#' This function download data about bike Paths in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about bike paths in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of all bike paths in Poznan.}
#' \item{Name}{character; Name of bike paths.}
#' \item{Lenght}{character; Lenght of bike paths}
#' \item{Description}{character; Description of bike paths}
#' }
#' @examples
#' Bike_Paths <- paths_bike(coords = F)
#' Area_Electoral_coords <- paths_bike(coords = T)

paths_bike <- function(coords = F){
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

  bcoord_id <- data.frame(Longitude=bcoord_id$V1,
                          Latitude=bcoord_id$V2,
                          id2=bcoord_id$id2)
  # z??czenie wszystkich kolumn
  
  
  
  if(coords == T){
    result <- bcoord_id
  } else {
    return(bikepaths_basic_info)
  }
}
