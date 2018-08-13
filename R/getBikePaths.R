#' Bike Paths Function
#'
#' This function download data about Bike Paths in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
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

getBike_Paths <- function(Coord = F){
  
  # szlaki rowerowe 
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  b <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=cycling_routes")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  bikepaths <- b$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  bcoord <- bikepaths$geometry$coordinates
  
  bcoord_df <- map(bcoord, as.data.frame)
  
  bcoord_id <- map2_df(bcoord_df, bikepaths$id, ~mutate(.x, id=.y))
  
  bikepaths_basic_info <- data.frame(ID=bikepaths$id,
                                           Name=bikepaths$properties$name,
                                           Lenght=bikepaths$properties$length,
                                           Description=bikepaths$properties$desc)

  
  # z??czenie wszystkich kolumn
  
  bikepaths_final <- cbind(bikepaths_basic_info)
  
  if(Coord == T){
    reuslt <- bcoord_id
  } else {
    result <- bikepaths_final
  }
  return(result)
}
