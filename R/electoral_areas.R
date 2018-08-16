#' electoral_areas Function
#'
#' This function download data about electoral areas in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about electoral areas in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of area electoral.}
#' \item{District_no}{numeric; Number of district.}
#' }
#' @examples
#' Area_Electoral <- electoral_areas(coords = F)
#' Area_Electoral_coords <- electoral_areas(coords = T)


electoral_areas <- function(coords = F){
  # dane wybory samorzadowe obwody wyborcze 
  
  # wczytanie danych o wyborach samorzadowych
  
  if(have_ip() == T){
  
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
                                         District_no=oblast2$properties$nr_obwodu)
  
  oblast2coord_id <- data.frame(Longitude=oblast2coord_id$V1,
                                Latitude=oblast2coord_id$V2,
                                id=oblast2coord_id$id)
  
  # z??czenie wszystkich kolumn
  
  if(coords == T){
     result <- oblast2coord_id
    } else {
      result <- oblast2_basic_info
    }
  return(result)
}
