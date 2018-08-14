#'Paths  Function
#'
#' This function download data about trail in Poznan.
#' @keywords keyword
#' @export
#' @param Coord show basic_data about trail in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{factor; ID of Paths in Poznan.}
#' \item{Name}{factor; Name of Paths in Poznan.}
#' \item{Lenght{factor; Lenght.}
#' \item{Description}{factor; Description Paths in Poznan.}
#' }
#' @examples
#' Paths <- getPaths(Coord = F)
#' Paths_coord <- getPaths(Coord = T)


getPaths <- function(Coord = F){
  # szlaki turystyczne 
  
  if(havingIP() == T) {
  
  
     tryCatch({ # w przypadku baraku internetu wywoła wyjątek
     t <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=trails")
     }, error = function(err) {
    
       warning("You used bad link!")
     })
    
  }else{
    
    warning("You lost connection to internet!")
    
  }    
  paths <- t$features
  
  paths_basic_info <- data.frame(ID=paths$id,
                                       Name=paths$properties$name,
                                       Lenght=paths$properties$length,
                                       Description=paths$properties$desc)

  
  # z??czenie wszystkich kolumn
  
  pathscoord <- paths$geometry$coordinates
  
  pathscoord_df <- map(pathscoord, as.data.frame)
  
  pathscoord_id <- map2_df(pathscoord_df, paths$properties$name, ~mutate(.x, id3=.y))
  
  if(Coord == T){
    result <- pathscoord_id
  } else {
   result <- paths_basic_info
  }
  return(result)
}