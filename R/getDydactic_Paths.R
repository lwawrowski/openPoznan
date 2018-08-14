#'Dydactic Paths  Function
#'
#' This function download data about Dydactic paths  in Poznan.
#' @keywords keyword
#' @export
#' @param Coord show basic_data about Dydactic pathsin Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{factor; ID of Dydactic paths.}
#' \item{Name}{factor; Name of Dydactic paths in Poznan.}
#' \item{Length}{factor; Length of Dydactic paths.}
#' \item{Description}{factor; Description of Dydactic paths in Poznan.}
#' }
#' @examples
#' Dydactic_Paths <- getDydactic_Paths(Coord = F)
#' Dydactic_Paths_coord <- getDydactic_Paths(Coord = T)


getDydactic_Paths <- function(Coord = F){
  
  # szlaki dydaktyczne
  
  if(havingIP() == T) {
  
    tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
    d <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=dydactic_paths")
    }, error = function(err) {
    
    warning("You used bad link!")
    })
    
  }else{
    
    warning("You lost connection to internet!")
    
  }    
    
  dpaths <- d$features
  
  trail_basic_info <- data.frame(ID=dpaths$id,
                                       Name=dpaths$properties$name,
                                       Length=dpaths$properties$length,
                                       Description=dpaths$properties$desc)
  
  
  # z??czenie wszystkich kolumn
  
  dpaths <- d$features
  
  dcoord <- dpaths$geometry$coordinates
  
  dcoord_df <- map(dcoord, as.data.frame)
  
  dcoord_id <- map2_df(dcoord_df, trail_basic_info$Name, ~mutate(.x, id1=.y))
  
  if(Coord == T){
    result <- dcoord_id
  } else {
    result <- trail_basic_info
  }
  return(result)
}
