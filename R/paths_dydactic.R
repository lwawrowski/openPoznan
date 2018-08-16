#' paths_dydactic  Function
#'
#' This function download data about dydactic paths  in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about dydactic pathsin Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of dydactic paths.}
#' \item{Name}{factor; Name of dydactic paths in Poznan.}
#' \item{Length}{factor; Length of dydactic paths.}
#' \item{Description}{factor; Description of dydactic paths in Poznan.}
#' }
#' @examples
#' Dydactic_Paths <- paths_dydactic(coords = F)
#' Dydactic_Paths_coord <- paths_dydactic(coords = T)


paths_dydactic <- function(coords = F){
  
  # szlaki dydaktyczne
  
  if(have_ip() == T) {
  
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
  
  dcoord_id <- data.frame(Longitude=dcoord_id$V1,
                          Latitude=dcoord_id$V2,
                          id1=dcoord_id$id1)
  
  if(coords == T){
    result <- dcoord_id
  } else {
    result <- trail_basic_info
  }
  return(result)
}
