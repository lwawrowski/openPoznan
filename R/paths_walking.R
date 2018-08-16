#'paths_walking  Function
#'
#' This function download data about paths walking in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about paths walking in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of Paths in Poznan.}
#' \item{Name}{factor; Name of Paths in Poznan.}
#' \item{Lenght{factor; Lenght.}
#' \item{Description}{factor; Description Paths in Poznan.}
#' }
#' @examples
#' Paths <- paths_walking(coords = F)
#' Paths_coord <- paths_walking(coords = T)


paths_walking <- function(coords = F){
  # szlaki turystyczne 
  
    if(have_ip() == T) {
  
  
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
  
  pathscoord_id <- data.frame(Longitude=pathscoord_id$V1,
                              Latitude=pathscoord_id$V2,
                              id3=pathscoord_id$id3)
  
  if(coords == T){
    result <- pathscoord_id
  } else {
   result <- paths_basic_info
  }
  return(result)
}