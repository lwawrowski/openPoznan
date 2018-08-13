#'Paths  Function
#'
#' This function download data about Paths in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
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
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  t <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=trails")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  paths <- t$features
  
  paths_basic_info <- data.frame(ID=paths$id,
                                       Name=paths$properties$name,
                                       Lenght=paths$properties$length,
                                       Description=paths$properties$desc)

  
  # z??czenie wszystkich kolumn
  
  pathscoord <- paths$geometry$coordinates
  
  pathscoord_df <- map(pathscoord, as.data.frame)
  
  pathscoord_id <- map2_df(pathscoord_df, paths$id, ~mutate(.x, id=.y))
  
  if(Coord == T){
    reuslt <- pathscoord_id
  } else {
   result <- paths_basic_info
  }
  return(result)
}