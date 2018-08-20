#' bikes_current Function
#'
#' This function download statistic about bikes from NextBike
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import XML
#' @importFrom purrr map map2_df map_lgl
#' @importFrom dplyr mutate
#' @format 
#' \describe{
#' \item{number}{character; bike number.}
#' \item{bike_type}{character; bike type.}
#' \item{lock_types}{character; lock types.}
#' \item{id}{factor; station id.}
#' \item{uid}{factor; uid.}
#' \item{lat}{factor; latitude.}
#' \item{lng}{factor; longitude}
#' \item{name}{factor; name.}
#' \item{bikes}{factor; bikes.}
#' \item{bike_racks}{factor; bike racks.}
#' \item{free_racks}{factor; free racks.}
#' \item{bike_types}{factor; bike types.}
#' }
#' @examples
#' bikes_current()
bikes_current <-function(){
  
  download.file("https://nextbike.net/maps/nextbike-official.xml?city=192","R/bike.xml" )
  
  d <- xmlParse("R/bike.xml")
  
  root <- xmlRoot(d)
  
  places <- xmlToList(root[[1]][[1]])
  
  places$.attrs <- NULL
  
  are_bikes <- map_lgl(places, is.list)
  station_basic_info <- list()
  
  for(i in 1:length(places)){
    
    if(are_bikes[[i]]){
      
      station <- places[[i]]
      
      station_attr <- as.data.frame(t(station$.attrs))
      station$.attrs <- NULL
      
      station_bikes <- map(map(station, t), as.data.frame)
      
      station_bikes_id <- map2_df(station_bikes, station_attr$number, ~mutate(.x, id=.y))
      station_bikes_id <- station_bikes_id[ -c(7) ] 
      
      station_bikes_description <- cbind(station_bikes_id,station_attr)
      station_basic_info<- data.frame(rbind.fill(station_basic_info,station_bikes_description))
      
      

    }
 
  }
  station_final <- station_basic_info[ -c(4:5,11,15:16,18:21) ] 
  return(station_final)
}