#' bikes_period Function
#'
#' This function download statistic about bikes from NextBike
#' @param start the beginning of data downloading
#' @param end the end of data downloading
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import XML
#' @importFrom purrr map map2_df map_lgl
#' @importFrom dplyr mutate id
#' @importFrom tidyr fill
#' @format 
#' \describe{
#' \item{number}{character; bike number.}
#' \item{bike_type}{character; bike type.}
#' \item{lock_types}{character; lock types.}
#' \item{id}{factor; station id.}
#' \item{uid}{factor; uid.}
#' \item{lat}{factor; latitude.}
#' \item{lng}{factor; longitude.}
#' \item{name}{factor; name.}
#' \item{bikes}{factor; bikes.}
#' \item{bike_racks}{factor; bike racks.}
#' \item{free_racks}{factor; free racks.}
#' \item{bike_types}{factor; bike types.}
#' \item{date}{factor; date.}
#' }
#' @examples
#' bikes_period("27-07-2018 13:05","28-07-2018 13:05")

## start -> dd-mm-yyyy hh:MM where d-day m- month -y year h- hours M- minute
# example get_bike_place_details_functions("27-07-2018 13:05","28-07-2018 13:05")
bikes_period <- function(start,end) {
## download data about bikes from NextBike every 5 minute.
#start <- "30-07-2018 8:34"
#end <- "30-08-2018 8:44"
  
time <- difftime(end,start,units = "hours")

n <- as.numeric(time) * 12

result <- FALSE

if(n < 0){
  
  Print("Incorrect value")
}
else{
  
if(result == FALSE){
  
 while(result == FALSE){
   
    x <- format(Sys.time(),"%d-%m-%Y %H:%M")
    
    result <- start == x
    
    Sys.sleep(10)
  }
}
if(result == TRUE){
  
for (i in 1:n){
  
x <- format(Sys.time(),"%d-%m-%Y-%H-%M")

download.file("https://nextbike.net/maps/nextbike-official.xml?city=192",paste0("R/NextBike/bike-",x,".xml") )
Sys.sleep(300)

}
}
data_table_final <- list()
data_table2 <- list()

list.xml <- list.files(path = "R/NextBike/", pattern = ".xml")

for(i in 1:length(list.xml)){
date <- substring(list.xml[i],6,21)

data<-xmlParse(paste0("R/NextBike/",list.xml[i]))

data_table<- get_nextbikes(data,date)
compare_table <- compare::compareEqual(data_table,data_table2) 


if(compare_table$result  == FALSE) {
  data_table_final <- rbind(data_table_final,data_table)
  data_table2 <- data_table 
  }
}
get_nextbikes <-function(data,date){

  
  
  root <- xmlRoot(data)
  
  places <- xmlToList(root[[1]][[1]])
  
  # usunięcie attrs
  
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

      station_bikes_description <- cbind(station_bikes_id,station_attr)
      station_basic_info<- data.frame(rbind.fill(station_basic_info,station_bikes_description))
      
      
      
    }
    
  }
  station_final <- station_basic_info[ -c(4:5,11,15:16,18:21) ] 
  station_final <- cbind(station_final,date)
}




return(data_table_final)


}
}
