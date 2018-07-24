library(XML)
library(tidyr)
library(plyr)

get_bike_place <- function(coords = F) {

download.file("https://nextbike.net/maps/nextbike-official.xml?city=192", "R/bike.xml")

data <- xmlParse("R/bike.xml")

root <- xmlRoot(data)

place <- root[[1]][[1]][[1]]

li <- xmlToList(place)

n <- 2


result <- tryCatch(
  {
  
  place_attrs <- li$.attrs

}
,error = function(w){
  
  place <- root[[1]][[1]][[n]]
  
  li <- xmlToList(place)
  
  n <- n + 1
  
  place_attrs <- li$.attrs
  
}
,finally = {
  
  data_table <- ldply(place_attrs, data.frame)
  
  colnames(data_table) <- c("id","value")
  
  data_table_final <- spread(data_table,id,value)
})

i <- n

  while(i <= 109) {
    
    place <- root[[1]][[1]][[i]]
    
    li <- xmlToList(place)
    
    result <- tryCatch({
      
      place_attrs <- li$.attrs
      
    }
    ,error = function(w){
      
      i <- i +1
      
      place <- root[[1]][[1]][[i]]
      
      li <- xmlToList(place)
      
      place_attrs <- li$.attrs
    }
    ,finally = {
      
      data_table <- ldply(place_attrs,data.frame)

    })
    colnames(data_table) <- c("id","value")
    
    
    data_table <- spread(data_table,id,value)
    
    data_table_final<- data.frame(rbind.fill(data_table_final,data_table))
    
    i <- i + 1
    
    data_table_final <- unique(data_table_final)
    
  }

if(arg == TRUE){
  
  data_table_final <- TRUE
  
} else {
  
  data_table_final <- FALSE
}

return(data_table_final)


