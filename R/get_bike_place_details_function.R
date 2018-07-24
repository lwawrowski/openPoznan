library(XML)
library(tidyr)
library(plyr)

get_bike_place_details_functions <- function(coords = F) {
## download data about bikes from NextBike.
for (i in 1:480){
x <- format(Sys.time(),"%d-%m-%Y-%H-%M")
download.file("https://nextbike.net/maps/nextbike-official.xml?city=192",paste0("R/NextBike/bike-",x,".xml") )
Sys.sleep(300)

}
data_table_final <- list()
data_table2 <- list()

list.xml <- list.files(path = "R/NextBike/", pattern = ".xml")

for(i in 1:length(list.xml)){
   
date <- substring(list.xml[i],6,21)

data<-xmlParse(paste0("R/NextBike/",list.xml[i]))

data_table<- getNextBike(data,date)
compare_table <- compare::compareEqual(data_table,data_table2) 
if(compare_table$result  == FALSE) {
  data_table_final <- rbind(data_table_final,data_table)
  data_table2 <- data_table 
  }
}



getNextBike <- function(data,date){

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
data_table

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
data_table_final <- data_table_final[ -c(10:13, 15) ] 
data_table_final <- cbind(data_table_final,date)
return(data_table_final)
}



if(arg == TRUE){
  
  data_table_final <- TRUE
  
} else {
  
  data_table_final <- FALSE
}

return(data_table_final)


}
