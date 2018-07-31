library(XML)
library(tidyr)
library(plyr)
## start -> dd-mm-yyyy hh:MM where d-day m- month -y year h- hours M- minute
# example get_bike_place_details_functions("27-07-2018 13:05","28-07-2018 13:05")
get_bike_place_details_functions <- function(start,end) {
## download data about bikes from NextBike every 5 minute.
#start <- "30-07-2018 8:34"
#end <- "30-07-2018 8:44"
  
time <- difftime(end,start,units = "hours")

n <- as.numeric(time) * 12

result <- FALSE

if(Sys.time() > start){
  
  print("Incorect value")
}
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

data_table<- getNextBike(data,date)
compare_table <- compare::compareEqual(data_table,data_table2) 
if(compare_table$result  == FALSE) {
  data_table_final <- rbind(data_table_final,data_table)
  data_table2 <- data_table 
  }
}
data_table_final2 <- cbind.data.frame(data_table_final$bike_number,
                                      data_table_final$lat,
                                      data_table_final$lng,
                                   data_table_final$date)

colnames(data_table_final2)<-c("bike","lat","lng","data")

# "Wyjęcie" rowerów z listy numerów , w data o nazwie f tworzy sie historia roweru jak zmieniał swoje położenie w czasie
data_table_final2 <- separate(data_table_final2,col = "bike",c('Bike',
                                                               'Bike2',
                                                               'Bike3',
                                                               'Bike4',
                                                               'Bike5',
                                                               'Bike6',
                                                               'Bike7',
                                                               'Bike8',
                                                               'Bike9',
                                                               'Bike10',
                                                               'Bike11',
                                                               'Bike12',
                                                               'Bike13',
                                                               'Bike14',
                                                               'Bike15',
                                                               'Bike16',
                                                               'Bike17',
                                                               'Bike18',
                                                               'Bike19',
                                                               'bike20',
                                                               'bike21',
                                                               'bike22',
                                                               'bike23',
                                                               'bike24',
                                                               'bike25',
                                                               'bike26',
                                                               'bike27',
                                                               'bike28',
                                                               'bike29',
                                                               'Bike30',
                                                               'Bike31',
                                                               'Bike32',
                                                               'Bike33',
                                                               'Bike34',
                                                               'Bike35',
                                                               'Bike36',
                                                               'Bike37',
                                                               'Bike38',
                                                               'Bike39',
                                                               'Bike40',
                                                               'Bike41'
                                                               ,'Bike42'
                                                               ,'Bike43'),",",fill = "right",extra = "drop")

f <- gather(data_table_final2,value = "bikes_number",key = "bikes",na.rm = TRUE,convert = TRUE,-lng,-lat,-data)

# warunek obsluguje tylko 1 przypadek gdy nie ma roweru, w wypadku braku rowerow na 2 stacjach po sobie wyskakuje błąd
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
}
