library(XML)
library(tidyr)
library(plyr)

download.file("https://nextbike.net/maps/nextbike-official.xml?city=192", "R/bike.xml")

data <- xmlParse("R/bike.xml")

root <- xmlRoot(data)

place <- root[[1]][[1]][[1]]

li <- xmlToList(place)

place_attrs <- li$.attrs

data_table <- ldply(place_attrs, data.frame)

colnames(data_table) <- c("id","value")

data_table_final <- spread(data_table,id,value)

i <- 2

  while(i <= 109){
    
    place <- root[[1]][[1]][[i]]
    
    print(i)
    
    li <- xmlToList(place)
    
    result = tryCatch({
      
      place_attrs <- li$.attrs
      
    }
    ,error = function(w){
      i <- i +1
    }
    ,finally = {
      
      data_table <- ldply(place_attrs,data.frame)
      
      colnames(data_table) <- c("id","value")
      
      data_table <- spread(data_table,id,value)
      
      data_table_final<- data.frame(rbind.fill(data_table_final,data_table))
      i <- i + 1
    })
    }
