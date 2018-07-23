library(XML)
library(tidyr)
library(plyr)
download.file("https://nextbike.net/maps/nextbike-official.xml?city=192", "R/bike.xml")
data <- xmlParse("R/bike.xml")

root <- xmlRoot(data)
print(root[[1]][[1]][[1]])
place <- root[[1]][[1]][[1]]

li <- xmlToList(place)

test <- li$.attrs
data_table <- ldply(test, data.frame)
colnames(data_table) <- c("id","value")

data_table_final <- spread(data_table,id,value)

i <- 2
  while(i <= 109){
    place <- root[[1]][[1]][[i]]
    print(i)
    li <- xmlToList(place)
    result = tryCatch({
      test <- li$.attrs
    },error = function(w){
      i <- i +1
    },finally = {
      
      data_table <- ldply(test,data.frame)
      colnames(data_table) <- c("id","value")
      data_table <- spread(data_table,id,value)
      
      data_table_final<- data.frame(rbind.fill(data_table_final,data_table))
      i <- i + 1
    })
    }


library(tidyr)

data_table2 <- spread(data_table2,id,value)
colnames(data_table) <- c("id",
                           "value")
data_table1 <- spread(data_table,id,value)





xml <- xmlTreeParse("R/bike.xml",useInternalNode=TRUE)
ns <- getNodeSet(xml, '//place')
rbindlist(lapply(ns, function(x) {
  event <- xmlValue(x)
  data.frame(place, t(xpathSApply(x, ".//bike", xmlAttrs)))
}), fill=TRUE)
