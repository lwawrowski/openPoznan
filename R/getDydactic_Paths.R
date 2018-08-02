library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getDydactic_Paths <- function(Coord = F){
  
  # szlaki dydaktyczne
  
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  d <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=dydactic_paths")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  dpaths <- d$features
  
  trail_basic_info <- data.frame(cbind(dpaths$id,
                                       dpaths$properties$name,
                                       dpaths$properties$length,
                                       dpaths$properties$desc))
  
  colnames(trail_basic_info)<-c("ID","Name","Lenght","Description")
  
  # z??czenie wszystkich kolumn
  
  dpaths <- d$features
  
  dcoord <- dpaths$geometry$coordinates
  
  dcoord_df <- map(dcoord, as.data.frame)
  
  dcoord_id <- map2_df(dcoord_df, dpaths$id, ~mutate(.x, id=.y))
  if(Coord == T){
    reuslt <- dcoord_id
  } else {
    return(trail_basic_info)
  }
}