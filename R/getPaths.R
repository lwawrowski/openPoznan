

library(jsonlite)
library(ggplot2)
library(dplyr)
library(purrr)

getPaths <- function(Coord = F){
  # szlaki turystyczne 
  
  t <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=trails")
  paths <- t$features
  
  paths_basic_info <- data.frame(cbind(paths$id,
                                       paths$properties$name,
                                       paths$properties$length,
                                       paths$properties$desc))
  
  colnames(paths_basic_info)<-c("ID","Name","Lenght","Description")
  
  # z??czenie wszystkich kolumn
  
  pathscoord <- paths$geometry$coordinates
  
  pathscoord_df <- map(pathscoord, as.data.frame)
  
  pathscoord_id <- map2_df(pathscoord_df, paths$id, ~mutate(.x, id=.y))
  
  if(Coord == T){
    reuslt <- pathscoord_id
  } else {
    return(paths_basic_info)
  }
}