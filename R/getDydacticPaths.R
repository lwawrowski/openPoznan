library(jsonlite)
library(purrr)
library(tidyverse)

# function

getDydacticPaths <- function(coords = F){
  
  d <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=dydactic_paths")
  
  dpaths <- d$features
  
  dcoord <- dpaths$geometry$coordinates
  
  dcoord_df <- map(dcoord, as.data.frame)
  
  dcoord_id <- map2_df(dcoord_df, dpaths$id, ~mutate(.x, id=.y))
  
  if(coords == T){
    result <- list(paths=dpaths[,-1], 
                   coords=dcoord_id)
  } else {
    result <- dpaths
  }
  
  return(result)
  
}

# examples

paths <- getDydacticPaths()
paths_coords <- getDydacticPaths(coords=TRUE)