
getCircle <- function(Coord = F){
  # okragi wyborow rad osiedli
  
  # wczytanie danych rad osiedli
  
  co <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_okregi/")
  circle <- co$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  circle_basic_info <- data.frame(cbind(circle$id,
                                        circle$properties$okreg))
  colnames(circle_basic_info)<-c("ID","Name")
  
  # z??czenie wszystkich kolumn
  
  circle_final <- cbind(circle_basic_info)
  
  circlecoord <- circle$geometry$coordinates
  
  circlecoord2d <- map(circlecoord, drop)
  
  circlecoord_df <- map(circlecoord2d, as.data.frame)
  
  circlecoord_id <- map2_df(circlecoord_df, circle$id, ~mutate(.x, id=.y))
  
  if(Coord == F){
    result <- circlecoord_id
  } else {
    return(circle_final)  
  }  
 
}  