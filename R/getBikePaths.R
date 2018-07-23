getBikePaths <- function(Coord = F){
  # szlaki rowerowe 
  
  b <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=cycling_routes")
  bikepaths <- b$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  bcoord <- bikepaths$geometry$coordinates
  
  bcoord_df <- map(bcoord, as.data.frame)
  
  bcoord_id <- map2_df(bcoord_df, bikepaths$id, ~mutate(.x, id=.y))
  
  bikepaths_basic_info <- data.frame(cbind(bikepaths$id,
                                           bikepaths$properties$name,
                                           bikepaths$properties$length,
                                           bikepaths$properties$desc))
  
  colnames(bikepaths_basic_info)<-c("ID","Name","Lenght","Description")
  
  # z??czenie wszystkich kolumn
  
  bikepaths_final <- cbind(bikepaths_basic_info)
  
  if(Coord == T){
    reuslt <- bcoord_id
  } else {
    return(bikepaths_final)
  }
}