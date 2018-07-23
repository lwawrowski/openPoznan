# trakt 
  tr <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=tourism&co=trakt")
  tract <- tr$features$geometry
  
  tractcoord <- tract$coordinates
  
  tractcoord2d <- map(tractcoord, drop)
  
  tractcoord_df <- map(tractcoord2d, as.data.frame)
  
  tractcoord_id <- map2_df(tractcoord_df, tr$features$id, ~mutate(.x, id=.y))
  
  
  ggplot(tractcoord_id, aes(x=V1 , y= V2, group_by(id))) +
    geom_path(aes(colour=as.factor(id)))
  