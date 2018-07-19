library(jsonlite)

get_property_fs <- function(coords = F){

property_fs<- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=properties_for_sale&co=wgn&type=3')

property_fs_features <- property_fs$features

property_fs_coord <- data.frame(matrix(unlist(property_fs_features$geometry$coordinates),
                                       nrow = nrow(property_fs_features), byrow = T))

property_fs_final <- data.frame(cbind(property_fs_features$id,
                                      property_fs_coord$X1,
                                      property_fs_coord$X2,
                                      property_fs_features$properties$dzialka,
                                      property_fs_features$properties$pole,
                                      property_fs_features$properties$opis,
                                      property_fs_features$properties$komentarz,
                                      property_fs_features$properties$url_bip))

colnames(property_fs_final)<-c("id", 
                                "longitude",
                                "latitude",
                                "plot_number",
                                "plot_of_land",
                                "description",
                                "comment",
                                "url_bip")

result <- property_fs_final

if(arg == TRUE){
  
  result <- TRUE
  
} else {
  
  result <- FALSE
}

return(result)

}