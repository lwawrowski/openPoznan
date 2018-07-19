library(jsonlite)

property_cto <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=properties_for_sale&co=wgn&type=4')

property_cto_features <- property_cto$features

property_cto_coord <- data.frame(matrix(unlist(property_cto_features$geometry$coordinates),
                                        nrow = nrow(property_cto_features), byrow = T))

property_cto_final <- data.frame(cbind(property_cto_features$id,
                                       property_cto_coord$X1,
                                       property_cto_coord$X2,
                                       property_cto_features$properties$dzialka,
                                       property_cto_features$properties$pole,
                                       property_cto_features$properties$opis,
                                       property_cto_features$properties$komentarz,
                                       property_cto_features$properties$url_bip))

colnames(property_cto_final)<-c("id", 
                          "longitude",
                          "latitude",
                          "plot_number",
                          "plot_of_land",
                          "description",
                          "comment",
                          "url_bip")


