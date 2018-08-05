#' get_property_fs Function
#'
#' This function download data about property for sale
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import jsonlite
#' @format 
#' \describe{
#' \item{id}{factor; property id.}
#' \item{longitude}{factor; longitude.}
#' \item{latitude}{factor; latitude.}
#' \item{plot_number}{factor; plot_number.}
#' \item{plot_of_land}{factor; plot_of_land.}
#' \item{description}{factor; description.}
#' \item{comment}{factor; comments.}
#' \item{url_bip}{factor; url bip.}
#' }
#' @examples
#' get_property_fs()
get_property_fs <- function(){

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

return(property_fs_final)

}