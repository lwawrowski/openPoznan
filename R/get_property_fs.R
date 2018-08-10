#' get_property_fs Function
#'
#' This function download data about property for sale
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{id}{factor; property id.}
#' \item{longitude}{numeric; longitude.}
#' \item{latitude}{numeric; latitude.}
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

property_fs_final <- data.frame(id = property_fs_features$id,
                                longitude =  property_fs_coord$X1,
                                latitude = property_fs_coord$X2,
                                 plot_number = property_fs_features$properties$dzialka,
                                 plot_of_land = property_fs_features$properties$pole,
                                 description = property_fs_features$properties$opis,
                                 comment = property_fs_features$properties$komentarz,
                                 url_bip = property_fs_features$properties$url_bip)


return(property_fs_final)

}