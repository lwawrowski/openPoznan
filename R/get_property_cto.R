#' get_property_cto Function
#'
#' This function download data about property current trade offer
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
#' get_property_cto()
get_property_cto <- function(){

property_cto <- fromJSON('http://www.poznan.pl/mim/plan/map_service.html?mtype=properties_for_sale&co=wgn&type=4')

property_cto_features <- property_cto$features

property_cto_coord <- data.frame(matrix(unlist(property_cto_features$geometry$coordinates),
                                        nrow = nrow(property_cto_features), byrow = T))

property_cto_final <- data.frame(id= property_cto_features$id,
                                 longitude = property_cto_coord$X1,
                                 latitude = property_cto_coord$X2,
                                 plot_number = property_cto_features$properties$dzialka,
                                 plot_of_land = property_cto_features$properties$pole,
                                 description = property_cto_features$properties$opis,
                                 comment = property_cto_features$properties$komentarz,
                                 url_bip = property_cto_features$properties$url_bip)


return(property_cto_final)



}
