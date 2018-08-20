#' addresses Function
#'
#' This function download information about address location
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom dplyr mutate left_join
#' @importFrom purrr map map2_df
#' @format 
#' \describe{
#' \item{longitude}{numeric; longitude}
#' \item{latitude}{numeric; latitude.}
#' \item{id}{factor; address id.}
#' \item{order}{numeric; order.}
#' \item{address}{factor; address name.}
#' }
#' @examples
#' addresses()

addresses <- function(){
  
search <- fromJSON('http://www.poznan.pl/featureserver/featureserver.cgi/ulice_wgs/')

search_features <- search$features

search_coord <- search_features$geometry$coordinates

search_coord_df <- map(search_coord, as.data.frame)

search_coord_id <- map2_df(search_coord_df, search$features$id, ~mutate(.x, id=.y))

search_coord_id$order <- 1:nrow(search_coord_id)

search_coord_id$id <- as.factor(search_coord_id$id)

search_address <- data.frame(Address=paste(search_features$properties$a4,search_features$properties$a6),
                             id = as.factor(search_features$id))

search_final <- left_join(search_coord_id, search_address, by ="id")

colnames(search_final)<-c("longitude",
                          "latitude",
                          "id",
                          "order",
                          "address")

return(search_final)

}