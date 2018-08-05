#' get_address Function
#'
#' This function download information about address location
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import jsonlite, dplyr, ggplot2, purrr
#' @format 
#' \describe{
#' \item{id}{factor; address id.}
#' \item{longitude}{factor; longitude}
#' \item{latitude}{factor; latitude.}
#' \item{address}{factor; address name.}
#' }
#' @examples
#' get_address()

get_address <- function(){
  
search <- fromJSON('http://www.poznan.pl/featureserver/featureserver.cgi/ulice_wgs/')

search_features <- search$features

search_coord <- search_features$geometry$coordinates

search_coord_df <- map(search_coord, as.data.frame)

search_coord_id <- map2_df(search_coord_df, search$features$id, ~mutate(.x, id=.y))

ggplot(search_coord_id, aes(x= V1, y= V2, group=id), col = "red") +
  geom_path()

search_address <- paste(search_features$properties$a4,search_features$properties$a6)

search_final <- data.frame(cbind(search_features$id,
                                 search_coord_id$V1,
                                 search_coord_id$V2,
                                 search_address))

colnames(search_final)<-c("id",
                          "longitude",
                          "latitude",
                          "address")

return(search_final)

}
