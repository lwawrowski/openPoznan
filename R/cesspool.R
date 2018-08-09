#' cesspool Function
#'
#' This function download data about cesspools in Poznań.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import jsonlite, textclean, tidyverse
#' @format 
#' \describe{
#' \item{ID}{factor; ID of cesspool.}
#' \item{Cesspool_Nr}{factor; Number of cesspool.}
#' \item{Cesspool_Street}{factor; Street of cesspool.}
#' \item{Cesspoll_Plot}{factor; Number of plot. }
#' \item{Longitude}{numeric; Longitude of cesspool.}
#' \item{Latitude}{numeric; Latitude of cesspool.}
#' }
#' @examples
#' Cesspools <- cesspool()

cesspool <- function() {

  #Wstepna analiza
  
  if(havingIP() == T) {
    
    tryCatch({
  
  Cesspool_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=environment&co=omnc01%20")
  
    },error = function(e) {
      warning("You used bad link!")
    })
    
  } else {
    warning("You lost connection to internet!")
  }
  
  Cesspool_feautures <- Cesspool_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  Cesspool_coord <- data.frame(matrix(unlist(Cesspool_feautures$geometry$coordinates),
                                      nrow = nrow(Cesspool_feautures), 
                                      byrow = T))
  
  colnames(Cesspool_coord) <- c("Longitude",
                                "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Cesspool_basic_info <- data.frame(cbind(Cesspool_feautures$id,
                                          Cesspool_feautures$properties$no,
                                          Cesspool_feautures$properties$street,
                                          Cesspool_feautures$properties$dzialka))
  
  colnames(Cesspool_basic_info) <- c("ID",
                                     "Cesspool_Nr",
                                     "Cesspool_Street",
                                     "Cesspool_Plot")
  
  # Ostateczne polaczenie 
  
  Cesspool_Final <- cbind(Cesspool_basic_info,Cesspool_coord)

return (Cesspool_Final)

}
