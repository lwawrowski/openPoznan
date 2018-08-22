#' sewage_works Function
#'
#' This function download data about sewage works in Poznań.
#' @keywords keyword
#' @export
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of sewage work.}
#' \item{Cesspool_Nr}{factor; Number of sewage work.}
#' \item{Cesspool_Street}{factor; Street of sewage work.}
#' \item{Cesspoll_Plot}{factor; Number of plot. }
#' \item{Longitude}{numeric; Longitude of sewage work.}
#' \item{Latitude}{numeric; Latitude of sewage work.}
#' }
#' @examples
#' 
#' sws <- sewage_works()
#' 
#' 
#' 


sewage_works <- function() {

  #Wstepna analiza
  
  if(have_ip() == T) {
    
    tryCatch({
      
  SW_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=environment&co=omnc02")
   
   },error = function(e) {
      warning("You used bad link or didn't load jsonlite package")
    })
    
  } else {
    warning("You lost connection to internet!")
  }
  
  SW_feautures <- SW_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  SW_coord <- data.frame(matrix(unlist(SW_feautures$geometry$coordinates),
                                nrow = nrow(SW_feautures), 
                                byrow = T))
  
  colnames(SW_coord) <- c("Longitude", "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  SW_basic_info <- data.frame(cbind(SW_feautures$id,
                                    SW_feautures$properties$no,
                                    SW_feautures$properties$street,
                                    SW_feautures$properties$dzialka))
  
  colnames(SW_basic_info) <- c("ID",
                               "SW_Nr",
                               "SW_Street",
                               "SW_Plot")
  
  # Ostateczne polaczenie 
  
  SW_final <- cbind (SW_basic_info,
                     SW_coord)
  
return (SW_final)

}