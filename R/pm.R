#' pm Function
#'
#' This function download data about parking machines in Poznań
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of PM.}
#' \item{PM_Zone}{factor; Zone in which PM is placed.}
#' \item{PM_Street}{factor; Street in which PM is placed.}
#' \item{Longitude}{numeric; Longitude of PM.}
#' \item{Latitude}{numeric; Latitude of PM.}
#' }
#' @examples
#' 
#' pms <- pm()
#' 
#' 
#' 


pm <- function () {

  #Wstepna analiza
  
  if(havingIP() == T) {
    
    tryCatch({
  
  PM_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=parking_meters")
    
  },error = function(e) {
      warning("You used bad link!")
    })
    
  } else {
    warning("You lost connection to internet!")
  }
  
  PM_features <- PM_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  PM_coord <- data.frame(matrix(unlist(PM_features$geometry$coordinates),
                                nrow = nrow(PM_features), 
                                byrow = T))
  
  colnames(PM_coord)<- c("Longitude",
                         "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  PM_basic_info <- data.frame (cbind(PM_features$id, PM_features$properties$zone, PM_features$properties$street))
  
  colnames(PM_basic_info) <- c("ID",
                               "PM_Zone", 
                               "PM_Street")
  # Ostateczne polaczenie 
  
  PM_final <- cbind(PM_basic_info,PM_coord)
  
return(PM_final)
}

