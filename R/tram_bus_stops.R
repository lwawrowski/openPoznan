#' tram_bus_stops Function
#'
#' This function download data about stops in Poznań
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @import curl
#' @format 
#' \describe{
#' \item{ID}{factor; ID of stop.}
#' \item{Stop_Zone}{factor; Zone in which stop is placed.}
#' \item{Route_Type}{factor; Shows route type on which stop is placed.}
#' \item{Stop_Headsign}{factor; Shows which trams and buses departure from stop.}
#' \item{Stop_Name}{factor; Name of stop.}
#' \item{Longitude}{numeric; Longitude of stop.}
#' \item{Latitude}{numeric; Latitude of stop.}
#' }
#' @examples
#' 
#' Stops <- tram_bus_stops() 
#' 
#' 

tram_bus_stops <- function () {

  #Przystanki ZTM calosc
  
  #Wstepna analiza
  
  if(have_ip() == T) {
    
    tryCatch({
  
  Stops_blank <- fromJSON ("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=cluster")
  
    },error = function(e) {
      warning("You used bad link!")
    })
    
  } else {
    warning("You lost connection to internet!")
  }
  
  Stops_features <- Stops_blank$features
  
  # Utworzenie koordynatow + nazwanie
  
  Stops_coord <- data.frame(matrix(unlist(Stops_features$geometry$coordinates),
                                   nrow = nrow(Stops_features), byrow = T))
  
  colnames (Stops_coord) <- c("Longitude",
                              "Latitude")
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  Stops_basic_info <- data.frame(cbind(Stops_features$id,
                                       Stops_features$properties$zone,
                                       Stops_features$properties$route_type,
                                       Stops_features$properties$headsigns,
                                       Stops_features$properties$stop_name))
  
  colnames (Stops_basic_info) <- c("ID",
                                   "Stop_Zone",
                                   "Route_Type",
                                   "Stop_Headsigns",
                                   "Stop_Name")
  
  # Ostateczne polaczenie 
  
  Stops_final <- cbind(Stops_basic_info,Stops_coord)
  
return(Stops_final)

}
