#' council_districts  Function
#'
#' This function download data about council districts in Poznan.
#' @keywords keyword
#' @export
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of the polls.}
#' \item{Address}{factor; Address of Residence.}
#' \item{Residence}{factor; Residence of council districts in Poznan.}
#' \item{Electoral_District}{factor; Electoral district in Poznan.}
#' \item{AFDP}{factor; Adaptation for disabled pearson.}
#' \item{Venue_no}{numeric; Venue of council districts in Poznan.}
#' \item{Distroct_no}{numeric; District number in Poznan.}
#' \item{Longitude}{numeric; Longitude of the polls.}
#' \item{Latitude}{numeric; Latitude of the polls.}
#' }
#' @examples
#' the polls <- council_districts()


council_districts <- function() {
  # dane wyborcze rad osiedli 
  
  # wczytanie danych rad osiedli
  
  if(have_ip() == T) {
  
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
        
      e <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_lokale/")
      
      }, error = function(err) {
    
      warning("You used bad link!")
      })
    
  }else{
    
    warning("You lost connection to internet!")
    
  }
  
    
  estate <- e$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  estate_coord <- data.frame(matrix(unlist(e$features$geometry$coordinates),
                                    nrow = nrow(e$features), byrow = T))
  colnames(estate_coord)[(names(estate_coord)=="X1")] <- "Longitude"
  colnames(estate_coord)[(names(estate_coord)=="X2")] <- "Latitude"
  
  estate_basic_info <- data.frame(ID=estate$id,
                                        Address=estate$properties$adres,
                                        Residence=estate$properties$siedziba,
                                        Electoral_District=estate$properties$okreg,
                                        AFDP=estate$properties$przystosowanie,
                                        Venue_no=estate$properties$nr_lokalu,
                                        District_no=estate$properties$nr_obwodu)
  
  # z??czenie wszystkich kolumn
  
  estate_final <- cbind(estate_basic_info, estate_coord)
  
  
  return(estate_final)
}
