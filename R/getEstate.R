#'Estate  Function
#'
#' This function download data about Estate in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of  Estate.}
#' \item{Type}{factor; Type of Estate in Poznan.}
#' \item{Address}{factor; Address of Residence.}
#' \item{Residence}{factor; Residence of Estate in Poznan.}
#' \item{Electoral_District}{factor; Electoral District in Poznan.}
#' \item{AFDP}{factor; Adaptation for disabled pearson.}
#' \item{Venue_no}{factor; Venue of Estate in Poznan.}
#' \item{Distroct_no}{factor; District number in Poznan.}
#' }
#' @examples
#' Estate <- getEstate(Coord = F)
#' Estate_coord <- getEstate(Coord = T)

getEstate <- function(Coord = F) {
  # dane wyborcze rad osiedli 
  
  # wczytanie danych rad osiedli
  
  if(havingIP() == T) {
  
  
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
  colnames(estate_coord)[(names(estate_coord)=="X1")] <- "V1"
  colnames(estate_coord)[(names(estate_coord)=="X2")] <- "V2"
  
  estate_basic_info <- data.frame(ID=estate$id,
                                        Type=estate$type,
                                        Address=estate$properties$adres,
                                        Residence=estate$properties$siedziba,
                                        Electoral_District=estate$properties$okreg,
                                        AFDP=estate$properties$przystosowanie,
                                        Venue_no=estate$properties$nr_lokalu,
                                        District_no=estate$properties$nr_obwodu)
  
  # z??czenie wszystkich kolumn
  
  estate_final <- cbind(estate_basic_info, estate_coord)
  
  if(Coord == T){
    result <- estate_coord
  } else {
    result <- estate_basic_info
  }
  return(result)
}