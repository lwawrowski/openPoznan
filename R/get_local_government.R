#' Local Government Function
#'
#' This function download data about local government in Poznań.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of District.}
#' \item{AFDP}{factor; Adaptation for disabled pearson.}
#' \item{District_no}{factor; Number of district.}
#' \item{Residence}{factor; Name of Residence. }
#' }
#' @examples
#' Local_Government <- get_local_government(Coord = F)


get_local_government <- function(Coord = F){
  # dane wyborzcze samorzadowe 
  
  # wczytanie danych o wyborach samorzadowych
  
  if(havingIP() == T){
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    

      go <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_lokale_wgs/")
  
      },error = function(err) {
    
      warning("You used bad link!")
      })
    
  }else{
    
      warning("You lost connection to internet!")
  }
  
  gov <- go$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  gov_coord <- data.frame(matrix(unlist(go$features$geometry$coordinates),
                                 nrow = nrow(go$features), byrow = T))
  colnames(gov_coord)[(names(gov_coord)=="X1")] <- "V1"
  colnames(gov_coord)[(names(gov_coord)=="X2")] <- "V2"
  
  gov_basic_info <- data.frame(ID=gov$id,
                                     AFDP=gov$properties$przystosowanie,
                                     District_no=gov$properties$nr_obwodu,
                                     Residence=gov$properties$siedziba)
  
  # z??czenie wszystkich kolumn
  
  gov_final <- cbind(gov_basic_info, gov_coord)
  
  if(Coord == T){
      result <- gov_coord
    } else {
      return(gov_basic_info)
    }
  
}

