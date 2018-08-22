#' local_government Function
#'
#' This function download data about local government in Poznań.
#' @keywords keyword
#' @export
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of District.}
#' \item{AFDP}{factor; Adaptation for disabled pearson.}
#' \item{District_no}{numeric; Number of district.}
#' \item{Residence}{factor; Name of Residence. }
#' \item{Longitude}{numeric; Longitude of the polls.}
#' \item{Latitude}{numeric; Latitude of the polls.} 
#' }
#' @examples
#' Local_Government <- local_government()


 local_government <- function(){
  # dane wyborzcze samorzadowe 
  
  # wczytanie danych o wyborach samorzadowych
  
  if(have_ip() == T){
  
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
  colnames(gov_coord)[(names(gov_coord)=="X1")] <- "Longitude"
  colnames(gov_coord)[(names(gov_coord)=="X2")] <- "Latitude"
  
  gov_basic_info <- data.frame(ID=gov$id,
                                     AFDP=gov$properties$przystosowanie,
                                     District_no=gov$properties$nr_obwodu,
                                     Residence=gov$properties$siedziba)
  
  # z??czenie wszystkich kolumn
  
  gov_final <- cbind(gov_basic_info, gov_coord)
  
    
    
  return(gov_final)
}

