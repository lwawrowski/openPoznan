#' historical_churches  Function
#'
#' This function download data about historical Churches in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about historical churches in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of historical churches .}
#' \item{Code}{factor; code of historical church in Poznan.}
#' \item{Name}{factor; Name of historical church.}
#' \item{Category_builiding}{factor; Category of historical church in Poznan.}
#' \item{City}{factor; City.}
#' \item{Lang}{factor; Language.}
#' \item{Address}{factor; Address of historical church.}
#' \item{ID_monument}{factor; ID monument in Poznan.}
#' \item{Description}{factor; Description of historical church.}
#' }
#' @examples
#' Church <- historical_churches(coords = F)
#' Church_coord <- historical_churches(coords = T)


historical_churches <- function(coords = F){
  
  # wczytanie danych o kosciolach
  
  if(have_ip() == T) {
  
    tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
    
    ch <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=2471")
    }, error = function(err) {
    
    warning("You used bad link!")
     })
    
  }else{
    
    warning("You lost connection to internet!")
    
  }    
    
  church <- ch$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  church_coord <- data.frame(matrix(unlist(ch$features$geometry$coordinates),
                                    nrow = nrow(ch$features), byrow = T))
  colnames(church_coord)[(names(church_coord)=="X1")] <- "Longitude"
  colnames(church_coord)[(names(church_coord)=="X2")] <- "Latitude"
  
  church_basic_info <- data.frame(ID=church$id,
                                        Code=church$properties$kod,
                                        Name=church$properties$nazwa,
                                        Category_builiding=church$properties$opis_klasy,
                                        City=church$properties$adres,
                                        Lang=church$properties$miasto,
                                        Address=church$properties$lang,
                                        ID_monument=church$properties$id_klasy,
                                        Description=church$properties$opis)

  
  # z??czenie wszystkich kolumn
  church_All <- cbind(church_basic_info,church_coord)
  
  if(coords == T){
    result <- church_All
  } else {
    result <- church_basic_info
  }
  return(result)
}
