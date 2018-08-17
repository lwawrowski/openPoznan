#' monuments  Function
#'
#' This function download data about monuments in Poznan.
#' @keywords keyword
#' @export
#' @param coords show basic data about monuments in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{numeric; ID of monument}
#' \item{Code}{factor; code of monument in Poznan.}
#' \item{Name}{factor; Name of monument}
#' \item{Category_builiding}{factor; Category of monument in Poznan.}
#' \item{City}{factor; City.}
#' \item{Lang}{factor; Language.}
#' \item{Address}{factor; Address of monument}
#' \item{ID_monument}{factor; ID monument in Poznan.}
#' \item{Description}{factor; Description of monument}
#' }
#' @examples
#' relikt. <- monuments(coords = F)
#' relikt._coord <- monuments(coords = T)

monuments <- function(coords = F){
  # turystyka 
  # zabytki
  
  # wczytanie danych o zabytkach
  
  if(have_ip() == T) {
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
        
      r <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=2572")
      
      }, error = function(err) {
      
      warning("You used bad link!")
    })
    
  }else{
    
      warning("You lost connection to internet!")
    
  }    
   
  relikt <- r$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  relikt_coord <- data.frame(matrix(unlist(r$features$geometry$coordinates),
                                    nrow = nrow(r$features), byrow = T))
  colnames(relikt_coord)[(names(relikt_coord)=="X1")] <- "Longitude"
  colnames(relikt_coord)[(names(relikt_coord)=="X2")] <- "Latitude"
  
  
  relikt_basic_info <- data.frame(ID=relikt$id,
                                        Code=relikt$properties$kod,
                                        Name=relikt$properties$nazwa,
                                        CAtegory_builiding=relikt$properties$opis_klasy,
                                        City=relikt$properties$miasto,
                                        Lang=relikt$properties$lang,
                                        Address=relikt$properties$adres,
                                        ID_monument=relikt$properties$id_klasy,
                                        Description=relikt$properties$opis)

  
  # z??czenie wszystkich kolumn
  relikt_coord_all <- cbind(relikt_coord, relikt_basic_info)
  
  if(coords == T){
    result <- relikt_coord_all
  } else {
    result <- relikt_basic_info
  }
  return(result)
}
