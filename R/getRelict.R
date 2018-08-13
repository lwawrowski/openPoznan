#' Relict  Function
#'
#' This function download data about Relict in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of relikt.}
#' \item{Code}{factor; code of relikt. in Poznan.}
#' \item{Name}{factor; Name of relikt.}
#' \item{Category_builiding}{factor; Category of relikt. in Poznan.}
#' \item{City}{factor; City.}
#' \item{Lang}{factor; Language.}
#' \item{Address}{factor; Address of relikt.}
#' \item{ID_monument}{factor; ID monument in Poznan.}
#' \item{Description}{factor; Description of relikt.}
#' }
#' @examples
#' relikt. <- getRelict(Coord = F)
#' relikt._coord <- getRelict(Coord = T)

getRelict <- function(Coord = F){
  # turystyka 
  # zabytki
  
  # wczytanie danych o zabytkach
  
  if(havingIP() == T) {
  
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
  colnames(relikt_coord)[(names(relikt_coord)=="X1")] <- "V1"
  colnames(relikt_coord)[(names(relikt_coord)=="X2")] <- "V2"
  
  
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
  
  relikt_final <- cbind(relikt_basic_info,relikt_coord)
  if(Coord == T){
    result <- relikt_coord
  } else {
    resut <- relikt_final
  }
  return(result)
}