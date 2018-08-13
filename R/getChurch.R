#' Church  Function
#'
#' This function download data about Church in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{ID}{factor; ID of church.}
#' \item{Code}{factor; code of church in Poznan.}
#' \item{Name}{factor; Name of church.}
#' \item{Category_builiding}{factor; Category of church in Poznan.}
#' \item{City}{factor; City.}
#' \item{Lang}{factor; Language.}
#' \item{Address}{factor; Address of church.}
#' \item{ID_monument}{factor; ID monument in Poznan.}
#' \item{Description}{factor; Description of church.}
#' }
#' @examples
#' Church <- getChurch(Coord = F)
#' Church_coord <- getChurch(Coord = T)


getChurch <- function(Coord = F){
  
  # wczytanie danych o kosciolach
  tryCatch({ # w przypadku baraku internetu wywoła wyjątek
  ch <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=2471")
  }, error = function(err) {
    
    print(paste(""))
    
  })
  church <- ch$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  church_coord <- data.frame(matrix(unlist(ch$features$geometry$coordinates),
                                    nrow = nrow(ch$features), byrow = T))
  colnames(church_coord)[(names(church_coord)=="X1")] <- "V1"
  colnames(church_coord)[(names(church_coord)=="X2")] <- "V2"
  
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
  
  church_final <- cbind(church_basic_info,church_coord)
  if(Coord == T){
    result <- church_coord
  } else {
    result <- church_final
  }
  return(result)
}