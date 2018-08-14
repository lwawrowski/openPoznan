#' Ticket  Function
#'
#' This function download data about Ticket in Poznan.
#' @keywords keyword
#' @export
#' @param Coord show basic_data about Ticket in Poznań
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @format 
#' \describe{
#' \item{Opening_Hours}{factor; Hours of open.}
#' \item{Sales_form}{factor; form of sale.}
#' \item{Ticket}{factor; kind of Ticket.}
#' \item{Name}{factor; Name.}
#' \item{Address}{factor; Address of shpo.}
#' \item{City}{factor;City.}
#' \item{Lang}{factor; Language.}
#' \item{PEKKA_Card}{factor; Card.}
#' \item{ID_Class}{factor; ID of Class.}
#' \item{Description}{factor; Decsription.}
#' \item{POS}{factor; Description Class.}
#' }
#' @examples
#' Ticket <- getTicket(Coord = F)
#' Ticket_coord <- getTicket(Coord = T)

getTicket <- function(Coord = F) {
  # sprzeda biletow
  
  # wczytanie danych biletow 
  
  if(havingIP() == T) {
    
  
  
      tryCatch({ # w przypadku baraku internetu wywoła wyjątek
    
      tick <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=4803")
      }, error = function(err) {
        
      warning("You used bad link!")
        
      })
    
  }else{
    
      warning("You lost connection to internet!")
    
  }    
   
  ticket <- tick$features 
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  ticket_coord <- data.frame(matrix(unlist(tick$features$geometry$coordinates),
                                    nrow = nrow(tick$features), byrow = T))
  colnames(ticket_coord)[(names(ticket_coord)=="X1")] <- "V1"
  colnames(ticket_coord)[(names(ticket_coord)=="X2")] <- "V2"
  
  
  ticket_basic_info <- data.frame(Opening_Hours=ticket$properties$y_4308_godziny_otwar,
                                        Opening_Hours=ticket$properties$y_4310_godziny_otwar,
                                        Opening_Hours=ticket$properties$y_4309_godziny_otwar,
                                        Sales_form=ticket$properties$y_4311_forma_sprzeda,
                                        Ticket=ticket$properties$y_4326_bilety_jednor,
                                        Name=ticket$properties$nazwa,
                                        Address=ticket$properties$adres,
                                        City=ticket$properties$miasto,
                                        Lang=ticket$properties$lang,
                                        PEKA_Card=ticket$properties$y_4327_peka__karta_n,
                                        ID_Class=ticket$properties$id_klasy,
                                        Description=ticket$properties$opis,
                                        POS=ticket$properties$opis_klasy)
  
  # z??czenie wszystkich kolumn
  
  ticket_final <- cbind(ticket_basic_info, ticket_coord)
  
  if(Coord == T){
    result <- ticket_coord
    
  } else {
    result <- ticket_basic_info
  }
  return(result)
}