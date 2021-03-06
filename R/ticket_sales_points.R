#' ticket_sales_points  Function
#'
#' This function download data about ticket sales points in Poznan.
#' @keywords keyword
#' @export
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
#' \item{Longitude}{numeric; Longitude of sales points.}
#' \item{Latitude}{numeric; Latitude of sales points.}
#' }
#' @examples
#' Ticket <- ticket_sales_points()

ticket_sales_points <- function() {
  # sprzeda biletow
  
  # wczytanie danych biletow 
  
  if(have_ip() == T) {
    
  
  
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
  colnames(ticket_coord)[(names(ticket_coord)=="X1")] <- "Longitude"
  colnames(ticket_coord)[(names(ticket_coord)=="X2")] <- "Latitude"
  
  
  ticket_basic_info <- data.frame(Mon_Fri=ticket$properties$y_4308_godziny_otwar,
                                        Opening_Hours_2=ticket$properties$y_4310_godziny_otwar,
                                        Sales_form=ticket$properties$y_4311_forma_sprzeda,
                                        Ticket=ticket$properties$y_4326_bilety_jednor,
                                        Name=ticket$properties$nazwa,
                                        Weekend=ticket$properties$y_4309_godziny_otwar,
                                        Address=ticket$properties$adres,
                                        City=ticket$properties$miasto,
                                        Lang=ticket$properties$lang,
                                        PEKA_Card=ticket$properties$y_4327_peka__karta_n,
                                        ID_Class=ticket$properties$id_klasy,
                                        Description=ticket$properties$opis)
  
  # z??czenie wszystkich kolumn
  
  ticket_final <- cbind(ticket_basic_info, ticket_coord)
  
  
  return(ticket_final)
}
