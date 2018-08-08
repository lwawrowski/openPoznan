#' tvm Function
#'
#' This function download data about ticket vending machines in Poznań
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @import jsonlite, textclean
#' @format 
#' \describe{
#' \item{number}{character; bike number.}
#' \item{bike_type}{character; bike type.}
#' \item{lock_types}{character; lock types.}
#' \item{id}{factor; station id.}
#' \item{uid}{factor; uid.}
#' \item{lat}{factor; latitude.}
#' \item{lng}{factor; longitude}
#' \item{name}{factor; name.}
#' \item{bikes}{factor; bikes.}
#' \item{bike_racks}{factor; bike racks.}
#' \item{free_racks}{factor; free racks.}
#' \item{bike_types}{factor; bike types.}
#' }
#' @examples
#' tvm()




tvm <- function () {

  #Biletomaty Calosc 
  
  #Wstepna analiza
  
  if(havingIP() == T) {
    
    tryCatch({
  
  TVM_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=4000")
  
    },error = function(e) {
      warning("You used bad link!")
    })
    
  } else {
    warning("You lost connection to internet!")
  }
  
  TVM_features <- TVM_blank$features
  
  # Utworzenie koordynatow + nazwanie 
  
  TVM_coord <- data.frame(matrix(unlist(TVM_features$geometry$coordinates),
                                 nrow = nrow(TVM_features),
                                 byrow = T))
  
  colnames(TVM_coord) <- c("Longitude",
                           "Latitude")
  
  
  #Oczyszczenie danych z niepotrzebnych informacji + nazwanie 
  
  TVM_basic_info <- data.frame(cbind(TVM_features$id,
                                     TVM_features$properties$nazwa,
                                     TVM_features$properties$y_4345_obs_uga_syste,
                                     TVM_features$properties$y_4346_karty_p_atnic,
                                     TVM_features$properties$kolejnosc,
                                     TVM_features$properties$opis))
  
  colnames(TVM_basic_info)<- c("ID",
                               "TVM",
                               "PEKA",
                               "Store_Card",
                               "TVM_Order",
                               "TVM_Description")
  
  TVM_basic_info$TVM_Description <- replace_html(TVM_basic_info$TVM_Description, symbol = FALSE)

  # Ostateczne polaczenie 
  
  TVM_final <- cbind(TVM_basic_info,TVM_coord)
  
return(TVM_final)

}


#examples 


tvms <- tvm()


