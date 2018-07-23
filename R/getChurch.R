getChurch <- function(Coord = F){
  
  # wczytanie danych o kosciolach
  ch <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=2471")
  church <- ch$features
  
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  
  church_coord <- data.frame(matrix(unlist(ch$features$geometry$coordinates),
                                    nrow = nrow(ch$features), byrow = T))
  colnames(church_coord)[(names(church_coord)=="X1")] <- "V1"
  colnames(church_coord)[(names(church_coord)=="X2")] <- "V2"
  
  church_basic_info <- data.frame(cbind(church$id,
                                        church$properties$kod,
                                        church$properties$nazwa,
                                        church$properties$opis_klasy,
                                        church$properties$adres,
                                        church$properties$miasto,
                                        church$properties$lang,
                                        church$properties$id_klasy,
                                        church$properties$opis))
  
  colnames(church_basic_info)<-c("ID","Code","Name","Category_builiding","City","Lang","Address","ID_monument","Description")
  
  # z??czenie wszystkich kolumn
  
  church_final <- cbind(church_basic_info,church_coord)
  if(Coord == F){
    result <- church_coord
  } else {
    return(church_final)
  }
}