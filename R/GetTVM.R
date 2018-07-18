

getTVM <- function () {

#Wstepna analiza

TVM_blank <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=class_objects&class_id=4000")

TVM_features <- TVM_blank$features

# Utworzenie koordynatow + nazwanie 

TVM_coord <- data.frame(matrix(unlist(TVM_features$geometry$coordinates),
                               nrow = nrow(TVM_features), byrow = T))

colnames(TVM_coord)[(names(TVM_coord) == "X1")] <- "Longitude"
colnames(TVM_coord)[(names(TVM_coord) == "X2")] <- "Latitude"

#Oczyszczenie danych z niepotrzebnych informacji + nazwanie 

TVM_basic_info <- data.frame(cbind(TVM_features$id,
                                   TVM_features$properties$nazwa,
                                   TVM_features$properties$y_4345_obs_uga_syste,
                                   TVM_features$properties$y_4346_karty_p_atnic,
                                   TVM_features$properties$kolejnosc,
                                   TVM_features$properties$opis))

colnames(TVM_basic_info)[(names(TVM_basic_info) == "X1")] <- "ID"
colnames(TVM_basic_info)[(names(TVM_basic_info) == "X2")] <- "TVM"
colnames(TVM_basic_info)[(names(TVM_basic_info) == "X3")] <- "PEKA"
colnames(TVM_basic_info)[(names(TVM_basic_info) == "X4")] <- "Store_Card"
colnames(TVM_basic_info)[(names(TVM_basic_info) == "X5")] <- "TVM_Order"
colnames(TVM_basic_info)[(names(TVM_basic_info) == "X6")] <- "TVM_Description"

# Ostateczne polaczenie 

TVM_final <- cbind(TVM_basic_info,TVM_coord)


return(TVM_final)

}


#examples 


TVMs <- getTVM()


