getEstate <- function(arg = TRUE) {
e <- fromJSON("http://www.poznan.pl/featureserver/featureserver.cgi/wybory_ro_lokale/")
estate <- e$features
estate_coord <- data.frame(matrix(unlist(e$features$geometry$coordinates),
                                  nrow = nrow(e$features), byrow = T))
colnames(estate_coord)[(names(estate_coord)=="X1")] <- "Longitude"
colnames(estate_coord)[(names(estate_coord)=="X2")] <- "Latitude"

estate_basic_info <- data.frame(cbind(estate$id,
                                      estate$type,
                                      estate$properties$adres,
                                      estate$properties$siedziba,
                                      estate$properties$okreg,
                                      estate$properties$przystosowanie,
                                      estate$properties$nr_lokalu,
                                      estate$properties$nr_obwodu))

colnames(estate_basic_info)[(names(estate_basic_info)== "X1")] <- "ID"
colnames(estate_basic_info)[(names(estate_basic_info)== "X2")] <- "Type"
colnames(estate_basic_info)[(names(estate_basic_info)== "X3")] <- "Address"
colnames(estate_basic_info)[(names(estate_basic_info)== "X4")] <- "Residence"
colnames(estate_basic_info)[(names(estate_basic_info)== "X5")] <- "Electoral_District"
colnames(estate_basic_info)[(names(estate_basic_info)== "X6")] <- "AFDP"
colnames(estate_basic_info)[(names(estate_basic_info)== "X7")] <- "Venue_nr"
colnames(estate_basic_info)[(names(estate_basic_info)== "X8")] <- "District_nr"


# złączenie wszystkich kolumn

estate_final <- cbind(estate_basic_info, estate_coord)
if(arg == TRUE){
  +    result <- TRUE
  +  } else {
+    result <- FALSE
}
+ 
+  return(ticket_final)
+}
