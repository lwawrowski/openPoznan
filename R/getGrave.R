# wczytanie danych groboW 
getGrave <- function(Coord = F){ 
  
  cementary_final <- getCemetery()
  
  grave_basic_info_all <- data.frame()
  
  grave_coord_all <- data.frame()
  
  # wybieranie 1000 grob?w z wszstkich cmentarzy i podzielenie na dwa zbiory 
  
  for(i in 1:length(cementary_final$ID)){
    
    cat("Cemetery of",i," from ",length(cementary_final$ID), "\n")
    
    id <- cementary_final$ID[i]
    new_link <- paste0("http://www.poznan.pl/featureserver/featureserver.cgi/groby?maxFeatures=1000&queryable=cm_id&cm_id=",id)
    
    n <- fromJSON(new_link)
    
    grave_basic_info <- n$features$properties
    
    grave_coord_raw <- n$features$geometry$coordinates
    
    grave_coord_t <- map(grave_coord_raw, t)
    
    grave_coord_df <- map(grave_coord_t, data.frame)
    
    grave_coord <- map2_df(grave_coord_df, grave_basic_info$cm_id, ~ mutate(.x, id= .y))
    
    grave_basic_info_all <- rbind(grave_basic_info_all, grave_basic_info)
    
    grave_coord_all <- rbind(grave_coord_all, grave_coord)
    
    
  } 
  # Oczyszczenie danych z niepotrzebnych informacji + nazwanie
  grave_basic_info_final <- data.frame(cbind(grave_basic_info_all$cm_id,
                                             grave_basic_info_all$cm_nr,
                                             grave_basic_info_all$g_date_birth,
                                             grave_basic_info_all$g_date_death,
                                             grave_basic_info_all$g_date_burial,
                                             grave_basic_info_all$g_row,
                                             grave_basic_info_all$g_name,
                                             grave_basic_info_all$g_surname,
                                             grave_basic_info_all$print_surname_name,
                                             grave_basic_info_all$g_field,
                                             grave_basic_info_all$g_place,
                                             grave_basic_info_all$paid))
  
  colnames(grave_basic_info_final) <-c("Cementary_ID",
                                       "Cementary_No",
                                       "Date_birth",
                                       "Date_death",
                                       "Date_burial",
                                       "Row_no",
                                       "Grave_name",
                                       "Grave_surname",
                                       "Grave_name_surname",
                                       "Field_no",
                                       "Place_no",
                                       "Paid")
  if(Coord == T){
    result <- grave_coord_all
  } else {
    return(grave_basic_info_final)
  }
  
}