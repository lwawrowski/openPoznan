#' churches_wiki  Function
#'
#' This function download data about churches in Poznan.
#' @keywords keyword
#' @export
#' @details Details of usage 
#' @importFrom jsonlite fromJSON 
#' @importFrom purrr map map2_df
#' @importFrom dplyr mutate distinct join
#' @importFrom openPoznan parishes
#' @importFrom read_xml read_html 
#' @importFrom rvest html_nodes html_text 
#' @importFrom textclean replace_non_ascii
#' @importFrom fuzzyjoin stringdist_join
#' @format 
#' \describe{
#' \item{Parish_Name.y}{character; Name of certain Church.}
#' \item{Longitude}{numeric; Longitude of church.}
#' \item{Latitude}{numeric; Latitude of church.}
#' }
#' @examples
#' Churches <- churches_wiki()

churches_wiki <- function () {

basic_info <- parishes()

Church <- distinct(basic_info, Parish_Name)

Church_list_df <- data.frame(Church, stringsAsFactors = FALSE)

Church_list_df$Parish_Name <- paste("Kosciol ", Church_list_df$Parish_Name)

Church_list_df$Parish_Name <- ifelse(grepl(" w ", Church_list_df$Parish_Name),
                                     Church_list_df$Parish_Name,
                                     paste(Church_list_df$Parish_Name, " w Poznaniu"))

Church_list_df[sapply(Church_list_df, is.character)] <- lapply(Church_list_df[sapply(Church_list_df,is.character)], as.factor)

#Webbscrapping
url <- "https://pl.wikipedia.org/wiki/Kategoria:Ko%C5%9Bcio%C5%82y_rzymskokatolickie_w_Poznaniu"

Church_wiki_list <- url %>%
  read_html %>%
  html_nodes(".mw-category ul li a") %>%
  html_text()

Church_wiki_links <- url %>%
  read_html %>%
  html_nodes(".mw-category ul li a") %>%
  html_attr("href")

Church_wiki_links <- paste("https://pl.wikipedia.org", Church_wiki_links, sep="")

Church_coord_final <- data.frame()

for(i in 1:length(Church_wiki_list)){
  
  church <- Church_wiki_links[i]
  
  long <- church %>%
    read_html %>%
    html_nodes(".infobox .geo-nondefault .geo-dms .longitude") %>%
    html_text()
  
  lat <- church %>%
    read_html %>%
    html_nodes(".infobox .geo-nondefault .geo-dms .latitude") %>%
    html_text()
  
  church_name <- church %>%
    read_html %>%
    html_nodes("#firstHeading") %>%
    html_text
  
  if(length(long) != 0 & length(lat) != 0){
    
    result <- data.frame(long, lat, church_name)
    
    Church_coord_final <- rbind(Church_coord_final, result)
  }
}

colnames(Church_coord_final) <- c("Longitude",
                                  "Latitude",
                                  "Parish_Name")

Church_coord_final$Parish_Name <- replace_non_ascii(Church_coord_final$Parish_Name)
Church_list_df$Parish_Name <- replace_non_ascii(Church_list_df$Parish_Name)

Church_list_df$Parish_Name <- gsub("Swietej", "sw.", Church_list_df$Parish_Name)
Church_list_df$Parish_Name <- gsub("Swietego", "sw.", Church_list_df$Parish_Name)
Church_coord_final$Parish_Name <- gsub("Swietej", "sw.", Church_coord_final$Parish_Name)
Church_coord_final$Parish_Name <- gsub("Swietego", "sw.", Church_coord_final$Parish_Name)
Church_coord_final$Parish_Name <- gsub("Bozej", "Boskiej", Church_coord_final$Parish_Name)
Church_coord_final$Parish_Name <- gsub("Bazylika", "Kosciol", Church_coord_final$Parish_Name)
Church_list_df$Parish_Name <- gsub("Bozej", "Boskiej", Church_list_df$Parish_Name)

#Modification for 1 church

Church_coord_final$Parish_Name <- gsub("archikatedralna", "", Church_coord_final$Parish_Name)
Church_coord_final$Parish_Name <- gsub("Apostolow Slowian i Patronow Europy", "", Church_coord_final$Parish_Name)
Church_coord_final$Parish_Name <- gsub("Kosciol Nawrocenia sw. Pawla Apostola", "Kosciol Nawrocenia sw. Pawla", Church_coord_final$Parish_Name)
Church_list_df$Parish_Name <- gsub("z Pietrelciny", "", Church_list_df$Parish_Name)
Church_list_df$Parish_Name <- gsub("Chojnica - Morasko", "", Church_list_df$Parish_Name)
Church_list_df$Parish_Name <- gsub("Kosciol sw. Lukasza", "Kosciol sw. Lukasza Ewangelisty", Church_list_df$Parish_Name)
Church_list_df$Parish_Name <- gsub("Kosciol Matki Odkupiciela", "Kosciol Najswietszej Maryi Panny Matki Odkupiciela", Church_list_df$Parish_Name)
Church_list_df$Parish_Name <- gsub("`", "", Church_list_df$Parish_Name)

removePunctWords <- function(x) {
  gsub(pattern = "\\(\\w*", "", x)
}

Church_coord_final$Parish_Name <- removePunctWords(Church_coord_final$Parish_Name)

Church_coord_final$Parish_Name <- gsub("(.*?) w .*" , "\\1" ,Church_coord_final$Parish_Name )
Church_list_df$Parish_Name<- gsub("(.*?) w .*" , "\\1" , Church_list_df$Parish_Name )

Podobienstwa <- Church_coord_final %>%
  stringdist_right_join(Church_list_df,
                        by = "Parish_Name",
                        distance_col = NULL,
                        method = "osa")

#Downloading missing data from Parish coords (they're close to church)

Parish_wiki_page1 <- "https://pl.wikipedia.org/wiki/Kategoria:Parafie_rzymskokatolickie_w_Poznaniu"

Parish_region_list <- Parish_wiki_page1 %>%
  read_html %>%
  html_nodes(".mw-category ul li a") %>%
  html_attr ("href")

Parish_wiki_links_page1 <- paste("https://pl.wikipedia.org", Parish_region_list, sep="")

Parish_wiki_links_page2 <- data.frame()

for(i in 1:length(Parish_wiki_links_page1)){
  
  Parish <- Parish_wiki_links_page1[i]
  
  Parish_name_wiki <- Parish %>%
    read_html %>%
    html_nodes(".mw-category ul li a") %>%
    html_attr ("href")
  
  result <- data.frame(Parish_name_wiki)
  
  Parish_wiki_links_page2 <- rbind(Parish_wiki_links_page2, result)
}

Parish_wiki_links_page2$Parish_name_wiki <- paste("https://pl.wikipedia.org", Parish_wiki_links_page2$Parish_name_wiki, sep="")

#Adding 2 additional links, cos we need em for filling things from town hall list

Parish_adds <- c("https://pl.wikipedia.org/wiki/Ko%C5%9Bci%C3%B3%C5%82_%C5%9Bw._J%C3%B3zefa_Oblubie%C5%84ca_Naj%C5%9Bwi%C4%99tszej_Maryi_Panny_w_Kicinie","https://pl.wikipedia.org/wiki/Parafia_%C5%9Bw._Marcina_i_%C5%9Bw._Wincentego_M%C4%99czennika_w_Sk%C3%B3rzewie") %>% data.frame

colnames(Parish_adds) <- c("Parish_name_wiki")

Parish_wiki_links_page2 <- rbind(Parish_wiki_links_page2,Parish_adds)

Parish_coord_final_wiki <- data.frame()

for(i in 1:length(Parish_wiki_links_page2$Parish_name_wiki)){
  
  church <- Parish_wiki_links_page2$Parish_name_wiki[i]
  
  long <- church %>%
    read_html %>%
    html_nodes(".infobox .geo-nondefault .geo-dms .longitude") %>%
    html_text()
  
  lat <- church %>%
    read_html %>%
    html_nodes(".infobox .geo-nondefault .geo-dms .latitude") %>%
    html_text()
  
  parish_name <- church %>%
    read_html %>%
    html_nodes("#firstHeading") %>%
    html_text
  
  if(length(long) != 0 & length(lat) != 0){
    
    result <- data.frame(long, lat, parish_name)
    
    Parish_coord_final_wiki <- rbind(Parish_coord_final_wiki, result)
  }
}

Parish_coord_final_wiki$parish_name <- replace_non_ascii(Parish_coord_final_wiki$parish_name)

Parish_coord_final_wiki$parish_name <- gsub("Parafia", "Kosciol", Parish_coord_final_wiki$parish_name)
Parish_coord_final_wiki$parish_name <- gsub("Swietej", "sw.", Parish_coord_final_wiki$parish_name)
Parish_coord_final_wiki$parish_name <- gsub("Swietego", "sw.", Parish_coord_final_wiki$parish_name)
Parish_coord_final_wiki$parish_name <- gsub("(.*?) w .*" , "\\1" ,Parish_coord_final_wiki$parish_name )

Parish_coord_final_wiki[sapply(Parish_coord_final_wiki, is.factor)] <- lapply(Parish_coord_final_wiki[sapply(Parish_coord_final_wiki,is.factor)], as.character)

Podobienstwa[sapply(Podobienstwa, is.factor)] <- lapply(Podobienstwa[sapply(Podobienstwa,is.factor)], as.character)

Church_mixed_with_Parish <- left_join(Podobienstwa, Parish_coord_final_wiki, by=c("Parish_Name.y"="parish_name")) %>%
  mutate(Longitude=if_else(is.na(Longitude),long,Longitude),
         Latitude=if_else(is.na(Latitude),lat,Latitude))

Church_mixed_with_Parish <- subset(Church_mixed_with_Parish, select=-c(Parish_Name.x,long,lat))

Church_mixed_with_Parish$Latitude <- gsub(",", ".", Church_mixed_with_Parish$Latitude)
Church_mixed_with_Parish$Longitude <- gsub(",", ".", Church_mixed_with_Parish$Longitude)

point_data <- transform(Church_mixed_with_Parish, Longitude = as.numeric(Longitude),
                        Church_mixed_with_Parish, Latitude = as.numeric(Latitude))

point_data <- point_data[,-(4:6)]

churches_data <- point_data

return (point_data)

}
