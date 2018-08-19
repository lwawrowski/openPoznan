# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(openPoznan)
library(leaflet)
library(shinydashboard)
library(sp)



# Define UI for application that draws a histogram
ui <- fluidPage(id="page",
                title = "OpenPoznan Shiny app",
                
      titlePanel(title = "openPoznan interactive map",
                 windowTitle = "openPoznan App"
      ),
  
     # Sidebar with a slider input for number of bins 
   div(class="outer",
       
       tags$head(
         includeCSS("styles.css")
       ),
       
       leafletOutput(outputId = "llmap", width = "100%", height = "100%"),
       
       absolutePanel(
       selectInput( inputId = "data",
                    label = "Pick :",
                   c("Ticket vending machine" = "tvm",
                     "Parking machine" = "pm",
                     "Stop" = "stop",
                     "Parish" = "parish",
                     "Church" = "church",
                     "School area" = "area",
                     "Cesspool" = "cesspool",
                     "Sewage work" = "sw",
                     "Local Government" = "lg",
                     "Bike Paths" = "bp",
                     "Dydactic Paths" = "dp",
                     "walking Paths" = "wp",
                     "Monument" = "Monument",
                     "Historical church" = "hischurch",
                     "Cementery" = "cemetery",
                     "Graves" = "graves",
                     "Poznan districts" = "district",
                     "Points with 'ZTM' tickets" = "ticket",
                     "Royal imperial route" = "rir",
                     "Council district" = "council",
                     "Electoral area" = "elearea",
                     "Electoral circle" = "circle",
                     "Bike stations" = "bike_stations",
                     "Local Spatial Development Plans Called" = "lsdpc",
                     "Local Passed Spatial Development Plans" = "lsdp"
                     #dodajemy po przecinku ;) 
                     )), id = "controls", 
                         class = "panel panel-default",
                         fixed = TRUE,
                         draggable = TRUE, 
                         top = 60, 
                         left = "auto", 
                         right = 20, 
                         bottom = "auto",
                         width = 330, 
                         height = "auto")
     )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$llmap <- renderLeaflet({
    
     
     leaflet() %>%
       addTiles() %>%
       setView(16.92, 52.40, zoom = 11)
   })
   
   
   observe({


     if (input$data == "tvm") {
       
       Points <- TRUE
       point_data <- ticket_vending_machines()
       marker_name <- point_data$ID

       Custom_icon <- makeIcon(iconUrl = "https://d30y9cdsu7xlg0.cloudfront.net/png/44651-200.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
     } else if (input$data == "pm") {
       
       Points <- TRUE
       point_data <- parking_machines()
       marker_name <- point_data$ID

       Custom_icon <- makeIcon(iconUrl = "https://image.flaticon.com/icons/svg/34/34783.svg",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
     } else if (input$data == "stop") {
        
       Points <- TRUE
       point_data <- tram_bus_stops()
       marker_name <- point_data$ID

       Custom_icon <- icons(iconUrl = ifelse(point_data$Route_Type == 3,"https://d30y9cdsu7xlg0.cloudfront.net/png/19259-200.png","http://icons.iconarchive.com/icons/icons8/android/512/Transport-Tram-icon.png"),
                           iconWidth = 25,
                           iconHeight = 30,
                           iconAnchorX = 15,
                           iconAnchorY = 25)
     } else if (input$data == "cesspool") {
       
       Points <- TRUE
       point_data <- cesspools()
       marker_name <- point_data$ID
       
       Custom_icon <- makeIcon(iconUrl = "http://cdn.onlinewebfonts.com/svg/img_537720.png",
                            iconWidth = 25,
                            iconHeight = 30,
                            iconAnchorY = 25)
       
     } else if(input$data == "parish") {
       
       
       Points <- FALSE
       basic_info <- parishes()
       pick_data <- parishes(T)
       Parish_coord_id <- pick_data$Coords

       Parish_split_data = lapply(unique(Parish_coord_id$ID), function(x) {
         df = as.matrix(Parish_coord_id[Parish_coord_id$ID == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })

       poly_data = SpatialPolygons(Parish_split_data)
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$Parish_Name) %>% 
         lapply(htmltools::HTML)


     } else if(input$data == "area") {
       
       Points <- FALSE
       basic_info <- school_basic_areas()
       pick_data <- school_basic_areas(T)
       Area_coord_id <- pick_data$result
       
       Area_split_data = lapply(unique(Area_coord_id$ID), function(x) {
         df = as.matrix(Area_coord_id[Area_coord_id$ID == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })
       
       poly_data = SpatialPolygons(Area_split_data)
       
       # Leaflet map with custom icon 
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$School_Name) %>% 
         lapply(htmltools::HTML)
       

     } else if (input$data == "sw") {
       
       Points <- TRUE
       point_data <- sewage_works()
       marker_name <- point_data$ID
       
       Custom_icon <- makeIcon(iconUrl = "https://www.corkdrain.ie/assets/icon4.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorY = 25)
       
     } else if(input$data == "church") {
       
       Points <- TRUE
       
       #Church List from town hall API 
       
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
       
       marker_name <- Church_mixed_with_Parish$Parish_Name.y
       
       Custom_icon <- makeIcon(iconUrl = "https://cdn.iconscout.com/icon/premium/png-512-thumb/church-221-425387.png",
                               iconHeight = 30,
                               iconAnchorX = 15, 
                               iconAnchorY = 25)
       
     } else if (input$data == "lg") {
       
       Points <- TRUE
       point_data <- local_government(coords = T)
       marker_name <- point_data$Residence
       
       Custom_icon <- makeIcon(iconUrl = "https://static.thenounproject.com/png/743167-200.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
       
     } else if (input$data == "Monument") {
       
       Points <- TRUE
       point_data <- monuments(coords = T)
       marker_name <- point_data$Name
       
       Custom_icon <- makeIcon(iconUrl = "https://image.flaticon.com/icons/png/512/8/8154.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
       
     } else if (input$data == "hischurch") {
       Points <- TRUE
       point_data <- historical_churches(coords = T)
       marker_name <- point_data$Name
       
       Custom_icon <- makeIcon(iconUrl = "http://www.stickpng.com/assets/images/5a018f9d7ca233f48ba6271a.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
     } else if (input$data == "cemetery") {
      
      Points <- FALSE
      basic_info <- cemeteries(coords = F)
      point_data <- cemeteries(coords = T)
      cemetery_coord <- point_data$coord
      
      Cemetery_split_data = lapply(unique(cemetery_coord$id), function(x) {
        df = as.matrix(cemetery_coord[cemetery_coord$id == x, c("Longitude", "Latitude") ])
        polys = Polygons(list(Polygon(df)), ID = x)
        return(polys)
       })
   
   poly_data = SpatialPolygons(Cemetery_split_data)
   
   labels <- sprintf("<strong>%s</strong><br/>",
                     basic_info$Cemetery_Name) %>% 
     lapply(htmltools::HTML)
   
     } else if (input$data == "graves") {
       Points <- TRUE
       point_data <- graves(coords = T)
       
       Custom_icon <- makeIcon(iconUrl = "https://cdn.iconscout.com/public/images/icon/premium/png-512/cemetery-367830362729accc-512x512.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
       
     } else if (input$data == "district") {
       
       Points <- FALSE 
       basic_info <- districts(coords = F)
       point_data <- districts(coords = T)
       distric_coord <- point_data$coord
       
       District_split_data = lapply(unique(distric_coord$id), function(x) {
         df = as.matrix(distric_coord[distric_coord$id == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })
       poly_data = SpatialPolygons(District_split_data)
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$Name) %>% 
         lapply(htmltools::HTML)
       
     } else if (input$data == "ticket") {
       
       Points <- TRUE
       point_data <- ticket_sales_points(coords = T)
       marker_name <- point_data$Name
       
       Custom_icon <- makeIcon(iconUrl = "https://image.flaticon.com/icons/svg/100/100130.svg",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
       
     } else if (input$data == "council") {
       
       Points <- TRUE
       point_data <- council_districts(coords = T)
       marker_name <- point_data$Electoral_District
       
       Custom_icon <- makeIcon(iconUrl = "https://static.thenounproject.com/png/743167-200.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)
     } else if (input$data == "elearea") {
       
       Points <- FALSE
       basic_info <- electoral_areas(coords = F)
       point_data <- electoral_areas(coords = T)
       Electoral_coord <- point_data$coord
       
       Electoral_split_data = lapply(unique(Electoral_coord$id), function(x) {
         df = as.matrix(Electoral_coord[Electoral_coord$id == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })
       poly_data = SpatialPolygons(Electoral_split_data)
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$ID) %>% 
         lapply(htmltools::HTML)
       
     } else if (input$data == "circle") {
       
       Points <- FALSE
       basic_info <- electoral_circles(coords = F)
       point_data <- electoral_circles(coords = T)
       Circle_coord <- point_data$coord
       
       Circle_split_data = lapply(unique(Circle_coord$id), function(x) {
         df = as.matrix(Circle_coord[Circle_coord$id == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })
       poly_data = SpatialPolygons(Circle_split_data)
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$ID) %>% 
         lapply(htmltools::HTML)
     }
     else if (input$data == "bike_stations") {
       
       Points <- TRUE
       point_data <- bike_stations()
       marker_name <- point_data$station
       
       Custom_icon <- makeIcon(iconUrl = "https://cdn.onlinewebfonts.com/svg/img_538285.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorY = 25)
       
     }
     else if(input$data == "lsdpc") {
       
       
       Points <- FALSE
       basic_info <- local_spatial_dev_plans_called(basic = TRUE)
       pick_data <- local_spatial_dev_plans_called(basic = FALSE)
       lsdpc_coord_id <- pick_data
       
       lsdpc_split_data = lapply(unique(lsdpc_coord_id$ID), function(x) {
         df = as.matrix(lsdpc_coord_id[lsdpc_coord_id$ID == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })
       
       poly_data = SpatialPolygons(lsdpc_split_data)
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$lsdpc_location_name) %>% 
         lapply(htmltools::HTML)
       
       
     }
     else if(input$data == "lsdp") {
       
       
       Points <- FALSE
       basic_info <- local_spatial_dev_plans_passed(basic = TRUE)
       pick_data <- local_spatial_dev_plans_passed(basic = FALSE)
       lsdp_coord_id <- pick_data
       
       lsdp_split_data = lapply(unique(lsdp_coord_id$ID), function(x) {
         df = as.matrix(lsdp_coord_id[lsdp_coord_id$ID == x, c("Longitude", "Latitude") ])
         polys = Polygons(list(Polygon(df)), ID = x)
         return(polys)
       })
       
       poly_data = SpatialPolygons(lsdp_split_data)
       
       labels <- sprintf("<strong>%s</strong><br/>",
                         basic_info$lsdp_location_name) %>% 
         lapply(htmltools::HTML)
       
       
     }

        if(Points == TRUE) {
         leafletProxy("llmap") %>%
         clearMarkerClusters() %>%
         clearShapes() %>%
         addMarkers(lat = point_data$Latitude,
                lng = point_data$Longitude,
                popup = marker_name,
                icon = Custom_icon,
                clusterOptions = markerClusterOptions())
        } else if (Points == FALSE) {
          leafletProxy("llmap") %>%
          clearMarkerClusters() %>%
          clearShapes() %>%
          addPolygons(data = poly_data,
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      color = "white",
                      smoothFactor = 0.5,
                      fillOpacity = 0.5,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                       label = labels,
                       labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
                      )
        }
     
     
     


   })

}




# Run the application 
shinyApp(ui = ui, server = server)







