# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(openPoznan)
library(leaflet)
library(shinydashboard)
library(sp)
library(shinyjs)
library(purrrlyr)
library(dplyr)

# Define UI for application that draws a histogram

#loading content
appCSS <- "
  #loading-content {
    position: absolute;
    background: black;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    color:  #FFFFFF;
}
  
"
myCSS <- "
#gif-contener {
  position: relative;

}
# loading-spinner {
  position: absolute;
  z-index: -1;
  margin-top: -33px;
  margin-left: -33px;

}

#plot.recalclatind {

  z-index: -2;
}
"
ui <- fluidPage(id="page",
                title = "OpenPoznan Shiny app",
                useShinyjs(),
                inlineCSS(appCSS),
                
                div(
                  id="loading-content",
                  h2("Loading...")
                ),
                hidden(
                  div(
                    id="app-content"
                  )
                ),
              
                
      column(6,offset = 4,         
      titlePanel(title = "openPoznan interactive map",
                 windowTitle = "openPoznan App"
      )),
  
     # Sidebar with a slider input for number of bins 
   div(class="outer",
       
       tags$head(tags$style(),
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
                     )),
       
       actionButton("clear_button", 
                    "Clear Map"),
                  
                         id = "controls", 
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
  
  Sys.sleep(1)
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  
   output$llmap <- renderLeaflet({
     
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     progress$set(message = "Clear Map", value = 0)
     
     input$clear_button
     Sys.sleep(1)
    
     leaflet() %>%
       addTiles() %>%
       setView(16.92, 52.40, zoom = 11)
   })
   
   observeEvent(input$clear_button, {
     
     leafletProxy("llmap") %>%
       clearShapes() %>%
       clearMarkerClusters()
     
   })
   
   observe({
     
     progress <- shiny::Progress$new()
     on.exit(progress$close())
     progress$set(message = "Waiting", value = 10)
     
       Points <- TRUE
      lines <- FALSE
       Custom_icon <- makeIcon(iconUrl = "",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)

       Longitude <- 16.931992
       Latitude <- 52.409538
       point_data <- data.frame(Longitude,Latitude)
       marker_name <- "Poznan"
       
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
       Area_coord_id <- pick_data$Coords

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


      }  else if (input$data == "sw") {

       Points <- TRUE
       point_data <- sewage_works()
       marker_name <- point_data$ID

       Custom_icon <- makeIcon(iconUrl = "https://www.corkdrain.ie/assets/icon4.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorY = 25)

     } else if(input$data == "church") {

       Points <- TRUE
       Churches_data_from_openPoznan <- openPoznan::churches_data
       point_data <- Churches_data_from_openPoznan
       marker_name <- Churches_data_from_openPoznan$Parish_Name.y

       Custom_icon <- makeIcon(iconUrl = "https://cdn.iconscout.com/icon/premium/png-512-thumb/church-221-425387.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)

     } else if (input$data == "lg") {

       Points <- TRUE
       point_data <- local_government()
       marker_name <- point_data$Residence

       Custom_icon <- makeIcon(iconUrl = "https://static.thenounproject.com/png/743167-200.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)

     } else if (input$data == "bp"){
       
       lines <- TRUE
       Points <- FALSE
       Clear_map <- FALSE
       mydf <- paths_bike(coords = T)
       longtitude <- mydf$Longitude
       latitude <- mydf$Latitude
       group <- mydf$id2

       df <- data.frame(group = group, lng = longtitude, lat = latitude)
       
       labels <- sprintf("<strong>%s</strong><br/>",mydf$id2) %>% lapply(htmltools::HTML)
       
     } else if (input$data == "dp") {
       
       lines <- TRUE
       Points <- FALSE
       Clear_map <- FALSE
       
       mydf <- paths_dydactic(coords = T)
       longtitude <- mydf$Longitude
       latitude <- mydf$Latitude
       group <- mydf$id1
       
       df <- data.frame(group = group, lng = longtitude, lat = latitude)
       
       labels <- sprintf("<strong>%s</strong><br/>",mydf$id1) %>% lapply(htmltools::HTML)
       
     } else if (input$data == "wp") {
       lines <- TRUE
       Points <- FALSE
       Clear_map <- FALSE
       
       mydf <- paths_walking(coords = T)
       longtitude <- mydf$Longitude
       latitude <- mydf$Latitude
       group <- mydf$id3
       
       df <- data.frame(group = group, lng = longtitude, lat = latitude)
       
       labels <- sprintf("<strong>%s</strong><br/>",mydf$id3) %>% lapply(htmltools::HTML)
       
     } else if (input$data == "rir") {
       lines <- TRUE
       Points <- FALSE
       Clear_map <- FALSE
       
       mydf <- royal_imperial_route()
       longtitude <- mydf$Longitude
       latitude <- mydf$Latitude
       group <- mydf$id
       
       df <- data.frame(group = group, lng = longtitude, lat = latitude)
       
       labels <- sprintf("<strong>%s</strong><br/>",mydf$id) %>% lapply(htmltools::HTML)

     } else if (input$data == "Monument") {

       Points <- TRUE
       point_data <- monuments()
       marker_name <- point_data$Name

       Custom_icon <- makeIcon(iconUrl = "https://image.flaticon.com/icons/png/512/8/8154.png",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)

     } else if (input$data == "hischurch") {
       Points <- TRUE
       point_data <- historical_churches()
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
       point_data <- graves()

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
       point_data <- ticket_sales_points()
       marker_name <- point_data$Name

       Custom_icon <- makeIcon(iconUrl = "https://image.flaticon.com/icons/svg/100/100130.svg",
                               iconWidth = 25,
                               iconHeight = 30,
                               iconAnchorX = 15,
                               iconAnchorY = 25)

     } else if (input$data == "council") {

       Points <- TRUE
       point_data <- council_districts()
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
       Area_coord <- point_data$coord
      
       Electoral_split_data = lapply(unique(Area_coord$id), function(x) {
         df = as.matrix(Area_coord[Area_coord$id == x, c("Longitude", "Latitude") ])
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

       } else if(input$data == "lsdp") {

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

        if(Points == TRUE & lines == FALSE) {
         leafletProxy("llmap") %>%
         clearMarkerClusters() %>%
         addMarkers(lat = point_data$Latitude,
                lng = point_data$Longitude,
                popup = marker_name,
                icon = Custom_icon,
                clusterOptions = markerClusterOptions())
        } else if (Points == FALSE & lines == FALSE) {
          leafletProxy("llmap") %>%
          clearShapes() %>%
          addPolygons(data = poly_data,
                      weight = 2,
                      opacity = 1,
                      dashArray = "3",
                      color = "blue",
                      smoothFactor = 0.5,
                      fillOpacity = 0.5,
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        fillOpacity = 0.2,
                        bringToFront = TRUE),
                       label = labels,
                       labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
                      )
        } else if (Points == FALSE & lines == TRUE) {
          pal <- colorFactor("Accent", NULL)
          grouped_coords <- function(coord = coord,group = group) {
            data.frame(coord = coord, group = group) %>%
              group_by(group) %>%
              by_slice(~c(.$coord, NA), .to = "output") %>%
              .$output %>%
              unlist()
          }
          leafletProxy("llmap") %>% 
            addPolylines(data = df,
                         lng = ~grouped_coords(lng, group),
                         lat = ~grouped_coords(lat, group),
                         color = pal(1:7),
                         label = labels)
                         
        }
   })

}

# Run the application 
shinyApp(ui = ui, server = server)







