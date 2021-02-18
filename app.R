# load packages
library(shiny)  
#library(rvest)  # website scrape
#library(rgdal)  # dealing with shapefiles
library(leaflet)  # fancy interactive map

# 
# 
# # load postcrossing data
# pc.data <- read.csv("data/postInfoScrape.csv")
# 
# 
# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody(
#     tags$img(src = "https://www.pngfind.com/pngs/m/461-4619276_hello-lettering-hd-png-download.png",
#              style = 'position: absolute; opacity: 0.2;')
#   )
# )
# 
# 
# setwd("~/Dropbox/Coding/AarhusThroughTime/")
# 
# 
# ui <- fluidPage(
#   mainPanel(
#     img(src='Trojborgvej50_2021.png', align = "right"),
#     ### the rest of your code
#   )
# )
# 
# server <- function(input, output) {}
# shinyApp(ui, server)



# https://stackoverflow.com/questions/53601495/overlaying-images-in-r-shiny
ui <- fluidPage(
  
  titlePanel("R-eindeer"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("check1","amount of new", choices = c("1","2","3"))
    ),
    
    mainPanel(
      imageOutput("reindeer")
    )
  )
)



server <- function(input,output){
  
  # get_image <- function(type, color) {
  #   image_read(file.path(type, paste0(color, ".png")))
  # }
  
  output$reindeer <- renderImage({
    
    # load the images
    # coat <- get_image("coat", paste0("reindeer_", input$check1))
    # outline <- get_image("outlines", "reindeer_outline")
    old <- image_read(file.path("old", "Trojborgvej50_1906.png"))
    mynew <- image_read(file.path("mynew", "Trojborgvej50_2021.png"))
    
    # make the reindeer: overlay in order
    reindeer <- c(old, mynew)
    
    # create a temp file
    tmpfile <- reindeer %>%
      image_mosaic() %>%
      image_flatten() %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # render the file
    return(list(src = tmpfile,
                height = 300,
                width = 300,
                alt = "Your reindeer",
                contentType = "image/jpg"))
  }, deleteFile = TRUE)
}

shinyApp(ui, server)

# 
# 
# ui <- fluidPage(
#   
#   title = "Test photo morph",
#   
#   h2(textOutput("country")),
#   h4("Click the map or use the seach bar to find information about your country of interest"),
#   fluidRow(
#     column(8,
#            leafletOutput('map', height=600),
#            h6("Data collected by", align = "center"),
#            HTML('<div style="text-align: center;"><p><a href="https://www.postcrossing.com/postal-monitor">
#                   <img src="PClogo.png" width="282" height="36" style="text-align:center" /></a></p></div>'),
#            h6(as.character(pc.data$updated[1]), align = "center")
#            
#            
#     ),
#     column(4, 
#            selectizeInput("searched.country",  # selectizeInput makes writable and searchable dropdown menu
#                           "Search country",
#                           choices = all.countries, 
#                           selected = "none searched"), 
#            downloadButton("downloadData", "Download Table"),
#            tableOutput('info_table')
#     )
#   ),
#   
#   # for user geolocation (with prompt) https://github.com/AugustT/shiny_geolocation
#   tags$script('
#       $(document).ready(function () {
#         navigator.geolocation.getCurrentPosition(onSuccess, onError);
#               
#         function onError (err) {
#           Shiny.onInputChange("geolocation", false);
#         }
#               
#         function onSuccess (position) {
#           setTimeout(function () {
#             var coords = position.coords;
#             console.log(coords.latitude + ", " + coords.longitude);
#             Shiny.onInputChange("geolocation", true);
#             Shiny.onInputChange("lat", coords.latitude);
#             Shiny.onInputChange("long", coords.longitude);
#           }, 1100)
#         }
#       });
#               ')
# )
# 
# server <- function(input, output, session) {
#   
#   # basic start map
#   output$map <- renderLeaflet({
#     
#     # if user has allowed geolocation, use that to select start country
#     if (!is.null(input$lat)){
#       lat <- input$lat
#       lon <- input$long
#       coords <- as.data.frame(cbind(lon, lat))
#       point <- SpatialPoints(coords)
#       proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#       selected.country <- world[point, ]@data$NAME_NEW
#       output$country <- renderText({ 
#         paste0("Where can't I send a postcard from ", selected.country, "?")
#       })
#     } else {
#       selected.country <- "Germany"  # else use Germany as start country
#       output$country <- renderText({ 
#         paste0("Where can't I send a postcard from ", selected.country, "?")
#       })
#     }
#     
#     # table
#     output$info_table <- renderTable(dfPrep(selected.country)) 
#     output$downloadData <- downloadHandler(
#       filename = paste0("PostBlocked_", selected.country, ".csv"),
#       content = function(file) {
#         write.csv(dfPrep(selected.country), file, row.names = FALSE)
#       }
#     )
#     
#     # plot
#     leaflet(world) %>% 
#       addTiles() %>% 
#       setView(lat=20, lng=0 , zoom=2) %>%
#       addPolygons( 
#         fillColor = mapColPrep(selected.country), 
#         stroke = F, 
#         group = 'initialColors',
#         label = mytext,
#         labelOptions = labelOptions( 
#           style = list("font-weight" = "normal", padding = "3px 8px"), 
#           textsize = "13px", 
#           direction = "auto"
#         )
#       ) %>% addLegend(colors=c("yellow", "green", "red", "#cccccc", "#5b0f00"), 
#                       labels=c("Selected country", "Receives mail from selected country", "Blocked by selected country", "Information lacking", "COVID blocked"), 
#                       opacity=0.3, position = "bottomleft")
#     
#   })
#   
#   ## add different map on click
#   observeEvent(input$map_shape_click, {
#     click <- input$map_shape_click
#     
#     if(is.null(click))
#       return()   
#     
#     #pulls lat and lon from shiny click event
#     lat <- click$lat
#     lon <- click$lng
#     
#     #puts lat and lon for click point into its own data frame
#     coords <- as.data.frame(cbind(lon, lat))
#     
#     #converts click point coordinate data frame into SP object, sets CRS
#     point <- SpatialPoints(coords)
#     proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#     
#     selected.country <- world[point, ]@data$NAME_NEW
#     
#     # title
#     output$country <- renderText({ 
#       paste0("Where can't I send a postcard from ", selected.country, "?")
#     })
#     
#     # table
#     output$info_table <- renderTable(dfPrep(selected.country)) 
#     output$downloadData <- downloadHandler(
#       filename = paste0("PostBlocked_", selected.country, ".csv"),
#       content = function(file) {
#         write.csv(dfPrep(selected.country), file, row.names = FALSE)
#       }
#     )
#     
#     # plot
#     proxy <- leafletProxy("map")
#     proxy %>% 
#       clearGroup('selectedColors') %>%
#       clearGroup('initialColors') %>%
#       setView(lat=20, lng=0 , zoom=2) %>%
#       addPolygons(data = world, 
#                   fillColor = mapColPrep(selected.country),
#                   color = "red",
#                   weight = 3, 
#                   stroke = F,
#                   group = 'selectedColors',
#                   label = mytext,
#                   labelOptions = labelOptions( 
#                     style = list("font-weight" = "normal", padding = "3px 8px"), 
#                     textsize = "13px", 
#                     direction = "auto"
#                   )
#       )
#   })
#   
#   ## add different map on search
#   observeEvent(input$searched.country, {
#     selected.country <- input$searched.country
#     
#     if(selected.country == "none searched") {
#       return()
#     } else if (!selected.country %in% all.countries) {
#       return ()
#     }
#     
#     # title
#     output$country <- renderText({ 
#       paste0("Where can't I send a postcard from ", selected.country, "?")
#     })
#     
#     # table
#     output$info_table <- renderTable(dfPrep(selected.country)) 
#     output$downloadData <- downloadHandler(
#       filename = paste0("PostBlocked_", selected.country, ".csv"),
#       content = function(file) {
#         write.csv(dfPrep(selected.country), file, row.names = FALSE)
#       }
#     )
#     
#     # plot
#     proxy <- leafletProxy("map")
#     proxy %>% 
#       clearGroup('selectedColors') %>%
#       clearGroup('initialColors') %>%
#       setView(lat=20, lng=0 , zoom=2) %>%
#       addPolygons(data = world, 
#                   fillColor = mapColPrep(selected.country),
#                   stroke = F,
#                   group = 'selectedColors',
#                   label = mytext,
#                   labelOptions = labelOptions( 
#                     style = list("font-weight" = "normal", padding = "3px 8px"), 
#                     textsize = "13px", 
#                     direction = "auto"
#                   )
#       )
#   })
# }
# 
# shinyApp(ui, server)
# 
