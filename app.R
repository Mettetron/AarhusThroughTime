# load packages
library(tidyverse) # for read_csv
library(shiny)  
library(shinyWidgets) # for sliderTextInput
library(magick)  # for nice image overlay in shiny
library(leaflet)  # fancy interactive map
#library(rvest)  # website scrape
#library(rgdal)  # dealing with shapefiles

# i got help from here:
# https://stackoverflow.com/questions/53601495/overlaying-images-in-r-shiny

# # prep - change here for different images
# old.photo.file <- "Trojborgvej50_1906.jpg"
# new.photo.file <- "Trojborgvej50_2021.png"
# old.year <- "1906"
# new.year <- "2021"
# img.or <- "landscape"
  
# set up user interface
ui <- fluidPage(
  
  titlePanel("Aarhus Through Time"),
  
  sidebarLayout(
    
    sidebarPanel(
      leafletOutput("mymap")
    ),
    
    mainPanel(
      fluidRow(align="center", 
               div(style = paste0("height:", ifelse(img.or =="landscape", 400, 650), "px"),
                   imageOutput("myphoto"))
      ),
      fluidRow(align="center",
               sliderTextInput("img.opacity", "Year shown",
                               choices = c(old.year, seq(0.05, 0.95, 0.05), new.year), 
                               selected = old.year)
      ),
      fluidRow(align="center",
               "For more postcards from the past visit:",
               tags$a(href="www.bjorneri.dk", "www.bjorneri.dk")
      )
    )
  )
)



server <- function(input, output, session){
  
  places <- read_csv("ATT_data.csv")
  
  # make sidebar map whte all the locations, which you can click to select photo
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>%
      addMarkers(p, lng = places$longitude, lat = places$latitude, 
                 popup = as.character(places$place.name), label = as.character(places$place.name),
                 layerId = places$place.name)
  })
  
  place.clicked <- "Trojborgvej50"  # if no clicking yet, start out with this place
  observeEvent(input$mymap_marker_click, { 
    place.clicked <- input$mymap_marker_click$id  # typo was on this line
  
  # prep - change here for different images
  old.photo.file <- places$oldpic[places$place.name == place.clicked]
  new.photo.file <- places$newpic[places$place.name == place.clicked]
  old.year <- places$year[places$place.name == place.clicked]
  new.year <- "2021"
  img.or <- places$orientation[places$place.name == place.clicked]  # set orientation, "landscape" or "portrait"
  
  updateSliderTextInput(session, "img.opacity", "Year shown",
                        choices = c(old.year, seq(0.05, 0.95, 0.05), new.year), 
                        selected = old.year)
  
  # if new pic exists, make photo composite with slider 
  if (!is.na(new.photo.file)) {
    output$myphoto <- renderImage({
      
      # load in photos
      old <- image_read(file.path("www", old.photo.file)) 
      new.overlay <- image_read(file.path("www", new.photo.file))
      
      # change opacity of overlay depending on user input
      my.op <- ifelse(input$img.opacity == old.year, 0, 
                      ifelse(input$img.opacity == new.year, 1, input$img.opacity))
      
      bitmap <- new.overlay[[1]]
      bitmap[4,,] <- as.raw(as.integer(bitmap[4,,]) * my.op)
      new.overlay <- image_read(bitmap)
      
      # make photo composite, in order
      composite <- c(old, new.overlay)
      composite <- image_scale(composite, ifelse(img.or == "landscape", "600", "x600"))
      
      # create a temp file
      tmpfile <- composite %>%
        image_mosaic() %>%
        image_flatten() %>%
        image_write(tempfile(fileext='jpg'), format = 'jpg')
      
      # render the file
      return(list(src = tmpfile,
                  alt = "Aarhus history",
                  contentType = "image/jpg"))
    }, deleteFile = TRUE)
    
  } else {
    output$myphoto <- renderImage({
      old <- image_read(file.path("www", old.photo.file)) 
      old <- image_scale(old, ifelse(img.or == "landscape", "600", "x600"))
      tmpfile <- old %>%
        image_mosaic() %>%
        image_flatten() %>%
        image_write(tempfile(fileext='jpg'), format = 'jpg')
      # render the file
      return(list(src = tmpfile,
                  alt = "Aarhus history",
                  contentType = "image/jpg"))
    }, deleteFile = TRUE)
    
  }
  
  })
  
}

shinyApp(ui, server)

