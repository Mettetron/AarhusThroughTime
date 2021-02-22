# load packages
library(shiny)  
library(leaflet)  # fancy interactive map
library(tidyverse)



# Define unser interfase
ui <- fluidPage(
  
  titlePanel("Aarhus Through Time"),
  
  sidebarLayout(
    sidebarPanel(
      leafletOutput("mymap")
    ),
    
    mainPanel(
      # these are both made in Server to be responsive to input
      uiOutput("mytext"),
      uiOutput("photospace"),
      # CSS style for morphing photo
      tags$head(
        tags$style(HTML(
          "#cf {
          position:relative;
          height:650px;
          margin:0 auto;
          }

          #cf img {
          position:absolute;
          left:0;
          -webkit-transition: opacity 2s ease-in-out;
          -moz-transition: opacity 2s ease-in-out;
          -o-transition: opacity 2s ease-in-out;
          transition: opacity 2s ease-in-out;
          }

          #cf img.top:hover {
          opacity:0;
          }"
          
        ))
      )
    )
  )
)



server <- function(input,output){
  
  places <- read_csv("ATT_data.csv")
  
  # make sidebar map with all the locations, which you can click to select photo
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>%
      addMarkers(p, lng = places$longitude, lat = places$latitude, 
                 popup = as.character(places$place.name), label = as.character(places$place.name),
                 layerId = places$place.name)
  })
  
  
  place.clicked <- "Trojborgvej50"  # if no clicking yet, start out with this place
  old.photo.file <- places$oldpic[places$place.name == place.clicked]
  new.photo.file <- places$newpic[places$place.name == place.clicked]
  old.year <- places$year[places$place.name == place.clicked]
  
  output$mytext <- renderUI( 
    fluidRow(h4(paste0("Postcard from ", old.year, ". Mouse over to see the same place in 2021.")))
  )
  
  output$photospace <- renderUI( 
    withTags({
      div(id="cf",
          img(class="bottom", src=new.photo.file, width="600"),
          img(class="top", src=old.photo.file, width="600")
      )
    })
  )
  
  ##### change everything when map is clicked
  observeEvent(input$mymap_marker_click, { 
    
    place.clicked <- input$mymap_marker_click$id  
    old.photo.file <- places$oldpic[places$place.name == place.clicked]
    new.photo.file <- places$newpic[places$place.name == place.clicked]
    old.year <- places$year[places$place.name == place.clicked]
    img.or <- places$orientation[places$place.name == place.clicked]  # set orientation, "landscape" or "portrait"
    img.width <- ifelse(img.or == "landscape", "600", "450")
    output$photospace <- renderUI(
      
      withTags({
        div(id="cf",
            img(class="bottom", src=new.photo.file, width=img.width),
            img(class="top", src=old.photo.file, width=img.width)
        )
      })
      
    )
    
    output$mytext <- renderUI( 
      fluidRow(h4(paste0("Postcard from ", old.year, ". Mouse over to see the same place in 2021.")))
    )
    
  })
  
}

shinyApp(ui, server)