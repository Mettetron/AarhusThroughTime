# load packages
library(shiny)  
library(leaflet)  # fancy interactive map
library(tidyverse)



# Define unser interfase
ui <- bootstrapPage(
  title = "Aarhus Through Time", 
  fluidRow(
    div(class="outer",
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css")),
        tags$script(src = "sliderFade.js"),
        
        # background map
        leafletOutput("mymap", width="100%", height="100%"),
        
        # photobox
        absolutePanel(id = "photobox",
                      top = 75, right = 75, width = 830, fixed=TRUE,
                      draggable = TRUE, height = "auto",
                      
                      # photo
                      uiOutput("myphoto"),
                      
                      # slider
                      withTags({
                        div(id="contrastSlider",
                            input(id="contrast", type="range", class="slider", value="0",
                                  max="1", min="0", step="0.01")
                        )
                      }),
                      
                      # text
                      span(tags$i(h6(textOutput("blabla"))), style="color:#045a8d; position:relative; left:10px; top:10px; width:80%;")
                      
                      
        ),
        
        # github and postcard links in lower right
        absolutePanel(id = "linkz",
                      bottom = 20, right = 10, width = 150, fixed=TRUE,
                      draggable = FALSE, height = "auto",
                      
         
                      fluidRow(
                        column(6,
                               HTML('<div style="text-align:center;"><p><a href="https://github.com/Mettetron/AarhusThroughTime">
                  <img src="GitHubLogo.png" width="36" height="36"/></a></p>
                                    <h6>GitHub repository</h6> </div>')),
                      column(6,
                             HTML('<div style="text-align:center;"><p><a href="https://www.bjorneri.dk">
                  <img src="postcardLogo.png" height="36" /></a></p>
                                  <h6>Source of postcards</h6> </div>')
                             ))
                      
          
        )
        
        
    )
    
  )
)
             
             
             
  


server <- function(input,output){

  # save as txt from ecxel -> open with sublime text -> save with encoding UTF-8
  places <- read_tsv("ATT_data.txt")
  
  # make map icons with color depending on the existence of new photo
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = ifelse(places$newpic == "placeholder.png", "red", "blue")
  )

  # make sidebar map with all the locations, which you can click to select photo
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>%
      addAwesomeMarkers(lng = places$longitude, lat = places$latitude, icon = icons,
                        popup = as.character(places$nice.name), label = as.character(places$nice.name),
                        layerId = places$place.name) %>%
      setView(10.247834136581673, 56.15996939526574, zoom=14)
  })
  
  
  place.clicked <- "Trojborgvej50"  # if no clicking yet, start out with this place
  old.photo.file <- places$oldpic[places$place.name == place.clicked]
  new.photo.file <- places$newpic[places$place.name == place.clicked]
  old.year <- places$year[places$place.name == place.clicked]
  img.or <- places$orientation[places$place.name == place.clicked]  # set orientation, "landscape" or "portrait"
  img.width <- ifelse(img.or == "landscape", "800", "650")
  output$blabla <- renderText(paste(places$nice.name[places$place.name == place.clicked], places$year[places$place.name == place.clicked]))
  
  
  output$myphoto <- renderUI( 
    withTags({
      div(id="photo",
          img(class="bottom", src=old.photo.file, width=img.width),
          img(class="top", src=new.photo.file, width=img.width)
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
    img.width <- ifelse(img.or == "landscape", "800", "650")
    output$blabla <- renderText(paste(places$nice.name[places$place.name == place.clicked], places$year[places$place.name == place.clicked]))
    
    
    output$myphoto <- renderUI( 
      withTags({
        div(id="photo",
            img(class="bottom", src=old.photo.file, width=img.width),
            img(class="top", src=new.photo.file, width=img.width)
        )
      })
    )
    
    
  })
  
}

shinyApp(ui, server)
