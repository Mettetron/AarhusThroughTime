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
                      top = 75, right = 75, width = 730, fixed=TRUE,
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
                      htmlOutput("blabla")
                      
        ),
        
        # github and postcard links in lower right
        absolutePanel(id = "links",
                      bottom = 20, right = 10, width = 100, fixed=TRUE,
                      draggable = FALSE, height = 46,
                      HTML('<div style="float: left;"><p><a href="https://github.com/Mettetron/AarhusThroughTime">
                  <img src="GitHubLogo.png" width="30" height="30" title="GitHub repository"/></a></p></div>'),
                      HTML('<div style="position: relative; margin-left:40px;"><p><a href="https://www.bjorneri.dk">
                  <img src="postcardLogo.png" height="30" title="Source of postcards"/></a></p></div>')
                      ),
           
                        
        
        # info button in upper left - button click brings out info panel
        absolutePanel(top = 12, left = 50, width = 50, fixed=TRUE,
                      draggable = FALSE, height = 50,
                      div(
                        actionButton(
                          inputId = "button_showInfo", 
                          icon = icon("info-circle"),
                          label = "",
                          style='padding:5px; font-size:250%; 
                          height:60px; width:60px; 
                          background-color: white; color: #4CAF50; 
                          border-color: darkgrey;'
                        )
                      )
        ),
        uiOutput(outputId = "myAppInfo")
    )
  )
)
             
             
             
  


server <- function(input,output){

  # save as txt from ecxel -> open with sublime text -> save with encoding UTF-8
  places <- read_tsv("ATT_data.txt")
  # text unidentified
  places$place.text[is.na(places$place.text)] <- " "
  
  # make map icons with color and symbol depending on the existence of new photo and direction of view
  dir.ang.df <- data.frame(dir=c(NA, "N", "NW", "W", "SW", "S", "SE", "E", "NE"),
                           ang=c(0, 0, 315, 270, 225, 180, 135, 90, 45))
  
  icons <- awesomeIcons(
    icon = ifelse(is.na(places$direction), 'times-circle', 'arrow-circle-up'),
    iconRotate = dir.ang.df$ang[match(places$direction, dir.ang.df$dir)],
    iconColor = 'black',
    library = 'fa',
    markerColor = ifelse(places$newpic == "placeholder.png", "red", "blue")
  )

  # make background map with all the locations, which you can click to select photo
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap=0.1)) %>% 
      addTiles(group = "OSM (default)") %>%
      addAwesomeMarkers(lng = places$longitude, lat = places$latitude, icon = icons,
                        popup = as.character(places$nice.name), label = as.character(places$nice.name),
                        layerId = places$place.name) %>%
      setView(10.247834136581673, 56.15996939526574, zoom=13.8)
  })
  
  # Choose initial location, but make the variable reactive 
  place.clicked  <- reactiveVal("Trojborgvej50")
  
  # change the location variable when map marker is clicked
  observeEvent(input$mymap_marker_click, {
    newPlace <- input$mymap_marker_click$id     
    place.clicked(newPlace)             
  })
  
  # make photos (overlayed)
  output$myphoto <- renderUI({
    old.photo.file <- places$oldpic[places$place.name == place.clicked()]
    new.photo.file <- places$newpic[places$place.name == place.clicked()]
    old.year <- places$year[places$place.name == place.clicked()]
    img.or <- places$orientation[places$place.name == place.clicked()]  # set orientation, "landscape" or "portrait"
    img.width <- ifelse(img.or == "landscape", "700", "500")
    withTags({
      div(id="photo",
          img(class="bottom", src=old.photo.file, width=img.width),
          img(class="top", src=new.photo.file, width=img.width)
      )
    })
  })
  
  # make text below photo
  output$blabla <- renderUI({
    str1 <- paste(places$nice.name[places$place.name == place.clicked()], places$year[places$place.name == place.clicked()], "- 2021")
    str2 <- places$place.text[places$place.name == place.clicked()]
    xtraimg <- places$xtraimg[places$place.name == place.clicked()]
    xtraimgtxt <- places$xtraimgtxt[places$place.name == place.clicked()]
    
    
    withTags({
      div(id="infotext",
          tags$script(src = "imageModal.js"),
          HTML(paste(str1, str2, sep = '<br/>')),
          if (!is.na(xtraimg)) {
            HTML(paste('
              <br/>
              <!-- Trigger the Modal -->
              <img id="myImg" src=', xtraimg, 'style="width:100%;max-width:30px">
          
              <!-- The Modal -->
              <div id="myModal" class="modal">
            
                <!-- The Close Button -->
                <span class="close">&times;</span>

                <!-- Modal Content (The Image) -->
                <img class="modal-content" id="img01">

                <!-- Modal Caption (Image Text) -->
                <div id="caption"><body>', xtraimgtxt, '</body></div>
              </div>'))
            
          }
              

      )
    })
  })

  
  # show app info when info button clicked
  observeEvent(input$button_showInfo, {
    output$myAppInfo <- renderUI({
      absolutePanel(id = "appInfo",
                    top = 10, left = 48, width = 800, fixed=TRUE,
                    draggable = FALSE, height = "auto",
                    actionButton(
                      inputId = "button_hideInfo",
                      icon = icon("times-circle"),
                      label = "",
                      style = 'padding:5px; font-size:250%;
                        height:60px; width:60px;
                        background-color: white; color: #555555;
                        border-color: darkgrey;
                        float:left;'
                    ),
                    h3(id = "appInfoText", 
                       style = 'font-weight: bold;',
                       "Aarhus Through Time"),
                    h5(id = "appInfoText",
                       "Click a marker on the map - Move the slider under the image - Enjoy your time travel!"),
                    h6(id = "appInfoText",
                       "Red map markers indicate locations that still do not 
                       have a present-day photo. I'm working on it. 
                       I'm also still working on adding some text for each location. See 'Huset' and 'Kollegier'.")
                    )
    })
  })
  
  # when X button clicked, info panel is removed (repleced with empty ui)
  observeEvent(input$button_hideInfo, {
    output$myAppInfo <- renderUI({})
  })
  
}

shinyApp(ui, server)
