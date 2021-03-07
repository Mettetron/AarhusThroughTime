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
                      htmlOutput("blabla")
                      
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
                      
          
        ),
        
        # info button in upper left - button click brings out info panel
        absolutePanel(top = 12, left = 50, width = 50, fixed=TRUE,
                      draggable = FALSE, height = 50,
                      div(
                        actionButton(
                          #class = "btn-primary",
                          inputId = "button_showInfo", 
                          icon = icon("info-circle"),
                          label = "",
                          style='padding:5px; font-size:250%; 
                          height:60px; width:60px; 
                          background-color: white; color: #555555; 
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
  # remove currently unwanted locations
  places <- places[places$use == "yes", ]
  
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
  str1 <- paste(places$nice.name[places$place.name == place.clicked], places$year[places$place.name == place.clicked], "- 2021")
  str2 <- places$place.text[places$place.name == place.clicked]
  output$blabla <- renderUI({
    withTags({
      div(id="infotext",
          HTML(paste(str1, str2, sep = '<br/>'))
      )
    })
  })

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
    
    str1 <- paste(places$nice.name[places$place.name == place.clicked], places$year[places$place.name == place.clicked], "- 2021")
    str2 <- places$place.text[places$place.name == place.clicked]
    output$blabla <- renderUI({
      withTags({
        div(id="infotext",
            HTML(paste(str1, str2, sep = '<br/>'))
        )
      })
    })
    output$myphoto <- renderUI( 
      withTags({
        div(id="photo",
            img(class="bottom", src=old.photo.file, width=img.width),
            img(class="top", src=new.photo.file, width=img.width)
        )
      })
    )
  })
  
  
  # showw app info when info button clicked
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
                    h3(id = "appInfoText", "Aarhus Through Time"),
                    h5(id = "appInfoText",
                       "Click a marker on the map."),
                    h5(id = "appInfoText",
                       "Move the slider under the image"),
                    h5(id = "appInfoText",
                       "Enjoy your time travel"),
                    
                    )
                           
                       
                  
                          
                    # h3("Aarhus Through Time", 
                    #    style='margin-left: 70px; margin-top: 20px; 
                    #    position: absolute;'),
                    # h6("bla", style='margin-left: 70px; position: relative;')
                      
                   #h6("hej")
                   # ),
                   # fluidRow(h3("blablbalablab"))
                      
      
    })
  })
  
  # when hide button clicked, info panel is removed (repleced with empty ui)
  observeEvent(input$button_hideInfo, {
    output$myAppInfo <- renderUI({})
  })
  
  
}

shinyApp(ui, server)
