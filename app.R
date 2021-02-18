# load packages
library(shiny)  
library(shinyWidgets) # for sliderTextInput
library(magick)  # for nice image overlay in shiny
#library(rvest)  # website scrape
#library(rgdal)  # dealing with shapefiles
#library(leaflet)  # fancy interactive map


# i got help from here:
# https://stackoverflow.com/questions/53601495/overlaying-images-in-r-shiny


# prep - change here for different images
old.photo.file <- "Trojborgvej50_1906.png"
new.photo.file <- "Trojborgvej50_2021.png"
old.year <- "1906"
new.year <- "2021"
  
# set up user interface
ui <- fluidPage(
  
  titlePanel("Photo Morp Test"),
  fluidRow(align="center",
    imageOutput("myphoto")
  ),
  fluidRow(align="center",
    sliderTextInput("img.opacity", "Year shown",
                choices = c(old.year, "mix1", "mix2", "mix3", new.year), 
                selected = old.year)
  )
)



server <- function(input,output){
  
  output$myphoto <- renderImage({
    
    # load in photos
    old <- image_read(file.path("www", old.photo.file)) 
    new.overlay <- image_read(file.path("www", new.photo.file))
   
    # change opacity of overlay depending on user input
    my.op <- ifelse(input$img.opacity == old.year, 0, 
                    ifelse(input$img.opacity == new.year, 1,
                           ifelse(input$img.opacity == "mix1", 0.3, 
                                  ifelse(input$img.opacity == "mix2", 0.5, 0.7))))
    bitmap <- new.overlay[[1]]
    bitmap[4,,] <- as.raw(as.integer(bitmap[4,,]) * my.op)
    new.overlay <- image_read(bitmap)
    
    # make photo composite, in order
    myphoto <- c(old, new.overlay)
    
    # create a temp file
    tmpfile <- myphoto %>%
      image_mosaic() %>%
      image_flatten() %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    
    # render the file
    return(list(src = tmpfile,
                height = 401,
                width = 631,
                alt = "Your photo",
                contentType = "image/jpg"))
  }, deleteFile = TRUE)
}

shinyApp(ui, server)

