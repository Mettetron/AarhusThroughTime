# load packages
library(shiny)  
library(magick)  # for nice image overlay in shiny
#library(rvest)  # website scrape
#library(rgdal)  # dealing with shapefiles
#library(leaflet)  # fancy interactive map



# https://stackoverflow.com/questions/53601495/overlaying-images-in-r-shiny
ui <- fluidPage(
  
  titlePanel("Photo Morp Test"),
  fluidRow(align="center",
    imageOutput("myphoto")
  ),
  fluidRow(align="center",
    sliderInput("img.opacity", "Overlay opacity",
                ticks = FALSE,
                min = 0, max = 7,
                value = 0, step = 1),
  )
)



server <- function(input,output){
  
  output$myphoto <- renderImage({

    old <- image_read(file.path("www", "Trojborgvej50_1906.png"))
    mynew <- image_read(file.path("www", paste0("Trojborgvej_", input$img.opacity, ".png")))

    # make the composite, in order
    myphoto <- c(old, mynew)
    
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
