# load packages
library(shiny)  
library(shinyWidgets) # for sliderTextInput
library(magick)  # for nice image overlay in shiny
#library(rvest)  # website scrape
#library(rgdal)  # dealing with shapefiles
#library(leaflet)  # fancy interactive map


# i got help from here:
# https://stackoverflow.com/questions/53601495/overlaying-images-in-r-shiny


ui <- fluidPage(
  
  titlePanel("Photo Morp Test"),
  fluidRow(align="center",
    imageOutput("myphoto")
  ),
  fluidRow(align="center",
    sliderTextInput("img.opacity", "Year shown",
                choices = c("1906", 1:6, "2021"), 
                selected = "1906")
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
