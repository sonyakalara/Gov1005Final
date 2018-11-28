#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(urbnmapr)
library(haven)
library(fs)
library(readxl)
library(lubridate)
library(kableExtra)
library(janitor)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(tidyverse)
library(moderndive)
library(tigris)

newYork <- readRDS("newYork.rds")

hateCrimes <- read_csv("Hate_Crimes_by_County_and_Bias_Type__Beginning_2010.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
        leafletOutput("mymap", height = "350px")
      )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$mymap <- renderLeaflet({
    leaflet(newYork) %>%
      addProviderTiles(provider = "CartoDB") %>% 
      addPolygons(layerId = newYork$NAME,
                  popup = newYork$NAME)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

