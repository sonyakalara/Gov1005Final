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

dataset <- readRDS("dataset.rds")

merged <- readRDS("merged.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
        leafletOutput("mymap", height = "350px"),
        sidebarLayout(
          sidebarPanel(
            sliderInput("year", 
                        "Observation Year:", 
                        min = 2010, 
                        max = 2016, 
                        step = 1, 
                        value = 2010,
                        timeFormat = TRUE), 
            radioButtons("type", 
                         "Type of Crime", 
                         choices = c("Property Crimes" = "Property Crimes", 
                                     "Crimes Against Persons" = "Crimes Against Persons"), 
                         selected = "Crimes Against Persons")
          ), 
          
          mainPanel(
            tableOutput("table")
          )
          
        )
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  pal <- colorNumeric(c("white", "red"), 0:1.15)
  
  filtered <- reactive({
    dataset %>% 
      filter(Year == input$year, 
             `Crime Type` == input$type)
    })
    
  shp <- reactive({
    geo_join(newYork, filtered(), "NAME","County", how = "left")
  })
    
  
  
  output$mymap <- renderLeaflet({
    
    # Creates new graph 
    leaflet(shp()) %>%
      addProviderTiles(provider = "CartoDB") %>% 
      addPolygons(layerId = ~NAME,
                  fillOpacity = 0.7,
                  color = "white",
                  fillColor = ~pal(ratio), 
                  dashArray = "3",
                  popup = ~NAME)
  })
  
  observeEvent(input$mymap_shape_click, {
    if(is.null(click))
      return()
    
    else
    {
      click <- input$mymap_shape_click
      output$table <- renderTable({
        table <- dataset %>% 
          filter(Year == input$year, `Crime Type` == input$type) %>% 
          filter(County == click$id) %>% 
          select(ratio)
        return(table)
      })
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

