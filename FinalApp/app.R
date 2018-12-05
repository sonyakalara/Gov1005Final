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

# Loads dataset from RDS object for shapefiles
newYork <- readRDS("newYork.rds")

# Loads dataset created by merging hate crime and
# population data 

dataset <- readRDS("dataset.rds")

# Defines user interface 
ui <- fluidPage(
        tabsetPanel(
          tabPanel("Summary", 
                   uiOutput("image"), 
                   h1("Hate Crimes in New York City 2010 - 2016"), 
                   h3("Ratio - First Question"), 
                   h5("The ratio tab seeks to figure out when sudden upticks
                      in hate crime density occur in any given county by adjusting
                      the number of hate crimes by the population."), 
                   h3("Disaggregated - Second Question"), 
                   h5("The disaggregated tab seeks to understand what trends
                      exist for different categories of discrimination for any 
                      given county."), 
                   h3("Total - Third Question"),
                   h5("The total tab seeks to explore how trends appear when
                      the entirety of the state is examined rather than individual 
                      counties. This tab divides the data into every specific kind of
                      discrimination rather than pockets like in the disaggregated tab.") 
                   ),
          tabPanel("Ratio",  
            leafletOutput("mymap", height = "350px"),
            sidebarLayout(
              sidebarPanel(
                
                # Creates a slider to choose observation year
                sliderInput("year", 
                            "Observation Year:", 
                            min = 2010, 
                            max = 2016, 
                            step = 1, 
                            value = 2010,
                            timeFormat = TRUE), 
                
                # Creates radio buttons to choose type of crime
                radioButtons("type", 
                             "Type of Crime", 
                             choices = c("Property Crimes" = "Property Crimes", 
                                         "Crimes Against Persons" = "Crimes Against Persons"), 
                             selected = "Crimes Against Persons"), 
                h5("Color of counties corresponds to relative ratio of hate crimes to the total
                   population within that district. Redder counties had relatively more crime per capita
                   than their whiter counterparts.")
              ), 
              mainPanel(
                tableOutput("table")
              )
            )
          ), 
          tabPanel("Disaggregated", 
            leafletOutput("populationMap"), 
            sidebarLayout(
              sidebarPanel(
                radioButtons("group", 
                             "Discrimination Category",
                             choices = c("Gender" = 1, 
                                         "Race / Ethnicity" = 2,
                                         "Religion" = 3, 
                                         "Sexuality" = 4, 
                                         "Age / Disability" = 5)), 
                radioButtons("secondtype", 
                             "Type of Crime", 
                             choices = c("Property Crimes" = "Property Crimes", 
                                         "Crimes Against Persons" = "Crimes Against Persons"), 
                             selected = "Crimes Against Persons"), 
                h5("Color of counties corresponds to relative population. Counties that are 
                   bluer are more likely to have statistically significant relationships between
                   hate crimes. ")
                  
                ), 
              mainPanel(
                plotOutput("popPlot"), 
                tableOutput("popTable") 
                
              )
              )
            ), 
          tabPanel("Total",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("all_types", "Discrimination Type", 
                                   choices = c("Anti-Male", "Anti-Female", "Anti-Transgender", 
                                               "Anti-Gender Identity Expression", "Anti-White", 
                                               "Anti-Black", "Anti-American Indian/Alaskan Native", 
                                               "Anti-Asian", "Anti-Native Hawaiian/Pacific Islander", 
                                               "Anti-Multi-Racial Groups" , "Anti-Other Race", 
                                               "Anti-Hispanic", "Anti-Arab", "Anti-Other Ethnicity/National Origin",
                                               "Anti-Non-Hispanic*", "Anti-Jewish", "Anti-Catholic", "Anti-Protestant", 
                                               "Anti-Islamic (Muslim)", "Anti-Multi-Religious Groups",
                                               "Anti-Atheism/Agnosticism", "Anti-Religious Practice Generally",
                                               "Anti-Other Religion", "Anti-Buddhist", "Anti-Eastern Orthodox (Greek, Russian, etc.)", 
                                               "Anti-Hindu", "Anti-Jehovahs Witness", "Anti-Mormon", 
                                               "Anti-Other Christian", "Anti-Sikh", 
                                               "Anti-Gay Male", "Anti-Gay Female", "Anti-Gay (Male and Female)", 
                                               "Anti-Heterosexual", "Anti-Bisexual", "Anti-Mental Disability", 
                                               "Anti-Physical Disability", "Anti-Age*")), 
                       h5("These graphs represent trends in hate crimes across the entirety of New York
                          across time. ")
                     ), 
                    
                  mainPanel(
                    plotOutput("totalGraphs")
                  )
                  )             
            
          ), 
          tabPanel("Insights", 
                   h1("Analytical Context"), 
                   p("The biggest issue facing analysis of this dataset is that there are small counties
                     and there are types of discrimination that are less common because the affected group is 
                     a smaller share of the population. These two trends result in numbers that are not statistically
                     significant because it is fallacious to make generalized claims about trends when there are very
                     few instances to begin with and therefore very few data points associated with them. It is easier
                     to discover trends in counties that are population drivers as well as types of hate crimes that are
                     more common and therefore have more substantive variation in data.")
          )
          )
          )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$image = renderUI({
    tags$img(src = "https://static1.squarespace.com/static/5b10b6968ab722b1af17a9ca/t/5b2134d588251b936fa55067/1528902920603/HateCrimesBanner_1.png", width = "100%")})
  
  # Colorvariable for map that gets redder as ratio 
  # gets larger. 
  
  pal <- colorNumeric(c("white", "red"), 0:1.15)
  
  # Reactive expression that filters the dataset
  # by the year and crime type chosen in the user
  # interface. Recalculated any time either variable
  # changes. 
  
  filtered <- reactive({
    dataset %>% 
      filter(Year == input$year, 
             `Crime Type` == input$type)
    })
    
  # Binds only the relevant filtered data
  # to the shapefile so that counties
  # are colored according to their distinct
  # ratio values later on. 
  
  shp <- reactive({
    geo_join(newYork, filtered(), "NAME","County", how = "left")
  })
    
  # Generates new map 
  output$mymap <- renderLeaflet({
    
    leaflet(shp()) %>%
      addProviderTiles(provider = "CartoDB") %>% 
      addPolygons(layerId = ~NAME,
                  fillOpacity = 0.9,
                  color = "black",
                  fillColor = ~pal(ratio), 
                  dashArray = "3",
                  popup = ~NAME)
  })
  
  # Defines events to occur when map is clicked
  observeEvent(input$mymap_shape_click, {
    if(is.null(click))
      return()
    
    # Displays a table
    else
    {
      click <- input$mymap_shape_click
      output$table <- renderTable({
        table <- filtered() %>% 
          filter(`Crime Type` == input$type) %>% 
          filter(County == click$id) %>% 
          gather(key = discrimination, value = number,
                 `Anti-Male`:`Anti-Mental Disability`) %>% 
          select(discrimination, number)
        
        return(table)
      })
    }
  })
  
  
  ## SECTION FOR SECOND TAB BEGINS HERE 
  
  palPop <- colorNumeric(c("white", "blue"), 0:2504700)
  
  filteredPop <- reactive({
    dataset %>% 
      filter(`Crime Type` == input$secondtype)
  })
  
  # Binds only the relevant filtered data
  # to the shapefile so that counties
  # are colored according to their distinct
  # ratio values later on. 
  
  shpPop <- reactive({
    geo_join(newYork, filteredPop(), "NAME","County", how = "left")
  })
    
    ## Creates second map for regressions table
    output$populationMap <- renderLeaflet({
      
      leaflet(shpPop()) %>%
        addProviderTiles(provider = "CartoDB") %>% 
        addPolygons(layerId = ~NAME,
                    fillOpacity = 0.9,
                    color = "black",
                    fillColor = ~palPop(Population), 
                    dashArray = "3",
                    popup = ~NAME)
    })
    
    # Defines events to occur when map is clicked
    observeEvent(input$populationMap_shape_click, {
      if(is.null(click))
        return()
      
      # Displays a table
      else
      {
        select <- input$populationMap_shape_click
        output$popTable <- renderTable({
          table <- dataset %>% 
            filter(`Crime Type` == input$secondtype) %>% 
            filter(County == select$id) %>% 
            gather(key = discrimination, 
                   value = number,
                   `Anti-Male`:`Anti-Mental Disability`)
          
          if (input$group == 1)
            data = c("Anti-Male", "Anti-Female", "Anti-Transgender", "Anti-Gender Identity Expression")
          else if (input$group == 2)
            data = c("Anti-White", "Anti-Black", "Anti-American Indian/Alaskan Native", 
                     "Anti-Asian", "Anti-Native Hawaiian/Pacific Islander", "Anti-Multi-Racial Groups", 
                     "Anti-Other Race", "Anti-Hispanic", "Anti-Arab", "Anti-Other Ethnicity/National Origin",
                     "Anti-Non-Hispanic*")
          else if (input$group == 3)
            data = c("Anti-Jewish", "Anti-Catholic", "Anti-Protestant", 
                     "Anti-Islamic (Muslim)", "Anti-Multi-Religious Groups",
                     "Anti-Atheism/Agnosticism", "Anti-Religious Practice Generally",
                     "Anti-Other Religion", "Anti-Buddhist", "Anti-Eastern Orthodox (Greek, Russian, etc.)", 
                     "Anti-Hindu", "Anti-Jehovahs Witness", "Anti-Mormon", "Anti-Other Christian", "Anti-Sikh")
          else if (input$group == 4)
            data = c("Anti-Gay Male", "Anti-Gay Female", "Anti-Gay (Male and Female)", 
                     "Anti-Heterosexual", "Anti-Bisexual")
          else if (input$group == 5)
            data = c("Anti-Mental Disability", "Anti-Physical Disability", "Anti-Age*")
          
          tabletwo <- table %>% 
            filter(discrimination %in% data) %>% 
            select(Year, discrimination, number) %>% 
            arrange(Year)
          
          return(tabletwo)
        })
        
        ## UNCOMPRESSED TABLE GENERATION
        
        output$popPlot <- renderPlot({
          
          table <- dataset %>% 
            filter(`Crime Type` == input$secondtype) %>% 
            filter(County == select$id) %>% 
            gather(key = discrimination, 
                   value = number,
                   `Anti-Male`:`Anti-Mental Disability`)
          
          if (input$group == 1)
            data = c("Anti-Male", "Anti-Female", "Anti-Transgender", "Anti-Gender Identity Expression")
          else if (input$group == 2)
            data = c("Anti-White", "Anti-Black", "Anti-American Indian/Alaskan Native", 
                     "Anti-Asian", "Anti-Native Hawaiian/Pacific Islander", "Anti-Multi Racial Groups", 
                     "Anti-Other Race", "Anti-Hispanic", "Anti-Arab", "Anti-Other Ethnicity/National Origin",
                     "Anti-Non-Hispanic*")
          else if (input$group == 3)
            data = c("Anti-Jewish", "Anti-Catholic", "Anti-Protestant", 
                     "Anti-Islamic (Muslim)", "Anti-Multi-Religious Groups",
                     "Anti-Atheism/Agnosticism", "Anti-Religious Practice Generally",
                     "Anti-Other Religion", "Anti-Buddhist", "Anti-Eastern Orthodox (Greek, Russian, etc.)", 
                     "Anti-Hindu", "Anti-Jehovahs Witness", "Anti-Mormon", "Anti-Other Christian", "Anti-Sikh")
          else if (input$group == 4)
            data = c("Anti-Gay Male", "Anti-Gay Female", "Anti-Gay (Male and Female)", 
                     "Anti-Heterosexual", "Anti-Bisexual")
          else if (input$group == 5)
            data = c("Anti-Mental Disability", "Anti-Physical Disability", "Anti-Age*")
          
          tabletwo <- table %>% 
            filter(discrimination %in% data)
          
          graph <- ggplot(tabletwo, aes(x=Year, y=number, color = discrimination)) + 
            geom_line(aes(group = discrimination)) + 
            geom_point(aes(group = discrimination)) 
          
          return(graph)
        })
        
      }
      })
    
    
    output$totalGraphs <- renderPlot({
      
      total <- dataset %>%
        gather(key = discrimination, 
               value = number,
               `Anti-Male`:`Anti-Mental Disability`) %>%
        group_by(discrimination, Year) %>% 
        mutate(total = sum(number)) %>% 
        select(discrimination, total, Year) %>% 
        unique() %>% 
      filter(discrimination == input$all_types)
      
      trend <- ggplot(total, aes(x = Year, y = total)) +
        geom_point() +
        geom_line() +
        geom_smooth(method = lm, se = TRUE)
      
      return(trend)
    })

  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

