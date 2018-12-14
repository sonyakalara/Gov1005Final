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
# population data.

dataset <- readRDS("dataset.rds")

# Defines user interface 
ui <- fluidPage(
  tabsetPanel(
    
    # Provides descriptive context for the project 
    # including guiding questions and links to outside 
    # resources.
    
    tabPanel("Summary", 
             uiOutput("image"), 
             h1("Hate Crimes in New York State, 2010 - 2016"), 
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
                 
                 # Title 
                 h3("Ratio"), 
        
                 # Caption for the sidebar
                 h5("Color of counties corresponds to relative ratio of hate crimes to the total
                    population within that district. Redder counties had relatively more crime per capita
                    than their whiter counterparts. Click on a county to output raw data!"), 
                 
                 # Creates a radio button input to choose observation year
                 radioButtons("year", 
                              "Observation Year:", 
                              choices = c("2010" = 2010,
                                          "2011" = 2011,
                                          "2012" = 2012,
                                          "2013" = 2013,
                                          "2014" = 2014,
                                          "2015" = 2015,
                                          "2016" = 2016)), 
                 # Creates radio buttons to choose type of crime
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
               ), 
    tabPanel("Disaggregated", 
             leafletOutput("populationMap"), 
             sidebarLayout(
               sidebarPanel(
                 h3("Disaggregated"),
                 
                 # Provides radio button input for 
                 # categories of discrimination as well
                 # as types of crimes.
                 
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
                 
                 # Caption for the tab
                 h5("Color of counties corresponds to relative population. Counties that are 
                    bluer are more likely to have statistically significant relationships between
                    hate crimes. Click on one of the counties to output trendlines and raw data!")
                 
                 ), 
               mainPanel(
                 
                 # Outputs the raw data as well as the table
                 # that is generated upon pressing a 
                 # county. 
                 
                 plotOutput("popPlot"), 
                 tableOutput("popTable") 
                 
               )
               )
             ), 
    tabPanel("Total",
             sidebarLayout(
               sidebarPanel(
                 
                 # Provides all possible filteration
                 # values for the original dataset.
                 
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
                                         "Anti-Physical Disability", "Anti-Age*"),
                             multiple = TRUE), 
                 
                 # Caption for this tab
                 h5("This graph represents trends in hate crimes across the entirety of New York
                    across time. Select variables to analyze those discrimination types!")
                 ), 
               
               # Generates panel for graph of NY-wide trends
               mainPanel(
                 plotOutput("totalGraphs")
               )
               )             
             
    ), 
    tabPanel("Parameters",
             
             # Paragraph section for describing the 
             # limitations of the data analysis on this
             # information. 
             
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
  
  # Fetches an image of hate crime art
  # from online and produces it on the summary page.
  
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
    
    # Displays a table corresponding to the 
    # raw data within the county in the given
    # year. 
    
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
  
  
  ## SERVER SECTION FOR SECOND TAB BEGINS HERE 
  
  # Defines variable to provide a gradient
  # based on population density. 
  
  palPop <- colorNumeric(c("white", "blue"), 0:2504700)
  
  filteredPop <- reactive({
    dataset %>% 
      filter(`Crime Type` == input$secondtype)
  })
  
  # Binds only the relevant filtered data
  # to the shapefile so that counties
  # are colored according to their distinct
  # population values and counties without 
  # information for one of the two different crime
  # types are colored in black. 
  
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
    
    
    else
    {
      # Creates, but does not display, a table corresponding to the 
      # county-wide trends for different discrimination 
      # groups. 
      
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
        
        # Generates the table that is associated with
        # the raw data in that county. 
        
        tabletwo <- table %>% 
          filter(discrimination %in% data) %>% 
          select(Year, discrimination, number) %>% 
          arrange(Year)
        
        return(tabletwo)
      })
      
      # Displays the original plot 
      # for the county wide trends. This 
      # looks a lot like the code in the previous
      # function because it is simply outputting
      # to the UI the trendlines using ggplot. 
      
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
  
  ## SECTION FOR THIRD TAB BEGINS HERE
  
  output$totalGraphs <- renderPlot({
    
    # Makes the data tidy before defining
    # totals across all counties in a given
    # year and filtering the dataset
    # by the user's selections. 
    
    total <- dataset %>%
      gather(key = discrimination, 
             value = number,
             `Anti-Male`:`Anti-Mental Disability`) %>%
      group_by(discrimination, Year) %>% 
      mutate(total = sum(number)) %>% 
      select(discrimination, total, Year) %>% 
      unique() %>% 
      filter(discrimination %in% input$all_types)
    
    # Plots the results with a linear regression
    
    trend <- ggplot(total, aes(x = Year, y = total)) +
      geom_point() +
      geom_line(aes(group = discrimination, color = discrimination)) +
      geom_smooth(method = lm, se = FALSE) +
      theme(legend.position="bottom")
    
    return(trend)
  })
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)