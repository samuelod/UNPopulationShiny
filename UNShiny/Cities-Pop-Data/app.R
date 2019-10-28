library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)
library(leaflet)
library(rsconnect)
library(readr)
library(shinythemes)

cities_pop <- read_csv('cities_pop.csv')

ui <- fluidPage(
  # add title
  titlePanel("UN Population Visualization"),
  br(),
  sidebarLayout(
    sidebarPanel(
      pickerInput(inputId = "year",
                  label = "Choose Year",
                  choices = unique(cities_pop$year),
                  selected = unique(cities_pop$year),
                  multiple = TRUE,
                  options = list('actions-box' = TRUE, 
                                 'selected-text-format' = "count > 2")
      ),
      pickerInput(inputId = "country",
                  label = "Choose Country",
                  choices = unique(cities_pop$country),
                  selected = unique(cities_pop$country),
                  multiple = TRUE,
                  options = list('actions-box' = TRUE,
                                 'selected-text-format' = "count > 2")
      ),
      sliderInput(inputId = "population", 
                  label = "Population(Thousands)",
                  min = min(cities_pop$population),
                  max = max(cities_pop$population),
                  value = c(min(cities_pop$population), max(cities_pop$population))
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("City Populations",
                 br(),
                 br(),
                 dataTableOutput(outputId = "Table")
        ),
        tabPanel("Map",
                 br(),
                 selectInput(inputId = "map_year",
                             label = "Choose Year",
                             choices = unique(cities_pop$year)
                 ),
                 h4(textOutput(outputId = "MapTitle")),
                 leafletOutput(outputId = "WorldMap")
        ),
        tabPanel("About",
                 br(),
                 tags$h4("What We've Done"),
                 p("This is a Shiny App which uses time series city Population from the ", tags$a(href = "http://data.un.org/Data.aspx?q=population+city&d=POP&f=tableCode%3a240", "UN", target = "_blank"),"dataset. The purpose of this app is to show how populations in major cities change over time and what we expect in the future"),
                 ))
        )
      ),
    
    )


server <- function(input, output) {
  
  df <- reactive({
    cities_pop %>% 
      select(-c(date)) %>% 
      filter(year %in% input$year & 
               country %in% input$country & 
               population >= input$population[1] & population <= input$population[2])
  })
  
  # create map dataframe
  df_map <- reactive({
    cities_pop %>% 
      filter(year == input$map_year)
  })
  
  # create Table
  output$Table <- renderDataTable({
    datatable(df(), options = list(scrollX = TRUE))
  }) 
  
  # create WorldMap
  output$WorldMap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, 
                                     maxZoom = 8, 
                                     maxBounds = list(list(-70, -160), list(70, 160)),
                                     worldCopyJump = FALSE)) %>%
      addTiles() %>% 
      addCircleMarkers(lng = df_map()$longitude, 
                       lat = df_map()$latitude,
                       radius = df_map()$population/1000,
                       color = "#042A58",
                       label = paste0(df_map()$city, ", ", df_map()$country, ", ", df_map()$population))
  })
  
  # create map title
  output$MapTitle <- renderText({
    paste("World City Populations in", input$map_year)
  })
  
}

# run application
shinyApp(ui = ui, server = server)

