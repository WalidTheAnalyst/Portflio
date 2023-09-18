library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)
library(shinyjs)

#setwd("/home/jovyan/Portfolio/Projects")

italy_data <- read_csv("covid19_italy_region.csv")
# Convert italy_data to a data frame
italy_data <- as.data.frame(italy_data)

# Converting the date column to a Date object
italy_data$Date <- as.Date(italy_data$Date)

# Formatting the date column in the desired format (mm.dd.yyyy)
italy_data$Date <- format(italy_data$Date, format = "%m.%d.%Y")

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(
    dateInput("start_date", "Select Start Date Between 2020-02-24 and 2020-12-06:", value = min(italy_data$Date)),
    dateInput("end_date", "Select End Date Between 24/02/2020 and 06/12/2020:", value = max(italy_data$Date)),
    actionButton("click_button", "Click Me", style = "display:none;")  # Hidden initially
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "COVID-19 Deaths by Region",
        width = 12,
        leafletOutput("italy_map")
      )
    )
  )
)


server <- function(input, output, session) {
  shinyjs::useShinyjs()
  
  observe({
    if (!is.null(input$start_date) && !is.null(input$end_date)) {
      shinyjs::show("click_button")
    }
  })
  # Create a reactive dataset filtered by date range and region
  filtered_data <- reactive({
    start_date <- format(input$start_date, format = "%m.%d.%Y")
    end_date <- format(input$end_date, format = "%m.%d.%Y")
    
    filtered <- italy_data %>%
      filter(Date >= start_date, Date <= end_date)
    
    return(filtered)
  })
  
  # Render the leaflet map
  output$italy_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 12.4964, lat = 41.9028, zoom = 6) %>%
      addCircleMarkers(
        data = filtered_data(),
        lng = ~Longitude,
        lat = ~Latitude,
        label = ~Deaths
      )
  })
}

shinyApp(ui, server)



