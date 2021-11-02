library(leaflet)
library(jsonlite)
library(shiny)
library(tidyverse)

#Set the path where the data is located
#setwd("")

ui <- fluidPage(
  selectInput("dato", "Archivo: ",
              c("Atm"="Zona_T_atm.json",
                "Bank"="Zona_T_bank.json",
                "Bar"="Zona_T_bar.json",
                "Cafe"="Zona_T_cafe.json",
                "Casino"="Zona_T_casino.json",
                "Clothing Store"="Zona_T_clothing_store.json",
                "Gym"="Zona_T_gym.json",
                "Night Club"="Zona_T_night_club.json",
                "Restaurant"="Zona_T_Restaurant.json",
                "Store"="Zona_T_store.json")),
  sliderInput(inputId = "day", label = "Day: ", value = 7, min = 1, max = 7),
  sliderInput(inputId = "hour", label = "Hour: ", value = 24, min = 1, max = 24),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  observe({
    
    data <- fromJSON(str_c("C:/Users/anes_/Documents/SEKIA/Proyecto epidemiologia/Codigo/",input$dato))
    heat <- c()
    
    colorTest <- function(x){
      ifelse(x==0, color <- "black", ifelse(x<30, color <- "green", ifelse(x<60, color <- "yellow", ifelse(x<90, color <- "orange", ifelse(x<=100, color <- "red")))))
      color
    }
    
    if(!is.null(data$populartimes)){
      for (n in seq(1,length(data$populartimes),1)) {
        for (p in data$populartimes[[n]]["data"]) {
          heat <- c(heat, p[[input$day]][input$hour])
        }
      }
      
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(lng=data$coordinates$lng, lat=data$coordinates$lat, popup=data$name, color = 'black', fillColor = colorTest(heat), radius = 20)
      })
      
    } else{
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(lng=data$coordinates$lng, lat=data$coordinates$lat, popup=data$name)
      })
    }
  })
  

  
}

shinyApp(ui, server)
