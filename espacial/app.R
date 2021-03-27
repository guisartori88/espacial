# install.packages('rsconnect')

library(shiny)
library(leaflet)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(RSocrata)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))



crash <- crash_raw %>%
    arrange(desc(crash_date)) %>%
    transmute(
        injuries = if_else(injuries_total > 0, "Injuries", "None"),
        crash_date,
        crash_hour,
        report_type = if_else(report_type == "", "UNKNOWN", report_type),
        num_units,
        posted_speed_limit,
        weather_condition,
        lighting_condition,
        roadway_surface_cond,
        first_crash_type,
        trafficway_type,
        prim_contributory_cause,
        latitude, longitude
    ) %>%
    na.omit()
crash <- crash %>%   mutate(
    crash_hour_cat=case_when(crash_hour<=12~ 'AM',
                             crash_hour>12~ 'PM'))

# ui object
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(
        title = "Lista 2 - Estatistica Espacial"
    ),
    dashboardSidebar(   
        sidebarMenu(
            selectInput(
                inputId = "select_injure",
                label = "Select",
                choices = c("Injuries", "None")
            ),
            menuItem("Analise de acidentes no transito de Chicago", tabName = "dashboard", icon = icon("map-pin"))),
        checkboxGroupInput(
            "crash_hour_cat_ampm",
            "Turno do Dia:",
            c("AM" = "AM",
              "PM" = "PM"
            ),
            selected = c("AM","PM")
        ),
        checkboxGroupInput(
            "iluminacao",
            "Condicoes de iluminacao:",
            c("Luz do dia" = "DAYLIGHT",
              "Amanhecer" = "DAWN",
              "Escuro" = "DARKNESS",
              "Escuro com iluminacao na pista" = "DARKNESS, LIGHTED ROAD",
              "Anoitecer" = "DUSK",
              "Desconhecido" = "UNKNOWN"),
            selected = c("DAYLIGHT","DAWN","DARKNESS","DARKNESS, LIGHTED ROAD","DUSK","UNKNOWN")
        )
    ),
    #Corpo
    dashboardBody(
        leafletOutput(outputId = "map", height = 800)
    )
)

# server object
server <- function(input, output, session) {
    
    
    
    # make palette
    pal <- colorFactor(c("red", "blue"), domain = c("Injuries", 
                                                    "None"))
    
    output$map <- renderLeaflet({
        
        crash2 = crash[which(crash$injuries==input$select_injure),]
        nomes <- input$iluminacao
        crash2 = crash2[which(crash2$lighting_condition %in% nomes),]
        
        AMPM <- input$crash_hour_cat_ampm
        crash2 = crash2[which(crash2$crash_hour_cat %in% AMPM),]
        leaflet(crash2) %>%
            addProviderTiles(providers$OpenStreetMap.HOT) %>%
            addMarkers(lng = ~longitude,
                       lat = ~latitude,
                       clusterOptions = markerClusterOptions()
                       
                       
            )
    })
}
shinyApp(ui, server)

