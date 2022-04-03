
library(shiny)
library(dplyr) #Enables pipes (%>%)
library(ggplot2)
library(plotly) #Interactive ggplots
library(leaflet) #Maps
library(scales) #Used to get the default ggplot color pallet
library(sf)
library(lubridate) #date formatting

#----------------------------------------------
#Data Call/Clean
#----------------------------------------------
crime_Data <- read.csv("Part1_Crime_data.csv") #Read original dataset
crime_Data <- select(crime_Data, c("CrimeDateTime",
                                   "Description",
                                   "Neighborhood",
                                   "Latitude",
                                   "Longitude"))

crime_Data$CrimeDateTime <- date(crime_Data$CrimeDateTime) #Change Date Format

neighborhoods <- st_read("Neighborhoods.geojson") #Read Neighborhood File

#----------------------------------------------
#Map Labels for Neighborhood Names
#----------------------------------------------



################################################################################
# Define UI for application 
################################################################################
ui <- fluidPage(
    
    #Application Title
    titlePanel("Crime App Prototype"),
    
    
    #Application Side Panel
    sidebarLayout(
        sidebarPanel(
            
            #----------------------------------------------
            #Drop-down Menu for Crime Options
            #----------------------------------------------
            selectInput(inputId = "crime_choice", 
                        label = "Choose a type of Crime",
                        "Names",
                        multiple = TRUE),
            
            
            
            #----------------------------------------------
            #Drop-down Menu for Neighborhood options
            #----------------------------------------------
            selectInput(inputId = "neighborhood_choice",
                        label = "Choose a Neighboor",
                        "Names",
                        multiple = TRUE),
            
            
            
            #----------------------------------------------
            #Date range (years) menu
            #----------------------------------------------
            
            
            
        ), #End of Side Panel
        
        #Application Main Panel/Area
        mainPanel(
            
            #----------------------------------------------
            #Map
            #----------------------------------------------
            
            
            
            
            #----------------------------------------------
            #Total Crimes Per Neighborhood
            #----------------------------------------------
            
            
            
            #----------------------------------------------
            #Crimes Over time
            #----------------------------------------------
            
            
            
            
            #----------------------------------------------
            #Crimes per Neighborhood Table
            #----------------------------------------------
            
            
            
        )#End of Main Panel
    )
)



################################################################################
# Define server logic
################################################################################
server <- function(input, output, session) {

    #----------------------------------------------
    #Reactive Data
    #----------------------------------------------
    data <- reactive({
        req(input$crime_choice, input$neighborhood_choice)
        
        filtered_data <- sr_311Data %>%
            filter(srtype %in% input$crime_choice) %>%
            filter(neighborhood %in% input$neighborhood_choice) #%>%
            #filter(createddate >= input$daterange[1] & createddate <= input$daterange[2])
        
    })
    
    
    
    #----------------------------------------------
    #Drop-down Menus
    #----------------------------------------------
    observe({
        updateSelectInput(session, "crime_choice", choices = sort(unique(crime_Data$Description)))
        updateSelectInput(session, "neighborhood_choice", choices = sort(unique(crime_Data$Neighborhood)))
    
    })
    
    
    
    #----------------------------------------------
    #Leaflet Map
    #----------------------------------------------
    
    
    
    
    #----------------------------------------------
    #Total Crimes Per Neighborhood Graph
    #----------------------------------------------
    
    
    
    
    #----------------------------------------------
    #Crimes over Time Graph
    #----------------------------------------------
    
    
    
    
    #----------------------------------------------
    #Crimes Table
    #----------------------------------------------
    
    
    
    
}


################################################################################
# Run the application 
################################################################################
shinyApp(ui = ui, server = server)


