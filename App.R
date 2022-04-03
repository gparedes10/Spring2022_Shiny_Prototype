
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
            
            
            
            
            #----------------------------------------------
            #Drop-down Menu for Neighborhood options
            #----------------------------------------------
            
            
            
            
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
server <- function(input, output) {

    #----------------------------------------------
    #Reactive Data
    #----------------------------------------------
    
    
    
    
    #----------------------------------------------
    #Drop-down Menus
    #----------------------------------------------
    
    
    
    
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


