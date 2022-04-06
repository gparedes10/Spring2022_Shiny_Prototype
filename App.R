
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
labels <- sprintf(
    "<strong>%s</strong>",
    neighborhoods$name
) %>% lapply(htmltools::HTML)

#make color pallet for service requests
#use hue_pal()(10) to add the 10 base colors from ggplot to see the 10 colors use show_col(hue_pal()(10))
#Use colorFactor(rainbow(10)) to test other colors
factpal = colorFactor(hue_pal()(14), crime_Data$Description)


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
            dateRangeInput(inputId = "date_range_selection",
                           label = "Select a Year",
                           start = "1963-01-01",
                           end = "2022-01-01",
                           min = "1963-01-01",
                           max = "2022-04-01")
            
            
        ), #End of Side Panel
        
        #Application Main Panel/Area
        mainPanel(
            
            #----------------------------------------------
            #Map
            #----------------------------------------------
            h1("Crime Map"),
            leafletOutput("crime_map", width = "90%", height = "600px"),
            
            
            
            #----------------------------------------------
            #Total Crimes Per Neighborhood
            #----------------------------------------------
            
            
            
            #----------------------------------------------
            #Crimes Over time
            #----------------------------------------------
            
            

            #----------------------------------------------
            #Crimes per Neighborhood Table
            #----------------------------------------------
            tableOutput("crimeTable")
            
            
        )#End of Main Panel
    ))




################################################################################
# Define server logic
################################################################################
server <- function(input, output, session) {

    #----------------------------------------------
    #Reactive Data
    #----------------------------------------------
    data <- reactive({
        req(input$crime_choice, input$neighborhood_choice)
        
        filtered_data <- crime_Data %>%
            filter(Description %in% input$crime_choice) %>%
            filter(Neighborhood %in% input$neighborhood_choice) %>%
            filter(CrimeDateTime >= input$date_range_selection[1] & CrimeDateTime <= input$date_range_selection[2])
        
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
    output$crime_map<- renderLeaflet({
        leaflet() %>%
            addTiles() %>% #adds the base map
            
            addPolygons( #adds the polygon layer for the neighborhoods
                data = neighborhoods,
                color = "#444444", #polygon color
                weight = 1.5,      #polygons' outline weight
                opacity = 1,       #polygons' outline opacity
                fillOpacity = 0.25, #polygon opacity
                highlightOptions = highlightOptions(
                    color = "white",  #Highlights the neighborhood hovered
                    weight = 3, 
                    bringToFront = FALSE), #moves the neighborhood outline layer up
                label = labels, #adds neighborhood name label when hovered
                labelOptions = labelOptions(
                    textsize = "14px"),
                layerId = ~name, #Used to get the name of the neighborhood when clicking on the map
                group = "neighborhoodShapes") %>%
    
            addCircleMarkers( #adds layer with Service Requests markers
                data = data(),
                color = ~factpal(Description), #change color for each type of request based on color pallet
                stroke = FALSE,           #remove the outer line of the marker
                fillOpacity = 0.75,       
                popup = data()$Description,    #adds popup when marker is clicked
                group = "crimes") %>%  
            
            addLayersControl(
                overlayGroups = c("crimes","neighborhoodShapes"),
                options = layersControlOptions(collapsed = FALSE)) %>%
            
            addLegend(pal = factpal, values = data()$Description, opacity = 1)
    })
    
    #Map zooms into the neighborhood that the user clicks on
    #leafletProxy makes it so that the map doesn't have to be redrawn whenever the user clicks on it.
    observeEvent(input$crime_map_shape_click, {
        click <- input$crime_map_shape_click
        
        leafletProxy("crime_map") %>%
            setView(
                lng = click$lng,
                lat = click$lat,
                zoom = 15)
    })
    
    #----------------------------------------------
    #Total Crimes Per Neighborhood Graph
    #----------------------------------------------
    
    
    
    
    #----------------------------------------------
    #Crimes over Time Graph
    #----------------------------------------------
    
    
    
    
    #----------------------------------------------
    #Crimes Table
    #----------------------------------------------
    output$crimeTable <- renderTable(
        as.data.frame.matrix(table(data()$Description, data()$Neighborhood)), 
        striped=TRUE, 
        bordered = TRUE, 
        rownames = TRUE
    )
    
    
    
}


################################################################################
# Run the application 
################################################################################
shinyApp(ui = ui, server = server)


