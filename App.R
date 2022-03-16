
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly) #Interactive ggplots
library(leaflet) #Maps
library(scales) #Used to get the default ggplot color pallet

#Call Data. Only need to run it once for the app to work, hence it is located up here.
source('App_Data.R')

#map labels for neighborhood names
labels <- sprintf(
  "<strong>%s</strong>",
  neighborhoods$name
) %>% lapply(htmltools::HTML)

#make color pallet for service requests
#use hue_pal()(10) to add the 10 base colors from ggplot to see the 10 colors use show_col(hue_pal()(10))
#Use colorFactor(rainbow(10)) to test other colors
factpal = colorFactor(hue_pal()(10), sr_311Data$srtype)

#------------------------------------------------------------------
# Define UI for application 
#------------------------------------------------------------------
ui <- fluidPage(
  # Application title
  titlePanel("Prototype Application - Reactive plots"),
  
  sidebarLayout(
    
    # Sidebar on the left
    sidebarPanel(
      
      #Add Select Neighborhood Menu
      selectInput(inputId = "neighborhood_choice",
                  label = "Choose a Neighboor",
                  "Names",
                  multiple = TRUE),
      
      #Drop down menu with Service Requests options.
      selectInput(inputId = "service_request_choice", 
                  label = "Choose a Service Request",
                  "Names",
                  multiple = TRUE),
      
      #Date range menu
      dateRangeInput(inputId = "daterange",
                     label = "Select a date range",
                     start = "2020-01-01",
                     end = "2020-12-31",
                     min = "2020-01-01",
                     max = "2020-12-31")
    ),
    
    # Plots based on user selection(s)
    mainPanel(
      
  #--------------------------------------------
  # Status for each Service Request
  #-------------------------------------------
      h1("Service Requests Map"),
      leafletOutput("sr_map", width = "90%", height = "600px"),
      
      h1("Service Requests Status"),
      #Requests per neighborhood
      plotlyOutput("tota_requests"),
      #Total Service Request Status graph
      plotlyOutput("sr_status"),
      
      h1("Service Requests Over Time"),
      plotlyOutput("sr_over_time"),
      
      tags$div(class = "rendoText", list(
        textOutput("selected_Neigh")
        
      )),
  
  #--------------------------------------------
  # Table for Service Requests per Neighborhood
  #-------------------------------------------
      tableOutput("requestTable")
  
  
  #Text for number of requests per neighborhood.
  #uiOutput(outputId = "nums"),
  
  
  #Practice reproducible text
  # textOutput("selected_var")
  
    )
  )
 
)


#------------------------------------------------------------------
# Define server logic
#------------------------------------------------------------------
server <- function(input, output, session) {
    
  ##############################################################################
  # REACTIVE DATA 
  ##############################################################################
  #Filter data based on user selection
  data <- reactive({
    req(input$service_request_choice, input$neighborhood_choice)
    
    filtered_data <- sr_311Data %>%
      filter(srtype %in% input$service_request_choice) %>%
      filter(neighborhood %in% input$neighborhood_choice) %>%
      filter(createddate >= input$daterange[1] & createddate <= input$daterange[2])
    
    
  })
  

  ##############################################################################
  # DROP-DOWN MENUS
  ##############################################################################
  
  #Create drop-down menus automatically
  observe({
    updateSelectInput(session, "service_request_choice", choices = sort(unique(sr_311Data$srtype)))
    
    updateSelectInput(session, "neighborhood_choice", choices = sort(unique(sr_311Data$neighborhood)))
    
    #updateSelectInput(session, "neighborhood_choice", selected = input$sr_map_shape_click)
  })
  
  ##############################################################################
  # LEAFLET MAP
  ##############################################################################
  
  #Leaflet Map 
  output$sr_map <- renderLeaflet({
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
        color = ~factpal(srtype), #change color for each type of request based on color pallet
        stroke = FALSE,           #remove the outer line of the marker
        fillOpacity = 0.75,       
        popup = data()$srtype,    #adds popup when marker is clicked
        group = "ServiceRequests") %>%    
      
      addLayersControl(
        overlayGroups = c("ServiceRequests","neighborhoodShapes"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      addLegend(pal = factpal, values = data()$srtype, opacity = 1)
  })
  
  #Map zooms into the neighborhood that the user clicks on
  #leafletProxy makes it so that the map doesn't have to be redrawn whenever the user clicks on it.
  observeEvent(input$sr_map_shape_click, {
    click <- input$sr_map_shape_click

    leafletProxy("sr_map") %>%
      setView(
        lng = click$lng,
        lat = click$lat,
        zoom = 15)
  })
  
  ##############################################################################
  # GRAPHS
  ##############################################################################
  
  #Service Request status based on user selection
  output$sr_status <- renderPlotly({
    g <- ggplot(data(), aes(x = srstatus, fill = srtype))
    g + geom_bar(position="dodge") + #Makes bars appear next to each other
      xlab("Service Request Type") +
      ylab("Total Requests") +
      labs(
        title = "Total Service Request Status"
      )
  })
  
  #Bar graph of total requests per neighborhood
  output$tota_requests <- renderPlotly({
    totalSR <- ggplot(data(), aes(x = srtype, fill = neighborhood))
    totalSR + geom_bar(position = "dodge") +
    xlab("Service Request Type") +
      ylab("Requests per Neighborhood") +
      labs(
        title = "Service Requests per Neighborhood"
      )
  })
  
  
  #Time graph for each Service Requests
  output$sr_over_time <- renderPlotly({
    graph <- ggplot(data(), aes(createddate, color = srtype))
    graph + geom_freqpoly() +
      xlab("Date") +
      ylab("Service Request") +
      labs(
        title = " Total Service Requests Over Time in 2020"
      )
  })
  
  ##############################################################################
  # TABLE
  ##############################################################################
  
  output$requestTable <- renderTable(
    as.data.frame.matrix(table(data()$srtype, data()$neighborhood)), 
    striped=TRUE, 
    bordered = TRUE, 
    rownames = TRUE
  )
  
  
  
  # output$selected_var <- renderText({ 
  #   paste("You have selected", input$var)
  # })
  
  ####Text for number of requests###
  
  # output$nums <- renderUI({
  #   requests <- data()$srtype %>%
  #     factor(levels = input$service_request_choice)
  #   counts <- table(requests)
  #   
  #   HTML(paste("There are", counts, input$service_request_choice, "Service Requests in ---<br>"))
  # })
  

  
  
}


#------------------------------------------------------------------
# Run the application 
#------------------------------------------------------------------
shinyApp(ui = ui, server = server)


