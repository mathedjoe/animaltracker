#'
#'You can run the cowtrackr Shiny application by calling this function.
#'
#'@param rds_path Path of cow data file to input
#'
run_shiny_animaltracker <- function(rds_path) {
  suppressWarnings(ani <- bind_rows(readRDS(rds_path)))
  
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    h1("Animal Tracker"),
    fluidRow(
      column(4, 
             uiOutput("choose_dates"),
             uiOutput("choose_data")),
      column(8, 
             leafletOutput("mainmap", height = 640),
             plotOutput("plot1"),
             plotOutput("plot2")
      )
    )
    
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    
    # print("got here")
    
    # Drop-down selection box for which data set
    output$choose_data<- renderUI({
      checkboxGroupInput("selected_ani", "ani", choices = as.list(unique(ani$Cow)), selected = unique(ani$Cow)[1])
    })
    
    
    # date range
    output$choose_dates <- renderUI({
      
      # If missing input, return to avoid error later in function
      if(is.null(input$selected_ani))
        return()
      
      # Get the data set with the appropriate name
      
      dates <- ani %>% 
        filter(Cow %in% input$selected_ani) 
      
      dates <- dates$Date
      
      sliderInput("dates", "Date Range", min = min(dates),
                  max = max(dates), value = c(min(dates), max(dates)), step = 1,
                  animate = animationOptions(loop = FALSE, interval = 1000))
    })
    
    dat <- reactive({
      if(is.null(input$selected_ani) || is.null(input$dates))
        return()
      
      ani %>% 
        filter(
          Cow %in% input$selected_ani,
          Date >= input$dates[1], 
          Date <= input$dates[2])
      
    })
    
    points <- reactive({
      # If missing input, return to avoid error later in function
      if(is.null(input$selected_ani) || is.null(input$dates) )
        return()
      
      SpatialPointsDataFrame(coords = dat()[c("Longitude", "Latitude")], data = dat(),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    })
    
    
    output$mainmap <- renderLeaflet({
      
      
      
      if(is.null(input$selected_ani) || is.null(input$dates) )
        return()
      
      factpal <- colorFactor(hue_pal()(length(input$selected_ani)), input$selected_ani)
      
      leaflet() %>%  # Add tiles
        addTiles() %>%
        # addProviderTiles("OpenTopoMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
        # addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
        
        addCircleMarkers(data = points(), radius=6, fillOpacity = .6, stroke=F,
                         color = ~ factpal(Cow), 
                         popup = ~ paste(paste("<h4>",paste("Animal ID:", points()$Cow), "</h4>"),
                                         paste("Date/Time:", points()$DateTime),
                                         paste("Altitude:", points()$Altitude),
                                         paste("Elevation:", points()$Elevation),
                                         paste("Lat/Lon:", paste(points()$Latitude, points()$Longitude, sep=", ")),
                                         sep="<br/>")
        )
      
      # leaflet() %>%
      #   addMarkers(data = points(),popup=as.character(points()$a))
    })
    
    output$plot1 <- renderPlot({
      if(is.null(input$selected_ani) || is.null(input$dates))
        return()
      
      # hist(dat()$TimeDiffMin [dat()$TimeDiffMin < 100], main = "Distribution of Time Between GPS Measurements" )
      ggplot(dat(), aes(x=DateTime, y=Elevation, group=Cow, color=Cow)) + 
        labs( title = "Elevation (meters) during Data Collection")+
        ylim(1000,2000)+geom_line() + 
        geom_point() + 
        theme_minimal()
      
      
    })
    
    output$plot2 <- renderPlot({
      if(is.null(input$selected_ani) || is.null(input$dates))
        return()
      
      ggplot(dat(), aes(x=TimeDiffMins, fill=Cow))+
        geom_histogram(  col="White", breaks = seq(0,40, 2)) +
        facet_wrap(~Cow, ncol=2)+
        labs( title = "Distribution of Time between GPS Readings, by GPS Unit" )+ 
        theme_minimal()
      
      
    })
  }
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}

    
   
