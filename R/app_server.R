### Server Function for the App

#'
#'Defines logic for updating the app based on user interaction in the ui
#'
#'
#'@param input see shiny app architecture
#'@param output see shiny app architecture
#'@param session see shiny app architecture
#'@return server function for use in a shiny app
#'@export
#'
# Define server logic for the shiny app
app_server <- function(input, output, session) {
  
  # initialize list of datasets
  meta <- reactive({ 
    if(is.null(input$zipInput)) {
      return()
    }
    clean_batch(input$zipInput) 
  }) 
  
  ######################################
  ## DYNAMIC USER INTERFACE
  
  # select data sites
  output$choose_site <- renderUI({
    if(is.null(input$zipInput)) {
      meta <- demo_meta
    }
    else {
      meta <- meta()
    }
    
    site_choices <- as.list(as.character(unique(meta$site)))
    
    pickerInput("selected_site", "Select Site(s)",
                choices = site_choices,
                selected = site_choices[c(1,2)],
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    ) 
  }) 
  
  
  # select animals
  output$choose_ani <- renderUI({
    if(is.null(input$selected_site)) {
      return()
    }
    if(is.null(input$zipInput)) {
      meta <- demo_meta
    }
    else {
      meta <- meta()
    }
    meta <- meta %>%
      dplyr::filter(site %in% input$selected_site) 
    
    ani_choices <- as.list(as.character(unique(meta$ani_id)))
    
    pickerInput("selected_ani", "Select Animal(s)",
                choices = ani_choices,
                selected = ani_choices[c(1,2)],
                multiple = TRUE, 
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  # select dates
  output$choose_dates <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$selected_ani))
      return()
    
    # Get the data set with the appropriate name
    
    if(is.null(input$zipInput)) {
      meta <- demo_meta
    }
    else {
      meta <- meta()
    }
    
    meta <- meta %>%
      dplyr::filter(ani_id %in% input$selected_ani)
    
    max_dates <- meta$max_date
    min_dates <- meta$min_date
    
    sliderInput("dates", "Date Range", min = min(min_dates),
                max = max(max_dates), value = c(min(min_dates), max(max_dates)), step = 1,
                animate = animationOptions(loop = FALSE, interval = 1000))
  })
  
  # select times
  output$choose_times <- renderUI({
    
    # If missing input, return to avoid error later in function
    if( is.null(dat()) | is.null(input$times) ) 
      return()
    
    sliderInput("times", "Time Range", min = min(dat()$DateTime),
                max = max(dat()$DateTime), value = c(min_times, max_times), step = 1,
                animate = animationOptions(loop = FALSE, interval = 1000))
  })
  
  
  # select variables to compute statistics
  output$choose_cols <- renderUI({
    if(is.null(input$selected_ani)) {
      return()
    }
    var_choices <- c("TimeDiffMins", "Altitude", "Course", "CourseDiff", "Distance", "Rate")
    pickerInput("selected_cols", "Choose Variables for Statistics",
                choices = var_choices,
                selected = var_choices[c(1,2,3)],
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  # select summary statistics
  output$choose_stats <- renderUI({
    if(is.null(input$selected_ani)) {
      return()
    }
    stats_choices <- c("N", "Mean", "SD", "Variance", "Min", "Max", "Range", "IQR",  "Q1", "Median", "Q3" )
    pickerInput("selected_stats", "Choose Summary Statistics",
                choices = stats_choices,
                selected = stats_choices[1:6],
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  ######################################
  ## DYNAMIC DATA
  
  # main dynamic data set
  dat <- reactive({
    if(is.null(input$selected_ani) || is.null(input$dates))
      return()
    
    # if no user provided data, use demo data
    if(is.null(input$zipInput)) {
      meta <- demo_meta %>%
        dplyr::filter(ani_id %in% input$selected_ani)
      
      current_df <- demo %>%
        dplyr::filter(Animal %in% meta$ani_id,
                      Date <= input$dates[2],
                      Date >= input$dates[1])
    }
    
    # if user provided data, get it
    else {
      meta <- meta() %>%
        dplyr::filter(ani_id %in% input$selected_ani)
      current_df <- get_data_from_meta(meta, input$dates[1], input$dates[2])
    }
    
    if( !is.null(input$times) ) {
      
      current_df <- current_df %>%
        dplyr::filter(DateTime >= input$times[1],
                      DateTime <= input$times[2])
    }
    
    return(current_df)
    
  })
  
  
  # spatial points for maps
  points <- reactive({
    # If missing input, return to avoid error later in function
    if( is.null(dat()) )
      return()

    SpatialPointsDataFrame(coords = dat()[c("Longitude", "Latitude")], data = dat(),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  })
  
  ######################################
  ## DYNAMIC DISPLAYS
  output$mainmap <- renderLeaflet({
    
    if( is.null(points()) )
      return()
    
    factpal <- colorFactor(scales::hue_pal()(length(input$selected_ani)), input$selected_ani)
    
    pts <- points()
    
    leaflet() %>%  # Add tiles
      addTiles(group="street map") %>%
      # addProviderTiles("OpenTopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
      # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
      # addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    
      
      addCircleMarkers(data = pts, group = "data points",
                       radius=6,  
                       stroke=TRUE, color = ~ factpal(Animal), weight = 3, opacity = .8,
                       fillOpacity = .3, fillColor = ~ factpal(Animal),
                       popup = ~ paste(paste("<h4>",paste("Animal ID:", pts$Animal), "</h4>"),
                                       paste("Date/Time:", pts$DateTime),
                                       paste("Altitude:", pts$Altitude),
                                       paste("Elevation:", pts$Elevation),
                                       paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep=", ")),
                                       sep="<br/>")
      ) %>%
      
      addHeatmap(
        data = pts,
        group = "heat map",
        # intensity = pts$Elevation,
        blur = 20, max = 0.05, radius = 15
      ) %>% 
      hideGroup("heatmap") %>% # turn off heatmap by default
      
      addLayersControl(
        baseGroups = c("satellite", "street map"),
        overlayGroups = c("data points", "heat map"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # leaflet() %>%
    #   addMarkers(data = points(),popup=as.character(points()$a))
  })
  
  output$plot1 <- renderPlot({
    if(is.null(dat()))
      return()
    
    # hist(dat()$TimeDiffMin [dat()$TimeDiffMin < 100], main = "Distribution of Time Between GPS Measurements" )
    ggplot(dat(), aes(x=DateTime, y=Elevation, group=Animal, color=Animal)) + 
      labs( title = "Elevation (meters) during Data Collection")+
      ylim(1000,2000)+geom_line() + 
      geom_point() + 
      theme_minimal()
    
    
  })
  
  output$plot2 <- renderPlot({
    if(is.null(dat()))
      return()
    
    ggplot(dat(), aes(x=TimeDiffMins, fill=Animal))+
      geom_histogram(  col="White", breaks = seq(0,40, 2)) +
      facet_wrap(~Animal, ncol=2)+
      labs( title = "Distribution of Time between GPS Readings, by GPS Unit" )+ 
      theme_minimal()
    
    
  })
  
  ######################################
  ## DYNAMIC STATISTICS
  # Summary Statistics
  
  # Time Difference
  output$timediff_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("TimeDiffMins" %in% input$selected_cols)) 
      return()
    h4("Time Difference (minutes) Between GPS Measurements")
  })
  
  timediff_stats <- reactive({
    if(!("TimeDiffMins" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "TimeDiffMins") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$timediff <- renderTable(timediff_stats())
  
  # Altitude
  output$altitude_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("Altitude" %in% input$selected_cols)) 
      return()
    h4("Altitude")
  })
  
  altitude_stats <- reactive({
    if(!("Altitude" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "Altitude") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$altitude <- renderTable(altitude_stats())
  
  # Speed
  output$speed_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("Speed" %in% input$selected_cols)) 
      return()
    h4("Speed")
  })
  
  speed_stats <- reactive({
    if(!("Speed" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "Speed") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$speed <- renderTable(speed_stats())
  
  # Course
  output$course_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("Course" %in% input$selected_cols)) 
      return()
    h4("Course")
  })
  
  course_stats <- reactive({
    if(!("Course" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "Course") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$course <- renderTable(course_stats())
  
  # Course Difference
  output$coursediff_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("CourseDiff" %in% input$selected_cols)) 
      return()
    h4("Course Difference Between GPS Measurements")
  })
  
  coursediff_stats <- reactive({
    if(!("CourseDiff" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "CourseDiff") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$coursediff <- renderTable(coursediff_stats())
  
  # Distance
  output$distance_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("Distance" %in% input$selected_cols)) 
      return()
    h4("Distance")
  })
  
  distance_stats <- reactive({
    if(!("Distance" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "Distance") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$distance <- renderTable(distance_stats())
  
  # Rate
  output$rate_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("Rate" %in% input$selected_cols)) 
      return()
    h4("Rate")
  })
  
  rate_stats <- reactive({
    if(!("Rate" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "Rate") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  output$rate <- renderTable(rate_stats())
  
  
  ######################################
  ## END CODE
  session$onSessionEnded(stopApp)
  
}