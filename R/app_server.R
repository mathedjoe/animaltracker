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
  
  meta <- reactive({ 
    if(is.null(input$zipInput)) {
      return()
    }
    clean_batch(input$zipInput) 
  }) 
  
  # Drop-down selection box for which sites
  output$choose_site <- renderUI({
    if(is.null(input$zipInput)) {
      meta <- demo_meta
    }
    else {
      meta <- meta()
    }
    pickerInput("selected_site", "Filter by Site",
                choices = as.list(as.character(unique(meta$site))),
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    ) 
  }) 
  
  
  # Drop-down selection box for which animals
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
    
    pickerInput("selected_ani", "Filter by Animal ID",
                choices = as.list(as.character(unique(meta$ani_id))),
                multiple = TRUE, 
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  # Which cols for stats?
  
  output$choose_cols <- renderUI({
    if(is.null(input$selected_ani)) {
      return()
    }
    cols <- c("TimeDiffMins", "Altitude", "Course", "CourseDiff", "Distance", "Rate")
    pickerInput("selected_cols", "Filter Variables for Statistics",
                choices = cols,
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  # Which summary stats?
  
  output$choose_stats <- renderUI({
    if(is.null(input$selected_ani)) {
      return()
    }
    stats <- c("Mean", "SD", "Variance", "Range", "IQR", "Min", "Q1", "Median", "Q3", "Max")
    pickerInput("selected_stats", "Filter Summary Statistics",
                choices = stats,
                selected = c("Mean", "SD", "Min", "Median", "Max"),
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  
  
  # date range
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
  
  output$choose_times <- renderUI({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$selected_ani))
      return()
    
    dat_no_time <- dat_no_time()
    
    min_times <- min(dat_no_time$DateTime)
    max_times <- max(dat_no_time$DateTime)
    
    sliderInput("times", "Time Range", min = min_times,
                max = max_times, value = c(min_times, max_times), step = 1,
                animate = animationOptions(loop = FALSE, interval = 1000))
  })
  
  
  dat_no_time <- reactive({
    if(is.null(input$selected_ani) || is.null(input$dates))
      return()
    
    if(is.null(input$zipInput)) {
      meta <- demo_meta
      meta <- meta %>%
        dplyr::filter(ani_id %in% input$selected_ani)
      current_df <- demo %>%
        dplyr::filter(Animal %in% meta$ani_id,
                      Date <= input$dates[2],
                      Date >= input$dates[1])
    }
    else {
      meta <- meta()
      meta <- meta %>%
        dplyr::filter(ani_id %in% input$selected_ani)
      current_df <- get_data_from_meta(meta, input$dates[1], input$dates[2])
    }
  })
  
  dat <- reactive({
    if(is.null(input$times)) {
      return()
    }
    current_df <- dat_no_time() %>%
      dplyr::filter(DateTime >= input$times[1],
                    DateTime <= input$times[2])
    
  })
  
  points <- reactive({
    # If missing input, return to avoid error later in function
    if(is.null(input$selected_ani) || is.null(input$dates)) 
      return()
    
    SpatialPointsDataFrame(coords = dat()[c("Longitude", "Latitude")], data = dat(),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  })
  
  
  output$mainmap <- renderLeaflet({
    
    
    
    if(is.null(input$selected_ani) || is.null(input$dates) )
      return()
    
    factpal <- colorFactor(scales::hue_pal()(length(input$selected_ani)), input$selected_ani)
    
    leaflet() %>%  # Add tiles
      addTiles() %>%
      # addProviderTiles("OpenTopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      # addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
      # addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
      
      addCircleMarkers(data = points(), radius=6, fillOpacity = .6, stroke=F,
                       color = ~ factpal(Animal), 
                       popup = ~ paste(paste("<h4>",paste("Animal ID:", points()$Animal), "</h4>"),
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
    ggplot(dat(), aes(x=DateTime, y=Elevation, group=Animal, color=Animal)) + 
      labs( title = "Elevation (meters) during Data Collection")+
      ylim(1000,2000)+geom_line() + 
      geom_point() + 
      theme_minimal()
    
    
  })
  
  output$plot2 <- renderPlot({
    if(is.null(input$selected_ani) || is.null(input$dates))
      return()
    
    ggplot(dat(), aes(x=TimeDiffMins, fill=Animal))+
      geom_histogram(  col="White", breaks = seq(0,40, 2)) +
      facet_wrap(~Animal, ncol=2)+
      labs( title = "Distribution of Time between GPS Readings, by GPS Unit" )+ 
      theme_minimal()
    
    
  })
  
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
  
  session$onSessionEnded(stopApp)
  
}