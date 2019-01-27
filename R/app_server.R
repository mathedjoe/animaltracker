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
      return(demo_meta)
    }
      return(clean_batch(input$zipInput))
  })
  
  ######################################
  ## DYNAMIC DATA
  
  # main dynamic data set
  dat_main <- reactive({
    req(input$selected_ani, input$dates, meta)
    
    meta <- meta()
    
    if(any(meta$ani_id  %in% input$selected_ani) ){
      meta <- meta%>%
        filter(ani_id %in% input$selected_ani)
    }
    
    
    # if(is.null(input$selected_ani) | is.null(input$dates) | is.null(meta)  )
    #   return()
    
    # if no user provided data, use demo data
    if(is.null(input$zipInput)) {
      
      current_df <- demo %>%
        filter(Animal %in% meta$ani_id,
               Date <= input$dates[2],
               Date >= input$dates[1])
    }
    
    # if user provided data, get it
    else {
      print(paste("Animals =", input$selected_ani) )
      if(any(meta$ani_id  %in% input$selected_ani) ){
        current_df <- get_data_from_meta(meta, input$dates[1], input$dates[2])
      }
      
      
    }
    
    # add LocationID column to the restricted data set
    current_df <- current_df %>% 
      mutate(LocationID = 1:n())
    
    return(current_df)
    
  })
  
  
  
  ######################################
  ## DYNAMIC USER INTERFACE
  
  # select data sites
  output$choose_site <- renderUI({
    req(meta)
    
    meta <- meta()
    
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
    
    req(meta, input$selected_site)
    
    meta <- meta() %>%
      filter(site %in% input$selected_site) 
    
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
    
    req(meta, input$selected_ani)
    
    # Get the data set with the appropriate name
    
    meta <- meta() %>%
      filter(ani_id %in% input$selected_ani)
    
    max_date <- max( meta$max_date, na.rm=T)
    min_date <- min( meta$min_date, na.rm=T)
    
    sliderInput("dates", "Date Range", min = min_date,
                max = max_date, value = c(min_date, max_date), step = 1,
                animate = animationOptions(loop = FALSE, interval = 1000))
  })
  
  # # select times
  # output$choose_times <- renderUI({
  #   
  #   # If missing input, return to avoid error later in function
  #   if( is.null(dat()) | is.null(input$dates) ) 
  #     return()
  #   min_times <- min(dat()$Time)
  #   max_times <- max(dat()$Time)
  #   sliderInput("times", "Time Range", 
  #               min = min_times, max =max_times, value = c(min_times, max_times), 
  #               step = 1,
  #               animate = animationOptions(loop = FALSE, interval = 1000))
  # })
  
  
  # select variables to compute statistics
  output$choose_cols <- renderUI({
    req(input$selected_ani) 
    
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
    req(input$selected_ani)
    
    stats_choices <- c("N", "Mean", "SD", "Variance", "Min", "Max", "Range", "IQR",  "Q1", "Median", "Q3" )
    pickerInput("selected_stats", "Choose Summary Statistics",
                choices = stats_choices,
                selected = stats_choices[1:6],
                multiple = TRUE,
                inline = FALSE, options = list(`actions-box` = TRUE)
    )
  })
  
  
  # spatial points for maps
  points_main <- reactive({
    # If missing input, return to avoid error later in function
    req(dat_main)
    
    SpatialPointsDataFrame(coords = dat_main()[c("Longitude", "Latitude")], 
                           data = dat_main(),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    
  })
  
  ### Subseted data set
  dat <- reactive({
    req(dat_main)
    # subset data if user has defined selected locations
    if(is.null(selected_locations())){
      return(dat_main())
    }
  
    else{
      return(
        dat_main() %>%
        filter(LocationID %in% selected_locations())
      )
    }
      
  })
  
  # subsetted spatial points for maps
  points <- reactive({
    # If missing input, return to avoid error later in function
    req(dat )

    SpatialPointsDataFrame(coords = dat()[c("Longitude", "Latitude")], 
                           data = dat(),
                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 
  
  })
  
  ######################################
  ## DYNAMIC DISPLAYS
  output$mainmap <- renderLeaflet({
    
    req(points, input$selected_ani ) 
    if(length(input$selected_ani) ==0 ){
      return(  leaflet() %>%  # Add tiles
               addTiles(group="street map"))
    }
    
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
                                       paste("Elevation:", pts$Elevation),
                                       paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep=", ")),
                                       paste("LocationID:", pts$LocationID),
                                  
                                       sep="<br/>")
      ) %>%
      
      addHeatmap(
        data = pts,
        group = "heat map",
        # intensity = pts$Elevation,
        blur = 20, max = 0.05, radius = 15
      ) %>% 
      hideGroup("heat map") %>% # turn off heatmap by default
      
      addDrawToolbar(
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        polygonOptions = drawPolygonOptions(
            shapeOptions=drawShapeOptions(
              fillOpacity = .2
              ,color = 'white'
              , fillColor = "mediumseagreen"
              ,weight = 3)),
        rectangleOptions = drawRectangleOptions(
          shapeOptions=drawShapeOptions(
            fillOpacity = .2
            ,color = 'white'
            , fillColor = "mediumseagreen"
            ,weight = 3)),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
      
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
  
  ##############################################################
  # SUBSET DATA VIA MAP
  selected_locations <- reactive({
    
    if(is.null(input$mainmap_draw_new_feature) | is.null(points_main())){
      return()
    }
    #Only add new layers for bounded locations
    
    # transform into a spatial polygon
    drawn_polygon <- sp::Polygon(
                        do.call(rbind,
                                lapply(input$mainmap_draw_new_feature$geometry$coordinates[[1]],
                                   function(x){
                                     c(x[[1]][1],x[[2]][1])
                                     })
                        )
                      )
    drawn_polys <-  sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon")))
    crs(drawn_polys) <- crs(points_main())
    
    # identify selected locations
    selected_locs <- sp::over(points_main(),drawn_polys)
    
    # get location ids
    as.character( points_main()[["LocationID"]][which(!is.na(selected_locs))] )
    
  })
  
  
  ##############################################################
  # DOWNLOAD DATA
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data_export_", format(Sys.time(), "%Y-%m-%d_%H-%M-%p"), ".csv")
    },
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )
  
  ######################################
  ## END CODE
  session$onSessionEnded(stopApp)
  
}