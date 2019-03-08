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
  
  # last data set accessed
  cache <- reactiveVal(list())
  
  # main dynamic data set
  dat_main <- reactive({
    req(input$selected_ani, input$dates, meta)
    
    meta <- meta()
    
    if(any(meta$ani_id  %in% input$selected_ani) ){
      meta <- meta%>%
        filter(ani_id %in% input$selected_ani)
    }
    
    ani_names <- paste(input$selected_ani, collapse = ", ")
    cache_name <- paste0(ani_names,", ",input$dates[1],"-",input$dates[2])
    
    # if(is.null(input$selected_ani) | is.null(input$dates) | is.null(meta)  )
    #   return()
    
    
    if(!(cache_name %in% names(cache()))) {
      # if no user provided data, use demo data
      if(is.null(input$zipInput)) {
        current_df <- demo %>% filter(Animal %in% meta$ani_id,
                 Date <= input$dates[2],
                 Date >= input$dates[1])
      }
      # if user provided data, get it
      else {
        # print(paste("Animals =", input$selected_ani) )
        # temporarily set current_df to cached df to avoid error
        current_df <- cache()[[1]]
        if(any(meta$ani_id  %in% input$selected_ani) ){
          current_df <- get_data_from_meta(meta, input$dates[1], input$dates[2])
        }
      }
      
      # add LocationID column to the restricted data set
      current_df <- current_df %>% 
        mutate(LocationID = 1:n())
              
      # enqueue to cache
      updated_cache <- cache()
      updated_cache[[cache_name]] <- list(df = current_df, ani = input$selected_ani, date1 = input$dates[1], date2 = input$dates[2])
      
      # dequeue if there are more than 5 dfs 
      if(length(updated_cache) > 5) {
        updated_cache <- updated_cache[-1]
      }
      cache(updated_cache)
    }
    if(is.null(input$selected_recent)) {
      return(cache()[[1]]$df)
    }
    else {
      return(cache()[[input$selected_recent]]$df)
    }
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
                selected = ani_choices[1:4],
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
    
    var_choices <- c( "Elevation", "TimeDiffMins", "Course", "CourseDiff", "Distance", "Rate")
    pickerInput("selected_cols", "Choose Variables for Statistics",
                choices = var_choices,
                selected = var_choices[c(1,2,3,4)],
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
  
  # select recent data
  
  output$choose_recent <- renderUI({
    req(dat_main)
    ani_names <- paste(input$selected_ani, collapse = ", ")
    cache_name <- paste0(ani_names,", ",input$dates[1],"-",input$dates[2])
    recent_choices <- names(cache())
    pickerInput("selected_recent", "Select Data",
                choices = recent_choices,
                selected = cache_name,
                multiple = FALSE,
                inline = FALSE
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

  # Show real-time Information about the Mapped Data
  output$mapinfo <- renderUI({
    req(input$mainmap_zoom)
    tags$div(class="row well", 
                list(tags$h4("Map Info"),
                tags$p( paste("Current zoom level =", as.character(input$mainmap_zoom) ) )
                  )
    )
      
    
  })
 
  ######################################
  ## DYNAMIC DISPLAYS
  
  base_map <- reactive({
    req(meta)
    leaflet() %>%  # Add tiles
    addTiles(group="street map") %>%
    fitBounds(median(meta()$min_long), median(meta()$min_lat), median(meta()$max_long), median(meta()$max_lat)) %>%
    #setView(mean(min(meta()$min_long), max(meta()$max_long)), mean(min(meta()$min_lat), max(meta()$max_lat)), zoom = 12) %>%
    # addProviderTiles("OpenTopoMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "satellite") %>%
    addDrawToolbar(
      polylineOptions=FALSE,
      markerOptions = FALSE,
      circleOptions = FALSE,
      circleMarkerOptions = FALSE,
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
      editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) 
  })

  output$mainmap <- renderLeaflet(base_map())
  last_drawn <- reactiveVal(NULL)
  
  observe({
    req(points, input$selected_ani)
    
    pts <- points()
    
    if (is.null(input$selected_recent)) {
      return(leaflet() %>%  # Add tiles
               addTiles(group = "street map"))
    }
    
    current_anilist <- cache()[[input$selected_recent]]
    
    factpal <-
      colorFactor(scales::hue_pal()(length(current_anilist$ani)), current_anilist$ani)
    
    proxy <- leafletProxy("mainmap", session)
    
    if (is.null(last_drawn()) || (!any(current_anilist$ani %in% last_drawn()$ani)) 
        || (identical(last_drawn()$ani, current_anilist$ani) & (last_drawn()$date1 != current_anilist$date1 || last_drawn()$date2 != current_anilist$date2))) {
      for(ani in last_drawn()$ani) {
        proxy %>% clearGroup(ani)
      }
      proxy %>%
        addCircleMarkers(
          data = pts,
          radius = 4,
          group = pts$Animal,
          stroke = FALSE,
          color = ~ factpal(Animal),
          weight = 3,
          opacity = .8,
          fillOpacity = 1,
          fillColor = ~ factpal(Animal),
          popup = ~ paste(
            paste("<h4>", paste("Animal ID:", pts$Animal), "</h4>"),
            paste("Date/Time:", pts$DateTime),
            paste("Elevation:", pts$Elevation),
            paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                      ", ")),
            paste("LocationID:", pts$LocationID),
            
            sep = "<br/>"
          )
        ) 
    }
    else if(!identical(last_drawn()$ani, current_anilist$ani)){
      # remove old points
      for(ani in setdiff(last_drawn()$ani, current_anilist$ani)) {
        proxy %>% clearGroup(ani)
      }
      # add new points
      if(length(setdiff(current_anilist$ani, last_drawn()$ani)) != 0) {
          pts <- subset(pts, Animal %in% setdiff(current_anilist$ani, last_drawn()$ani))
          proxy %>%
            addCircleMarkers(
              data = pts,
              radius = 4,
              group = pts$Animal,
              stroke = FALSE,
              color = ~ factpal(Animal),
              weight = 3,
              opacity = .8,
              fillOpacity = 1,
              fillColor = ~ factpal(Animal),
              popup = ~ paste(
                paste("<h4>", paste("Animal ID:", pts$Animal), "</h4>"),
                paste("Date/Time:", pts$DateTime),
                paste("Elevation:", pts$Elevation),
                paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                          ", ")),
                paste("LocationID:", pts$LocationID),
                
                sep = "<br/>"
              )
            ) 
      }
    }
    # add heatmap and layer control 
    proxy %>% 
      addHeatmap(
        data = pts,
        group = "heat map",
        # intensity = pts$Elevation,
        blur = 20,
        max = 0.05,
        radius = 15
      ) %>%
      hideGroup("heat map") %>% # turn off heatmap by default
      addLayersControl(
        baseGroups = c("satellite", "street map"),
        overlayGroups = c("data points", "heat map"),
        options = layersControlOptions(collapsed = FALSE)
      )
    last_drawn(current_anilist)
  })
  
  
  ######################################
  # DYNAMIC PLOTS PANEL
  ######################################
  # Elevation Line Plot
  output$plot_elevation_line <- renderPlot({
   req(dat)
    
    # hist(dat()$TimeDiffMin [dat()$TimeDiffMin < 100], main = "Distribution of Time Between GPS Measurements" )
    ggplot(dat(), aes(x=DateTime, y=Elevation, group=Animal, color=Animal)) + 
      labs( title = "Elevation Time Series, by Animal",
            x = "Date",
            y = "Elevation (meters)")+
      ylim(1000,2000)+geom_line() + 
      geom_point() + 
      theme_minimal()
    
    
  })
  
  # Sample Rate Histograms
  output$plot_samplerate_hist <- renderPlot({
    req(dat)
    
    ggplot(dat(), aes(x=TimeDiffMins, fill=Animal))+
      geom_histogram(  col="White", breaks = seq(0,40, 2)) +
      facet_wrap(~Animal, ncol=2)+
      labs( title = "Sample Rate, by GPS Unit" ,
            x = "Time between GPS Readings (minutes)", 
            y = "Frequency") + 
      theme_minimal()
    
    
  })
  
  # Rate by Animal

  output$plot_rate_violin <- renderPlot({
    req(dat)
    
    ggplot(dat() %>% filter(Rate < 50), aes(x=Animal, y= Rate, fill=Animal))+
      geom_violin() + 
      geom_boxplot(width=.2, outlier.color = NA) +
      theme_minimal()+
      labs( title = "Rate of Travel, by GPS Unit" ,
            x = "Animal", 
            y = "Rate of Travel (meters/minute)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    
  })
  
  # time spent by lat/long
  output$plot_time_heatmap <- renderPlot({
    req(dat)
    
    dat<- dat()
    # Heatmap of Time Spent
    mybreaks <- list(x = round( seq(min(dat$Longitude), max(dat$Longitude), length.out = 10 ),3),
                     y = round( seq(min(dat$Latitude), max(dat$Latitude), length.out = 10 ),3))
    ggplot(dat %>% 
             mutate( LongBin = cut_number(Longitude, 100, 
                                          labels= round( seq(min(Longitude), max(Longitude), length.out = 100 ),3)
             ),
             LatBin = cut_number(Latitude, 100, 
                                 labels=round( seq(min(Latitude), max(Latitude), length.out = 100 ), 3)
             )) %>%
             group_by(LongBin, LatBin, Animal) %>%
             summarize(Duration = sum(TimeDiffMins, na.rm=T)/60), 
           aes (x = LongBin,  y = LatBin, fill = Duration))+
      geom_tile()+
      facet_wrap(~Animal, ncol=2)+
      labs( title = "Total Time Spent per Location (hours)" ,
            x = "Longitude", 
            y = "Latitude")+
      scale_fill_gradientn(colors = c("white", "green", "red"))+
      scale_x_discrete( breaks = mybreaks$x) +
      scale_y_discrete( breaks = mybreaks$y) +
      coord_equal()+
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
  
  # Elevation
  output$elevation_title <- renderUI({
    if(is.null(input$selected_stats) | is.null(input$selected_cols) | !("Elevation" %in% input$selected_cols)) 
      return()
    h4("Elevation")
  })
  
  elevation_stats <- reactive({
    if(!("Elevation" %in% input$selected_cols) | is.null(input$selected_stats)) 
      return()
    
    summary <- summarize_col(dat(), "Elevation") 
    subset(summary, select=c("Animal", input$selected_stats))
    
  })
  
  output$elevation <- renderTable(elevation_stats())
  
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
    raster::crs(drawn_polys) <- raster::crs(points_main())
    
    # identify selected locations
    selected_locs <- sp::over(points_main(),drawn_polys)
    
    # get location ids
    locs_out <- as.character( points_main()[["LocationID"]] )
    
    # if any non-na selected locations, subset the selected locations
    if( any(!is.na(selected_locs)) ){
      
      locs_out <-locs_out[ which(!is.na(selected_locs)) ] 
      
    }

    locs_out
    
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