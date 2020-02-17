### Server Function for the App

if(getRversion() >= '2.5.1') {
  globalVariables(c('demo_info', 'demo_unfiltered', 'demo_filtered', 'demo_meta', 'demo',
                    'ani_id', 'Animal', 'Date', 'site', 'LocationID', 'tags', 'js', 'DateTime',
                    'Elevation', 'TimeDiffMins', 'Rate', 'Longitude', 'Latitude', 'LongBin',
                    'LatBin', 'Duration', 'stopApp', 'Speed', 'Slope', 'Aspect'))
}

#'
#'Defines logic for updating the app based on user interaction in the ui
#'
#'
#'@param input see shiny app architecture
#'@param output see shiny app architecture
#'@param session see shiny app architecture
#'@return server function for use in a shiny app
#'@import shiny
#'@import ggplot2
#'@import dplyr
#'@import leaflet
#'@import leaflet.extras
#'@export
#'
app_server <- function(input, output, session) {
  # initialize list of datasets
  meta <- reactiveVal(demo_meta)
  uploaded <- reactiveVal(FALSE)
  
  raw_dat <- reactive({
    if(is.null(input$zipInput)) {
      return(demo_info)
    }
    dat_info <- store_batch_list(input$zipInput)
    meta(dat_info$meta)
    return(dat_info)
  })
  
  output$numUploaded <- renderText(paste0(ifelse(is.null(input$zipInput), 0, length(raw_dat()$data)), " files uploaded"))
  
  clean_unfiltered <- reactive({
    if(is.null(input$zipInput)) {
      return(demo_unfiltered)
    }
    if(!identical(raw_dat(), demo_info)) {
      return(clean_batch_df(raw_dat(), filters = FALSE, 
                            zoom = input$selected_zoom, get_slope = input$slopeBox, get_aspect = input$aspectBox))
    }
  })
  
  clean_filtered <- reactive({
    if(is.null(input$zipInput)) {
      return(demo_filtered)
    }
    if(!identical(raw_dat(), demo_info)) {
      return(clean_batch_df(raw_dat(), filters = TRUE,
                            zoom = input$selected_zoom, get_slope = input$slopeBox, get_aspect = input$aspectBox))
    }
  })
  
  
  observeEvent(input$processButton, {
    if(!identical(raw_dat(), demo_info)) {
      uploaded(TRUE)
      if(!is.null(input$selected_lat) && !is.null(input$selected_long)) {
        meta(clean_store_batch(raw_dat(), filters = TRUE, zoom = input$selected_zoom,
                               input$slopeBox, input$aspectBox, 
                               input$selected_lat[1], input$selected_lat[2],
                               input$selected_long[1], input$selected_long[2]))
      }
      else {
        meta(clean_store_batch(raw_dat(), input$filterBox, zoom = input$selected_zoom,
                               input$slopeBox, input$aspectBox, 
                               raw_dat()$min_lat, raw_dat()$max_lat,
                               raw_dat()$min_long, raw_dat()$max_long))
      }
    }
  })
  
  ######################################
  ## DYNAMIC DATA
  
  # last data set accessed
  cache <- reactiveVal(list())
  
  
  # main dynamic data set
  dat_main <- reactive({
    req(choose_ani(), choose_dates(), meta, input$selected_min_time, input$selected_max_time)
    
    meta <- meta()
    
    if(any(meta$ani_id  %in% choose_ani()) ){
      meta <- meta %>%
        dplyr::filter(ani_id %in% choose_ani())
    }
    
    ani_names <- paste(choose_ani(), collapse = ", ")
    
    min_datetime <- lubridate::with_tz(lubridate::ymd_hms(paste(choose_dates()[1], input$selected_min_time), tz="UTC", quiet = TRUE), tz="UTC")
    max_datetime <- lubridate::with_tz(lubridate::ymd_hms(paste(choose_dates()[2], input$selected_max_time), tz="UTC", quiet = TRUE), tz="UTC")
    
    cache_name <- paste0(ani_names,", ",min_datetime,"-",max_datetime)
    
    if( (uploaded() || !(cache_name %in% names(cache()))) & (!is.na(min_datetime) & !is.na(max_datetime)) ) {
      # if no user provided data, use demo data
      if(is.null(input$zipInput)) {
        current_df <- demo %>% dplyr::filter(Animal %in% meta$ani_id,
                 DateTime <= max_datetime,
                 DateTime >= min_datetime)
        if(nrow(current_df) == 0) {
          current_df <- demo %>% dplyr::filter(Animal %in% meta$ani_id)
        }
      }
      # if user provided data, get it
      else {
        # temporarily set current_df to cached df to avoid error
        current_df <- cache()[[1]]$df
        if(any(meta$ani_id  %in% choose_ani()) ){
          current_df <- get_data_from_meta(meta, min_datetime, max_datetime)
        }
      }
     
      # add LocationID column to the restricted data set
      current_df <- current_df %>% 
        dplyr::mutate(LocationID = 1:dplyr::n())
              
      # enqueue to cache
      updated_cache <- cache()
      updated_cache[[cache_name]] <- list(df = current_df, ani = choose_ani(), date1 = min_datetime, date2 = max_datetime)
      
      # dequeue if there are more than 5 dfs 
      if(length(updated_cache) > 5) {
        updated_cache <- updated_cache[-1]
      }
      cache(updated_cache)
    }
    if(is.null(choose_recent())) {
      return(cache()[[1]]$df)
    }
    else {
      return(cache()[[choose_recent()]]$df)
    }
  })
  
  output$nrow_recent <- renderText(paste0(nrow(dat_main()), " rows selected"))
  output$head_recent <- renderTable(utils::head(dat_main() %>% dplyr::select(Date, Time, Animal, GPS, Latitude, Longitude, Distance, Rate, Course)))
  
  ######################################
  ## DYNAMIC USER INTERFACE
  
  # select lat/long bounds
  
  output$lat_bounds <- renderUI({
    if(!input$filterBox) {
      return()
    }
    shinyWidgets::numericRangeInput("selected_lat", "Latitude Range:", value = c(raw_dat()$min_lat, raw_dat()$max_lat))
  })
  
  output$long_bounds <- renderUI({
    if(!input$filterBox) {
      return()
    }
    shinyWidgets::numericRangeInput("selected_long", "Longitude Range:", value = c(raw_dat()$min_long, raw_dat()$max_long))
  })
  
  output$zoom <- renderUI({
    req(input$mainmap_zoom)
    numericInput("selected_zoom", "Zoom:", value = input$mainmap_zoom, min = 1, max = 14, step = 1)
  })
  
  # select data sites
  choose_site <- callModule(reactivePicker, "choose_site",
                            type = "site", req_list = list(meta = meta), 
                            text = "Select Site(s)", min_selected = 1, max_selected = 2, 
                            multiple = TRUE, options = list(`actions-box` = TRUE))
  
  
  # select animals
  choose_ani <- callModule(reactivePicker, "choose_ani",
                           type = "ani", req_list = list(meta = meta, selected_site = choose_site),
                           text = "Select Animal(s)", min_selected = 1, max_selected = 4,
                           multiple = TRUE, options = list(`actions-box` = TRUE))
  
  # select dates
  choose_dates <- callModule(datePicker, "choose_dates",
                             req_list = list(meta = meta, selected_ani = choose_ani), text = "Date Range")
  
  # select time range
  output$min_time <- renderUI({
    req(meta, choose_ani())
    
    textInput("selected_min_time", "Min Time", value = strftime(min(meta()$min_date), format="%H:%M:%S", tz="UTC"), placeholder = "HH:MM:SS")
  })
  
  # select time range
  output$max_time <- renderUI({
    req(meta, choose_ani())
    
    textInput("selected_max_time", "Max Time", value = strftime(max(meta()$max_date), format="%H:%M:%S", tz="UTC"), placeholder = "HH:MM:SS")
  })
  
  # select variables to compute statistics
  choose_cols <- callModule(staticPicker, "choose_cols",
                            selected_ani = choose_ani, text = "Choose Variables for Statistics",
                            choices = c("Elevation", "TimeDiffMins", "Course", "CourseDiff", "Distance", "Rate", "Slope", "Aspect"),
                            min_selected = 1, max_selected = 4)
  
  # select summary statistics
  choose_stats <- callModule(staticPicker, "choose_stats",
                             selected_ani = choose_ani, text = "Choose Summary Statistics",
                             choices = c("N", "Mean", "SD", "Variance", "Min", "Max", "Range", "IQR",  "Q1", "Median", "Q3"),
                             min_selected = 1, max_selected = 6)
  
  # select recent data
  choose_recent <- callModule(reactivePicker, "choose_recent",
                              type = "recent", 
                              req_list = list(dat_main = dat_main, selected_ani = choose_ani, dates = reactive({choose_dates()}), 
                                              min_time = reactive({input$selected_min_time}), max_time = reactive({input$selected_max_time}), cache = cache),
                              text = "Select Data", multiple = FALSE)
  
  # spatial points for maps
  points_main <- reactive({
    # If missing input, return to avoid error later in function
    req(dat_main)
    
    sp::SpatialPointsDataFrame(coords = dat_main()[c("Longitude", "Latitude")], 
                           data = dat_main(),
                           proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    
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
        dplyr::filter(LocationID %in% selected_locations())
      )
    }
      
  })
  
  # subsetted spatial points for maps
  points <- reactive({
    # If missing input, return to avoid error later in function
    req(dat )

    sp::SpatialPointsDataFrame(coords = dat()[c("Longitude", "Latitude")], 
                           data = dat(),
                           proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
 
  
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
    fitBounds(stats::median(meta()$min_long), stats::median(meta()$min_lat), stats::median(meta()$max_long), stats::median(meta()$max_lat)) %>%
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
  last_locations <- reactiveVal(NULL)
  
  observe({
    req(points, choose_ani())
    
    pts <- points()
    
    if (is.null(choose_recent())) {
      return(leaflet() %>%  # Add tiles
               addTiles(group = "street map"))
    }
    
    current_anilist <- cache()[[choose_recent()]]
    
    factpal <-
      colorFactor(scales::hue_pal()(length(current_anilist$ani)), current_anilist$ani)
    
    proxy <- leafletProxy("mainmap", session)
    
    if (is.null(last_drawn()) || (!is.null(selected_locations()) & is.null(last_locations())) || (!is.null(selected_locations()) & !identical(last_locations(), selected_locations()) & !identical(last_drawn()$ani, current_anilist))  
         || (!any(current_anilist$ani %in% last_drawn()$ani)) || (identical(last_drawn()$ani, current_anilist$ani) & identical(last_locations(), selected_locations()) & (last_drawn()$date1 != current_anilist$date1 || last_drawn()$date2 != current_anilist$date2))) {
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
              paste("Slope:", pts$Slope),
              paste("Aspect:", pts$Aspect),
              paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                        ", ")),
              paste("LocationID:", pts$LocationID),
              
              sep = "<br/>"
            )
          )
      # is a subset selected?
      if(!is.null(selected_locations())) {
        proxy %>% fitBounds(min(dat()$Longitude), min(dat()$Latitude), max(dat()$Longitude), max(dat()$Latitude))
        shinyjs::js$removePolygon()
      }
    } # if closing bracket
    else if(uploaded() || !identical(last_drawn()$ani, current_anilist$ani)){
      # remove old points
      for(ani in setdiff(last_drawn()$ani, current_anilist$ani)) {
        proxy %>% clearGroup(ani)
      }
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
                paste("Slope:", pts$Slope),
                paste("Aspect:", pts$Aspect),
                paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                          ", ")),
                paste("LocationID:", pts$LocationID),
                
                sep = "<br/>"
              )
            ) 
      } # if new points
      else if(uploaded()) {
        uploaded = FALSE
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
              paste("Slope:", pts$Slope),
              paste("Aspect:", pts$Aspect),
              paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                        ", ")),
              paste("LocationID:", pts$LocationID),
              
              sep = "<br/>"
            )
          ) 
      }
    } # else if closing bracket
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
    last_locations(selected_locations())
    }) # observe

  
  
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
            y = "Elevation (meters)") +
      ylim(1000,2000) + 
      geom_line(na.rm = TRUE) + 
      geom_point(na.rm = TRUE) + 
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
    
    ggplot(dat() %>% dplyr::filter(Rate < 50), aes(x=Animal, y= Rate, fill=Animal))+
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
             dplyr::mutate( LongBin = cut_number(Longitude, 100, 
                                          labels= round( seq(min(Longitude), max(Longitude), length.out = 100 ),3)
             ),
             LatBin = cut_number(Latitude, 100, 
                                 labels=round( seq(min(Latitude), max(Latitude), length.out = 100 ), 3)
             )) %>%
             group_by(LongBin, LatBin, Animal) %>%
             summarize(Duration = sum(TimeDiffMins, na.rm = TRUE)/60), 
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
 
  timediff_title <- callModule(statsLabel, "timediff_title", 
                               choose_cols, choose_stats, 
                               "TimeDiffMins", "Time Difference (minutes) Between GPS Measurements")
  
  
  timediff <- callModule(stats, "timediff", 
                         choose_cols, choose_stats, 
                         "TimeDiffMins", TimeDiffMins, dat)
  
  # Elevation
  
  elevation_title <- callModule(statsLabel, "elevation_title", 
                               choose_cols, choose_stats, 
                               "Elevation", "Elevation")
  
  
  elevation <- callModule(stats, "elevation", 
                         choose_cols, choose_stats, 
                         "Elevation", Elevation, dat)
  
  # Speed
  
  speed_title <- callModule(statsLabel, "speed_title", 
                            choose_cols, choose_stats, 
                            "Speed", "Speed")
  
  
  speed <- callModule(stats, "speed", 
                      choose_cols, choose_stats, 
                      "Speed", Speed, dat)
  
  # Course
  
  course_title <- callModule(statsLabel, "course_title", 
                             choose_cols, choose_stats, 
                             "Course", "Course")
  
  
  course <- callModule(stats, "course", 
                       choose_cols, choose_stats, 
                       "Course", Course, dat)
  
  # Course Difference
  
  coursediff_title <- callModule(statsLabel, "coursediff_title", 
                             choose_cols, choose_stats, 
                             "CourseDiff", "Course Difference Between GPS Measurements")
  
  
  coursediff <- callModule(stats, "coursediff", 
                       choose_cols, choose_stats, 
                       "CourseDiff", CourseDiff, dat)
  
  # Distance
  
  distance_title <- callModule(statsLabel, "distance_title", 
                             choose_cols, choose_stats, 
                             "Distance", "Distance")
  
  
  distance <- callModule(stats, "distance", 
                       choose_cols, choose_stats, 
                       "Distance", Distance, dat)
  
  # Rate
  
  rate_title <- callModule(statsLabel, "rate_title", 
                             choose_cols, choose_stats, 
                             "Rate", "Rate")
  
  
  rate <- callModule(stats, "rate", 
                       choose_cols, choose_stats, 
                       "Rate", Rate, dat)
  
  # Slope
  
  slope_title <- callModule(statsLabel, "slope_title", 
                             choose_cols, choose_stats, 
                             "Slope", "Slope")
  
  
  slope <- callModule(stats, "slope", 
                       choose_cols, choose_stats, 
                       "Slope", Slope, dat)
  
  # Aspect
  
  aspect_title <- callModule(statsLabel, "aspect_title", 
                             choose_cols, choose_stats, 
                             "Aspect", "Aspect")
  
  
  aspect <- callModule(stats, "aspect", 
                       choose_cols, choose_stats, 
                       "Aspect", Aspect, dat)
  
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
      if(input$downloadOptions == "Processed (unfiltered) data") {
        utils::write.csv(clean_unfiltered(), file, row.names = FALSE)
      }
      else if(input$downloadOptions == "Processed (filtered) data") {
        utils::write.csv(clean_filtered(), file, row.names = FALSE)
      }
      else {
        utils::write.csv(dat(), file, row.names = FALSE)
      }
    }
  )
  
  ######################################
  ## END CODE
  session$onSessionEnded(stopApp)
  
}