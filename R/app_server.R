### Server Function for the App

if(getRversion() >= '2.5.1') {
  globalVariables(c('demo_info', 'demo_unfiltered', 'demo_filtered', 'demo_meta', 'demo',
                    'ani_id', 'Animal', 'Date', 'site', 'LocationID', 'tags', 'DateTime',
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
  water_uploaded <- reactiveVal(FALSE)
  fence_uploaded <- reactiveVal(FALSE)
  processingInitiated <- reactiveVal(FALSE)
  processingInitiatedAll <- reactiveVal(FALSE)
  
  raw_dat <- reactive({
    if(is.null(input$zipInput)) {
      return(demo_info)
    }
    dat_info <- store_batch_list(input$zipInput)
    meta(dat_info$meta)
    uploaded(TRUE)
    return(dat_info)
  })
  
  water_geoms <- reactive({
    if(!is.null(input$waterInput)) {
      unlink(file.path("temp_water"), recursive=TRUE)
      
      water_coords <- maptools::getKMLcoordinates(kmlfile = utils::unzip(zipfile = input$waterInput$datapath, 
                                                                         exdir = "temp_water"),
                                                  ignoreAltitude = TRUE)
      water_geoms <- sf::st_sfc(lapply(water_coords, kmz_to_sf))
      names(water_geoms) <- paste0("V",1:length(water_geoms))
      water_uploaded(TRUE)
      unlink(file.path("temp_water"), recursive=TRUE)
      return(water_geoms)
    }
    return(list())
  })
  
  kmz_coords <- reactive({
    if(!is.null(input$kmzInput)) {
      unlink(file.path("temp_fence"), recursive=TRUE)
      coords <- maptools::getKMLcoordinates(kmlfile = utils::unzip(zipfile = input$kmzInput$datapath, 
                                                                   exdir = "temp_fence"),
                                            ignoreAltitude = TRUE)
      fence_uploaded(TRUE)
      unlink(file.path("temp_fence"), recursive=TRUE)
      return(coords)
    }
    return(list())
  })
  
  output$numUploaded <- renderText(paste0(ifelse(is.null(input$zipInput), 0, length(raw_dat()$data)), " files uploaded"))
  
  clean_unfiltered <- reactive({
    if(is.null(input$zipInput)) {
      return(demo_unfiltered)
    }
    if(!identical(raw_dat(), demo_info)) {
      return(clean_batch_df(raw_dat(), filters = FALSE))
    }
  })
  
  clean_filtered <- reactive({
    if(is.null(input$zipInput)) {
      return(demo_filtered)
    }
    if(!identical(raw_dat(), demo_info)) {
      return(clean_batch_df(raw_dat(), filters = TRUE))
    }
  })
  
  
  observeEvent(input$processButton, {
      if(!is.null(lat_bounds()) && !is.null(long_bounds())) {
        processingInitiatedAll(TRUE)
        meta(clean_store_batch(raw_dat(), filters = input$filterBox, zoom = input$selected_zoom,
                               input$slopeBox, input$aspectBox, 
                               lat_bounds()[1], lat_bounds()[2],
                               long_bounds()[1], long_bounds()[2]))
      }
      else {
        processingInitiatedAll(TRUE)
        meta(clean_store_batch(raw_dat(), input$filterBox, zoom = input$selected_zoom,
                               input$slopeBox, input$aspectBox, 
                               raw_dat()$min_lat, raw_dat()$max_lat,
                               raw_dat()$min_long, raw_dat()$max_long))
      }
  })
  
  observeEvent(input$processSelectedButton, {
    processingInitiated(TRUE)
  })
  
  ######################################
  ## DYNAMIC DATA
  
  # last data set accessed
  cache <- reactiveVal(list())
  
  # main dynamic data set
  dat_main <- reactive({
    if(is.null(choose_ani()) || is.null(choose_dates()) || is.null(min_time()) || is.null(max_time())) {
      if(is.null(choose_recent())) {
        return(demo)
      }
      return(cache()[[1]]$df)
    }
    else {
    
      req(meta)
      
      meta <- meta()
      
      if(any(meta$ani_id  %in% choose_ani()) ){
        meta <- meta %>%
          dplyr::filter(ani_id %in% choose_ani())
      }
      
      ani_names <- paste(choose_ani(), collapse = ", ")
      
      
      min_datetime <- lubridate::with_tz(lubridate::ymd_hms(paste(choose_dates()[1], min_time()), tz="UTC", quiet = TRUE), tz="UTC")
      max_datetime <- lubridate::with_tz(lubridate::ymd_hms(paste(choose_dates()[2], max_time()), tz="UTC", quiet = TRUE), tz="UTC")
      
      cache_name <- paste0(ani_names,", ",min_datetime,"-",max_datetime)
  
      if( processingInitiatedAll() || processingInitiated() || (uploaded() || !(cache_name %in% names(cache())))) {
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
          if(processingInitiated()) {
            processingInitiated(FALSE)
            
            cache_name <- paste(cache_name, "(processed)")
            
            current_df <- cache()[[choose_recent()]]$df %>%
              dplyr::filter(Latitude >= lat_bounds()[1], Latitude <= lat_bounds()[2], 
                            Longitude >= long_bounds()[1], Longitude <= long_bounds()[2],
                            DateTime >= min_datetime, DateTime <= max_datetime) 
            
            if(nrow(current_df) == 0) {
              return(cache()[[choose_recent()]]$df)
            }
            current_df <- clean_location_data(current_df, dtype = "", prep = FALSE, filters = input$filterBox) 
          
            
            status_message <- modalDialog(
              pre(id = "console"),
              title = "Please Wait...",
              easyClose = TRUE,
              footer = NULL
            )
            
            showModal(status_message)
            
            withCallingHandlers({
              shinyjs::html("console", "")
              current_df <- lookup_elevation_aws(current_df, zoom = input$selected_zoom, get_slope = input$slopeBox, get_aspect = input$aspectBox)
            },
            message = function(m) {
              shinyjs::html(id = "console", html = m$message)
            })
           
            removeModal()  
          }
          else if(processingInitiatedAll()) {
            processingInitiatedAll(FALSE)
            meta <- meta()
            ani_names <- paste(meta$ani_id, collapse = ", ")
            
            min_datetime <- min(meta$min_date)
            max_datetime <- max(meta$max_date)
            
            cache_name <- paste(paste0(ani_names,", ",min_datetime,"-",max_datetime), "(processed)")
            current_df <- get_data_from_meta(meta, min_datetime, max_datetime)
          }
          else {
            if(any(meta$ani_id  %in% choose_ani()) ){
              current_df <- get_data_from_meta(meta, min_datetime, max_datetime)
            }
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
      return(cache()[[choose_recent()]]$df)
    }
  })
  
  output$nrow_recent <- renderText(paste0(nrow(dat_main()), " rows selected"))
  output$head_recent <- renderTable(utils::head(dat_main() %>% dplyr::select(Date, Time, Animal, GPS, Latitude, Longitude, Distance, Rate, Course)))
  
  ######################################
  ## DYNAMIC USER INTERFACE
  
  # select lat/long bounds
  
  lat_bounds <- callModule(reactiveRange, 
                           id = "lat_bounds", type = "latitude", dat = raw_dat)
  
  long_bounds <- callModule(reactiveRange, 
                           id = "long_bounds", type = "longitude", dat = raw_dat)
  
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
  min_time <- callModule(time, id = "min_time",
                         type = "min", meta = meta, selected_ani = choose_ani)
  
  max_time <- callModule(time, id = "max_time",
                         type = "max", meta = meta, selected_ani = choose_ani)
  
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
                              req_list = list(dat_main = dat_main, selected_ani = choose_ani, dates = choose_dates, 
                                              min_time = min_time, max_time = max_time, cache = cache),
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
      return(dat_main() %>% dplyr::filter(Latitude != 0 | Longitude != 0))
    }
  
    else{
      return(
        dat_main() %>%
        dplyr::filter(Latitude != 0 | Longitude != 0) %>% 
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
    
    pts$Animal <- as.character(pts$Animal)
    
    if (is.null(choose_recent())) {
      return(leaflet() %>%  # Add tiles
               addTiles(group = "street map"))
    }
    
    current_anilist <- cache()[[choose_recent()]]
  
    proxy <- leafletProxy("mainmap", session)
    
    custom_icon_list <- list("asterisk", "plus", "star", "heart", "ok",
                             "stop", "remove-circle", "minus-sign", "eye-open", "bell")
    
    color_list <- as.list("green", "pink", "darkpurple", "cadetblue", "lightblue", "darkred",
                          "lightred", "darkgreen", "red", "darkblue", "lightgreen",
                          "blue", "gray", "black", "beige", "purple", "orange")
    
    my_colors <- color_list[1:length(levels(factor(pts$Animal)))]
    names(my_colors) <- levels(factor(pts$Animal))
    pts$color_label <- as.character(factor(pts$Animal, labels = my_colors[levels(factor(pts$Animal))]))
    water_geoms <- water_geoms()
   
    if (uploaded() || water_uploaded() || grepl("(processed)", choose_recent()) || is.null(last_drawn()) || (!is.null(selected_locations()) & is.null(last_locations())) || (!is.null(selected_locations()) & !identical(last_locations(), selected_locations()) & !identical(last_drawn()$ani, current_anilist))  
         || (!any(current_anilist$ani %in% last_drawn()$ani)) || (identical(last_drawn()$ani, current_anilist$ani) & identical(last_locations(), selected_locations()) & (last_drawn()$date1 != current_anilist$date1 || last_drawn()$date2 != current_anilist$date2))) {
      
      # TODO: Fix bug where demo data remains if filtered before uploading 
      if(!is.null(last_drawn())) {
        for(ani in last_drawn()$ani) {
          proxy %>% clearGroup(ani)
        }
      }
      
      if(length(kmz_coords()) > 0) {
        for(kmz_element in kmz_coords()) {
          if(!is.matrix(kmz_element)) {
            df_point <- data.frame(lng = kmz_element[1], lat = kmz_element[2])
            proxy %>% 
              addCircleMarkers(
                data = df_point,
                group = "fencing",
                radius = 4,
                stroke = FALSE,
                weight = 3,
                opacity = .8,
                fillOpacity = 1,
                color = "black",
                fillColor = "black",
                popup = ~ paste(
                  paste("Lat/Lon:", paste(kmz_element[2], kmz_element[1], sep =
                                            ", "))
                )
              ) 
          }
          else if(kmz_element[1, 1] == kmz_element[nrow(kmz_element), 1] &
                  kmz_element[1, 2] == kmz_element[nrow(kmz_element), 2]) {
            proxy %>% 
              addPolygons(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "fencing")
          }
          else {
            proxy %>% 
              addPolylines(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "fencing")
          }
        }
      }
      
      if(length(water_geoms) > 0) {
        my_icons <- custom_icon_list[1:length(names(water_geoms))]
        names(my_icons) <- names(water_geoms)
        
        pt_indices <- c()
        line_indices <- c()
        polygon_indices <- c()
        
        for(i in 1:length(water_geoms)) {
          if(typeof(water_geoms[[i]]) == "double") {
            if(length(dim(water_geoms[[i]]) > 1)) {
              line_indices <- c(line_indices, i)
            }
            else {
              pt_indices <- c(pt_indices, i)
            }
          }
          else if(typeof(water_geoms[[i]]) == "list") {
            polygon_indices <- c(polygon_indices, i)
          }
        }
        
        if(length(polygon_indices) > 0 || length(line_indices) > 0) {
          if(length(polygon_indices) > 0) {
            proxy %>% 
              addPolygons(data = water_geoms[[polygon_indices]], group = "water", color = "blue")
          }
          if(length(line_indices) > 0) {
            proxy %>% 
              addPolylines(data = water_geoms[[line_indices]], group = "water", color = "blue")
          }
          
          proxy %>% 
            addAwesomeMarkers(data = water_geoms[c(polygon_indices, line_indices)] %>% sf::st_centroid(),
                              group = "water",
                              icon = awesomeIcons(
                                icon =  as.character(my_icons[names(water_geoms)[c(polygon_indices, line_indices)]]),
                                iconColor = 'black',
                                markerColor = 'blue',
                                squareMarker = TRUE))
        }
        if(length(pt_indices) > 0) {
          proxy %>% 
            addAwesomeMarkers(data = water_geoms[pt_indices],
                              group = "water",
                              icon = awesomeIcons(
                                icon =  as.character(my_icons[names(water_geoms)[pt_indices]]),
                                iconColor = 'black',
                                markerColor = 'blue',
                                squareMarker = TRUE
                              ))
        }
        
        water_dists <- dist_points_to_water(points = data.frame(lat = pts$Latitude, lon = pts$Longitude), 
                                            water = water_geoms)
   
        pt_markers <- water_dists %>%
          select(lat, lon, closest_water) %>%
          mutate(closest_water = as.factor(closest_water),
                 water_label = factor(closest_water, labels = my_icons[levels(closest_water)]))
       
        pts$closest_water <- pt_markers$closest_water
        pts$water_label <- pt_markers$water_label
        
        for(ani in unique(pts$Animal)) {
          proxy %>% 
            addAwesomeMarkers(data = pts %>% subset(Animal == ani),
                              icon = awesomeIcons(icon =  ~water_label,
                                                  markerColor = ~color_label),
                              group = ani,
                              popup = ~ paste(
                                paste("<h4>", paste("Animal ID:", ani), "</h4>"),
                                paste("Date/Time:", pts$DateTime),
                                paste("Elevation:", pts$Elevation),
                                paste("Slope:", pts$Slope),
                                paste("Aspect:", pts$Aspect),
                                paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                                          ", ")),
                                paste("Closest Water:", pts$closest_water),
                                paste("LocationID:", pts$LocationID),
                                
                                sep = "<br/>"
                              ),
                              clusterOptions = markerClusterOptions()) 
        } # end plotting for loop
        
        if(water_uploaded()) {
          water_uploaded(FALSE)
        }
        
      } # if water closing bracket
      else {
        for(ani in unique(pts$Animal)) {
          proxy %>% 
            addAwesomeMarkers(data = pts %>% subset(Animal == ani),
                              icon = awesomeIcons(icon = "map-marker",
                                                  markerColor = ~color_label),
                              group = ani,
                              popup = ~ paste(
                                paste("<h4>", paste("Animal ID:", ani), "</h4>"),
                                paste("Date/Time:", pts$DateTime),
                                paste("Elevation:", pts$Elevation),
                                paste("Slope:", pts$Slope),
                                paste("Aspect:", pts$Aspect),
                                paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                                          ", ")),
                                paste("Closest Water:", pts$closest_water),
                                paste("LocationID:", pts$LocationID),
                                
                                sep = "<br/>"
                              ),
                              clusterOptions = markerClusterOptions()) 
        } # end plotting for loop
        
      } # else no water closing bracket
 
      if(!is.null(selected_locations())) {
        proxy %>% fitBounds(min(dat()$Longitude), min(dat()$Latitude), max(dat()$Longitude), max(dat()$Latitude))
        shinyjs::js$removePolygon()
      } # if selected locations closing bracket
      
      if(uploaded()) {
        uploaded(FALSE)
      }
    } # if closing bracket
    else if(fence_uploaded()) {
      for(kmz_element in kmz_coords()) {
        if(!is.matrix(kmz_element)) {
          df_point <- data.frame(lng = kmz_element[1], lat = kmz_element[2])
          proxy %>% 
            addCircleMarkers(
              data = df_point,
              group = "fencing",
              radius = 4,
              stroke = FALSE,
              weight = 3,
              opacity = .8,
              fillOpacity = 1,
              color = "black",
              fillColor = "black",
              popup = ~ paste(
                paste("Lat/Lon:", paste(kmz_element[2], kmz_element[1], sep =
                                          ", "))
              )
            ) 
        }
        else if(kmz_element[1, 1] == kmz_element[nrow(kmz_element), 1] &
                kmz_element[1, 2] == kmz_element[nrow(kmz_element), 2]) {
          proxy %>% 
            addPolygons(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "fencing")
        }
        else {
          proxy %>% 
            addPolylines(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "fencing")
        }
      }
      fence_uploaded(FALSE)
    }
    else if(!identical(last_drawn()$ani, current_anilist$ani)){
      # remove old points
      for(ani in setdiff(last_drawn()$ani, current_anilist$ani)) {
        proxy %>% clearGroup(ani)
      }
      
      if(length(kmz_coords()) > 0) {
        for(kmz_element in kmz_coords()) {
          if(!is.matrix(kmz_element)) {
            df_point <- data.frame(lng = kmz_element[1], lat = kmz_element[2])
            proxy %>% 
              addCircleMarkers(
                data = df_point,
                group = "fencing",
                radius = 4,
                stroke = FALSE,
                weight = 3,
                opacity = .8,
                fillOpacity = 1,
                color = "black",
                fillColor = "black",
                popup = ~ paste(
                  paste("Lat/Lon:", paste(kmz_element[2], kmz_element[1], sep =
                                            ", "))
                )
              ) 
          }
          else if(kmz_element[1, 1] == kmz_element[nrow(kmz_element), 1] &
                  kmz_element[1, 2] == kmz_element[nrow(kmz_element), 2]) {
            proxy %>% 
              addPolygons(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "fencing")
          }
          else {
            proxy %>% 
              addPolylines(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "fencing")
          }
        }
      }
      
      if(length(water_geoms) > 0) {
        water_dists <- dist_points_to_water(points = data.frame(lat = pts$Latitude, lon = pts$Longitude), 
                                            water = water_geoms)
       
        pt_markers <- water_dists %>%
          select(lat, lon, closest_water) %>%
          mutate(closest_water = as.factor(closest_water),
                 water_label = factor(closest_water, labels = my_icons[levels(closest_water)]))
        
        pts$closest_water <- pt_markers$closest_water
        pts$water_label <- pt_markers$water_label
        
        for(ani in unique(pts$Animal)) {
          proxy %>% 
            addAwesomeMarkers(data = pts %>% subset(Animal == ani),
                              icon = awesomeIcons(icon =  ~water_label,
                                                  markerColor = ~color_label),
                              group = ani,
                              popup = ~ paste(
                                paste("<h4>", paste("Animal ID:", ani), "</h4>"),
                                paste("Date/Time:", pts$DateTime),
                                paste("Elevation:", pts$Elevation),
                                paste("Slope:", pts$Slope),
                                paste("Aspect:", pts$Aspect),
                                paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                                          ", ")),
                                paste("Closest Water:", pts$closest_water),
                                paste("LocationID:", pts$LocationID),
                                
                                sep = "<br/>"
                              ),
                              clusterOptions = markerClusterOptions()) 
        } # end plotting for loop
        
      } # if water closing bracket
      else {
        for(ani in unique(pts$Animal)) {
          proxy %>% 
            addAwesomeMarkers(data = pts %>% subset(Animal == ani),
                              icon = awesomeIcons(icon =  "map-marker",
                                                  markerColor = ~color_label),
                              group = ani,
                              popup = ~ paste(
                                paste("<h4>", paste("Animal ID:", ani), "</h4>"),
                                paste("Date/Time:", pts$DateTime),
                                paste("Elevation:", pts$Elevation),
                                paste("Slope:", pts$Slope),
                                paste("Aspect:", pts$Aspect),
                                paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                                          ", ")),
                                paste("Closest Water:", pts$closest_water),
                                paste("LocationID:", pts$LocationID),
                                
                                sep = "<br/>"
                              ),
                              clusterOptions = markerClusterOptions()) 
        } # end plotting for loop
      } # else no water closing bracket

    } # if new points
    # add heatmap and layer control 
    if(length(kmz_coords()) > 0  & length(water_geoms()) > 0) {
      overlay <- c(unique(pts$Animal), "fencing", "water", "heat map")
    }
    else if(length(kmz_coords()) > 0) {
      overlay <- c(unique(pts$Animal), "fencing", "heat map")
    }
    else if(length(water_geoms()) > 0) {
      overlay <- c(unique(pts$Animal), "water", "heat map")
    }
    else {
      overlay <- c(unique(pts$Animal), "heatmap")
    }
  
    proxy %>% 
      addHeatmap(
        data = pts,
        group = "heat map",
        # intensity = pts$Elevation,
        blur = 20,
        max = 0.05,
        radius = 15
      ) %>%
      hideGroup("heat map") %>%  # turn off heatmap by default
      addLayersControl(
        baseGroups = c("satellite", "street map"),
        overlayGroups = overlay,
        options = layersControlOptions(collapsed = FALSE)
      )
    
      last_drawn(current_anilist)
      last_locations(selected_locations())
  }) # observe

  
  
  ######################################
  # DYNAMIC PLOTS PANEL
  ######################################
  
  # Elevation Line Plot
  output$plot_elevation_line <- callModule(reactivePlot, id = "plot_elevation_line", plot_type = "line", dat = dat)
  
  # Sample Rate Histograms
  output$plot_samplerate_hist <- callModule(reactivePlot, id = "plot_samplerate_hist", plot_type = "hist", dat = dat)
  
  # Rate by Animal
  output$plot_rate_violin <- callModule(reactivePlot, id = "plot_rate_violin", plot_type = "violin", dat = dat)
  
  # time spent by lat/long
  output$plot_time_heatmap <- callModule(reactivePlot, id = "plot_time_heatmap", plot_type = "heatmap", dat = dat)
  
  
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