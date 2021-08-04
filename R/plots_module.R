#'
#''shiny' module UI output for the animaltracker app's plots tab.
#'
#'@param id chosen ID of UI output
#'@return 'shiny' plotOutput object
#'@noRd
#'
reactivePlotOutput <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot_out"))
}

#'
#''shiny' module server-side UI generator for the animaltracker app's summary statistics tables.
#'
#'@param input 'shiny' server input, automatically populated
#'@param output 'shiny' server output, automatically populated
#'@param session 'shiny' server session, automatically populated
#'@param plot_type plot type to generate
#'@param dat animal data frame 
#'@return 'shiny' renderPlot object
#'@noRd
#'
reactivePlot <- function(input, output, session, plot_type, dat) {
  req(dat)
    if(plot_type == "line") {
      output$plot_out <- renderPlot({
        ggplot(dat(), aes(x=DateTime, y=Elevation, group=Animal, color=Animal)) + 
          labs( title = "Elevation Time Series, by Animal",
                x = "Date",
                y = "Elevation (meters)") +
          ylim(1000,2000) + 
          geom_line(na.rm = TRUE) + 
          geom_point(na.rm = TRUE) + 
          theme_minimal()
      })
    }
    else if(plot_type == "hist") {
      output$plot_out <- renderPlot({
        ggplot(dat(), aes(x=TimeDiffMins, fill=Animal)) +
          geom_histogram(  col="White", breaks = seq(0,40, 2)) +
          facet_wrap(~Animal, ncol=2)+
          labs( title = "Sample Rate, by GPS Unit" ,
                x = "Time between GPS Readings (minutes)", 
                y = "Frequency") + 
          theme_minimal()
      })
    }
    else if(plot_type == "violin") {
      output$plot_out <- renderPlot({
        ggplot(dat() %>% dplyr::filter(Rate < 50), aes(x=Animal, y= Rate, fill=Animal))+
          geom_violin() + 
          geom_boxplot(width=.2, outlier.color = NA) +
          theme_minimal()+
          labs( title = "Rate of Travel, by GPS Unit" ,
                x = "Animal", 
                y = "Rate of Travel (meters/minute)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
    else if(plot_type == "heatmap") {
      mybreaks <- reactive({
        list(x = round( seq(min(dat()$Longitude), max(dat()$Longitude), length.out = 10 ),3),
            y = round( seq(min(dat()$Latitude), max(dat()$Latitude), length.out = 10 ),3))
      })
      output$plot_out <- renderPlot({
        ggplot(dat() %>% 
               dplyr::mutate( LongBin = cut_number(Longitude, 100, 
                                                   labels= round( seq(min(Longitude), max(Longitude), length.out = 100 ),3)
               ),
               LatBin = cut_number(Latitude, 100, 
                                   labels=round( seq(min(Latitude), max(Latitude), length.out = 100 ), 3)
               )) %>%
               group_by(LongBin, LatBin, Animal) %>%
               summarize(Duration = sum(TimeDiffMins, na.rm = TRUE)/60), 
             aes (x = LongBin,  y = LatBin, fill = Duration))+
        geom_tile() +
        facet_wrap(~Animal, ncol=2)+
        labs( title = "Total Time Spent per Location (hours)" ,
              x = "Longitude", 
              y = "Latitude")+
        scale_fill_gradientn(colors = c("white", "green", "red")) +
        scale_x_discrete( breaks = mybreaks()$x) +
        scale_y_discrete( breaks = mybreaks()$y) +
        coord_equal() +
        theme_minimal()
      }, height = 1200)
    }
}

#'
#'Helper function for plotting geographic features on app leaflet map
#'
#'@param proxy leaflet proxy object
#'@param kmz_coords list of geographic feature coordinates 
#'@noRd
#'
plot_geographic_features <- function(proxy, kmz_coords) {
    for(kmz_element in kmz_coords) {
      if(!is.matrix(kmz_element)) {
        df_point <- data.frame(lng = kmz_element[1], lat = kmz_element[2])
        proxy %>% 
          addCircleMarkers(
            data = df_point,
            group = "geographic feature",
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
          addPolygons(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "geographic feature")
      }
      else {
        proxy %>% 
          addPolylines(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2, group = "geographic feature")
      }
    }
}

#'
#'Helper function for plotting water sources and distances to water on app leaflet map
#'
#'@param proxy leaflet proxy object
#'@param pts animal SpatialPointsDataFrame 
#'@param water_geoms list of sf geometries representing water sources
#'@param custom_icon_list list of glyphicons for each water source
#'@noRd
#'
plot_water_sources <- function(proxy, pts, water_geoms, custom_icon_list) {
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
        addPolygons(data = water_geoms[[polygon_indices]], group = "water source", color = "blue")
    }
    if(length(line_indices) > 0) {
      proxy %>% 
        addPolylines(data = water_geoms[[line_indices]], group = "water source", color = "blue")
    }
    
    proxy %>% 
      addAwesomeMarkers(data = water_geoms[c(polygon_indices, line_indices)] %>% sf::st_centroid(),
                        group = "water source",
                        icon = awesomeIcons(
                          icon =  as.character(my_icons[names(water_geoms)[c(polygon_indices, line_indices)]]),
                          iconColor = 'black',
                          markerColor = 'blue',
                          squareMarker = TRUE))
  }
  if(length(pt_indices) > 0) {
    proxy %>% 
      addAwesomeMarkers(data = water_geoms[pt_indices],
                        group = "water source",
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
                          paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                                    ", ")),
                          paste("Elevation:", pts$Elevation),
                          paste("Slope:", pts$Slope),
                          paste("Aspect:", pts$Aspect),
                          paste("Wind Direction:", pts$wind_direction),
                          paste("Wind Speed:", pts$wind_speed),
                          paste("Temperature:", pts$temperature),
                          paste("Dewpoint Temp:", pts$temperature_dewpoint),
                          paste("Air Pressure:", pts$air_pressure),
                          paste("Closest Water:", pts$closest_water),
                          paste("LocationID:", pts$LocationID),
                          
                          sep = "<br/>"
                        ),
                        clusterOptions = markerClusterOptions()) 
  } # end plotting for loop
}

#'
#'Helper function for plotting animal locations on app leaflet map
#'
#'@param proxy leaflet proxy object
#'@param pts animal SpatialPointsDataFrame
#'@noRd
#'
plot_animal_points <- function(proxy, pts) {
  for(ani in unique(pts$Animal)) {
    proxy %>% 
      addAwesomeMarkers(data = pts %>% subset(Animal == ani),
                        icon = awesomeIcons(icon = "map-marker",
                                            markerColor = ~color_label),
                        group = ani,
                        popup = ~ paste(
                          paste("<h4>", paste("Animal ID:", ani), "</h4>"),
                          paste("Date/Time:", pts$DateTime),
                          paste("Lat/Lon:", paste(pts$Latitude, pts$Longitude, sep =
                                                    ", ")),
                          paste("Elevation:", pts$Elevation),
                          paste("Slope:", pts$Slope),
                          paste("Aspect:", pts$Aspect),
                          paste("Wind Direction:", pts$wind_direction),
                          paste("Wind Speed:", pts$wind_speed),
                          paste("Temperature:", pts$temperature),
                          paste("Dewpoint Temp:", pts$temperature_dewpoint),
                          paste("Air Pressure:", pts$air_pressure),
                          paste("Closest Water:", pts$closest_water),
                          paste("LocationID:", pts$LocationID),
                          
                          sep = "<br/>"
                        ),
                        clusterOptions = markerClusterOptions()) 
  } 
}