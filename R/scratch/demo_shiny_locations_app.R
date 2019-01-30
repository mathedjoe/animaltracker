## Allow users to subset data by choosing a region of a leaflet map
# modeled after the geoshaper app at https://redoakstrategic.com/geoshaper/
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)
print(getwd())
source("find_locations_function.R") # this should move into the package code

# source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
airports <- read.csv('airport_codes.csv')

# longitudinal coordinates in dataset are off, reverse to negative values to place them in the western hemisphere
airports$Longitude <- airports$Longitude - 2 * airports$Longitude

# generate second set of unique location IDs for second layer of selected locations
airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep="")

coordinates <- SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')] , airports)

head(airports)

shinyApp(
  ui <- fluidPage(
    leafletOutput("mymap")
  ),
  
  server <- function(input, output) {
    
    ################################################# section one #################################################
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())
    
    ################################################# section two #################################################
    # base map
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircles(data = airports,
                   radius = 1000,
                   lat = airports$Latitude,
                   lng = airports$Longitude,
                   fillColor = "white",
                   fillOpacity = 1,
                   color = "hotpink",
                   weight = 2,
                   stroke = T,
                   layerId = as.character(airports$locationID),
                   highlightOptions = highlightOptions(color = "mediumseagreen",
                                                       opacity = 1.0,
                                                       weight = 2,
                                                       bringToFront = TRUE)) %>%
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          circleOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'white'
                                                                            ,weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                ,color = 'white'
                                                                                ,weight = 3)),
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    })
    
    ############################################### section three #################################################
    observeEvent(input$mymap_draw_new_feature,{
      #Only add new layers for bounded locations
      found_in_bounds <- find_locations(shape = input$mymap_draw_new_feature
                                       , location_coordinates = coordinates
                                       , location_id_colname = "locationID")
      
      for(id in found_in_bounds){
        if(id %in% data_of_click$clickedMarker){
          # don't add id
        } else {
          # add id
          data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
        }
      }
      
      # look up airports by ids found
      selected <- subset(airports, locationID %in% data_of_click$clickedMarker)
      
      proxy <- leafletProxy("mymap")
      proxy %>% addCircles(data = selected,
                           radius = 1000,
                           lat = selected$Latitude,
                           lng = selected$Longitude,
                           fillColor = "wheat",
                           fillOpacity = 1,
                           color = "mediumseagreen",
                           weight = 3,
                           stroke = T,
                           layerId = as.character(selected$secondLocationID),
                           highlightOptions = highlightOptions(color = "hotpink",
                                                               opacity = 1.0,
                                                               weight = 2,
                                                               bringToFront = TRUE))
      
    })
    
    ############################################### section four ##################################################
    observeEvent(input$mymap_draw_deleted_features,{
      # loop through list of one or more deleted features/ polygons
      for(feature in input$mymap_draw_deleted_features$features){
        
        # get ids for locations within the bounding shape
        bounded_layer_ids <- find_locations(shape = feature
                                           , location_coordinates = coordinates
                                           , location_id_colname = "secondLocationID")
        
        
        # remove second layer representing selected locations
        proxy <- leafletProxy("mymap")
        proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
        
        first_layer_ids <- subset(airports, secondLocationID %in% bounded_layer_ids)$locationID
        
        data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                   %in% first_layer_ids]
      }
    })
  },
  
  options = list(height = 400)
)
