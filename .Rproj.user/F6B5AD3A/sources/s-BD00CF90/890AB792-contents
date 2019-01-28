#' Find locations inside a polygon, square, or circle drawn with leaflet.extras drawing tools on a Shiny Leaflet map.
#'
#' @param shape Shiny input (input$MAPID_draw_new_feature), representing shape drawn on the map by the user.
#' @param location_coordinates A SpatialPointsDataFrame containing coordinates and ids for all map locations.
#' @param location_id_colname Column name from location_coordinates containing desired names or ids for set of locations returned.
#' @return A vector of location ids.
#' @examples
#' mock_input.map_feature <- list(type = "Feature"
#'                          , properties = list(`_leaflet_id`= 13477, feature_type = "rectangle")
#'                          , geometry = list(type = "Polygon"
#'                          , coordinates = list(list(list(-76.15723, 39.51252)
#'                          , list(-76.15723,  40.30467), list(-74.73999, 40.30467)
#'                          , list(-74.73999, 39.51252), list(-76.15723, 39.51252)))))
#' airports <- data.frame('locationID' = c('PHL', 'DTW')
#'                       , 'Longitude' = c(-75.2408, -83.3533)
#'                       , 'Latitude' = c(39.8722, 42.2125))
#' coords = sp::SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')], airports)
#' findLocations(shape = mock_input.map_feature
#'                      , location_coordinates = coords
#'                      , location_id_colname = "locationID")


find_locations <- function(shape, location_coordinates, location_id_colname) {
  
  # print(head(coordinates(location_coordinates)))
  
  
    
    print(head(selected_loc_id))
    
    return(selected_loc_id)
    
}