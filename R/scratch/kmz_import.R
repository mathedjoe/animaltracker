library(maptools)
library(leaflet)
library(rgdal)

# get coordinates

kmz_coords <- getKMLcoordinates(kmlfile = unzip(zipfile = "test_data/PastureFiles/McIntyre Pasture.kmz", 
                                                exdir = "test_data/PastureFiles"),
                                ignoreAltitude = TRUE)

# loop through sets of coordinates (lapply)
# extract points, paths, and polygons
# look at first dimension (number of points)
# if first dimension = 1
# return spatial point for map
# else if first point = last point
# return spatial polygon for map
# else
# return path for map

test_poly <- as.data.frame(kmz_coords[[5]])

states <- readOGR("test_data/states_21basic/states.shp")
myStates <- subset(states, states$STATE_ABBR %in% c("AZ", "NM","OR","ID") )
leaflet(myStates) %>%
  addTiles() %>% 
  addPolygons(data = test_poly, lng = ~V1, lat = ~V2)

proxy <- leafletProxy("mainmap", session)
extract_kmz_coords <- function(map, kmz_element) {
  if(!is.matrix(kmz_element)) {
    map %>% 
      addCircleMarkers(
        lng = kmz_element[1],
        lat = kmz_element[2],
        radius = 4,
        stroke = FALSE,
        weight = 3,
        opacity = .8,
        fillOpacity = 1,
        popup = ~ paste(
          paste("Lat/Lon:", paste(kmz_element[2], kmz_element[1], sep =
                                    ", "))
        )
      ) 
  }
  else if(kmz_element[1, 1] == kmz_element[nrow(kmz_element), 1] &
          kmz_element[1, 2] == kmz_element[nrow(kmz_element), 2]) {
    map %>% 
      addPolygons(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2)
  }
  else {
    map %>% 
      addPolylines(data = as.data.frame(kmz_element), lng = ~V1, lat = ~V2)
  }
    
}

lapply(kmz_coords, extract_kmz_coords, map = leaflet(myStates))

for(i in length(kmz_coords)) {
  extract_kmz_coords(leaflet(myStates), kmz_coords[[i]])
  
}
