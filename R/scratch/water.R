library(maptools)
library(tidyverse)
library(leaflet)
# library(rgdal)
# library(usmap)

######################
### EXAMPLE CODE FOR COMPUTING DISTANCE TO WATER
### WATER  = FUNNY GEOMETRIC SHAPES
### POINTS = RANDOM GENERATED
library(sf)
library(ggplot2)

# make a list of "water" objects
outer <- matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole <- matrix(c(0,0,0,1,1,1,1,0,0,0),ncol=2, byrow=TRUE)
hole1 <- hole + c(2,2)
hole2 <- 3*hole+c(5,3)

my_water <- st_multipolygon(list(
  list(outer, hole1, hole2),
  list(1.2*outer + c(12,3), hole1 + 12)
))
####
# main function to find distance between n points and the water objects
# points can be a number of points to generate, or data.frame with lat/lon coordinates
dist_points_to_water <- function(points, xlim = c(0,25), ylim = c(0,25), water = my_water, plot_points = 0){
  
  # points can be a number of points to generate, or data.frame with lat/lon coordinates
  if (length(points) == 1){
    points_data <- data.frame( lat = runif(points, xlim[1], xlim[2]),
                               lon = runif(points, ylim[1], ylim[2]))
    
  }
  else{
    points_data <- points
  }
  
  # convert point data to sf
  pts <- st_as_sf(points_data, coords = c("lon", "lat"))
  
  # compute pairwise distance from each point to each water feature
  if( length(water) * nrow(points_data) > 10^6) {
    print(paste0("Warning: ", length(water) * nrow(points_data),
                 " distances to calculate, this may take a long time.")
    )
  }
  # pairwise distances
  dist_to_water <-   st_distance(pts, water)
  
  # return(dist_to_water) ## DELETE ME
  
  # find closest distance to water from pairwise distances
  dist_to_water <- bind_cols(points_data, as.data.frame(dist_to_water)) %>%
    mutate(index = 1:n()) %>%
    pivot_longer( contains('V'), 'water_object', 'dist', values_to = "distance") %>%
    group_by(index) %>%
    mutate(min_dist = min(distance),
           closest_water = water_object[distance == min_dist]) %>%
    pivot_wider( names_from = water_object, values_from = distance, names_prefix = "dist_") %>%
    ungroup() %>% select(-index)
  
  out <- list( pts = dist_to_water )
  
  # show a map
  if( plot_points > 0 ){
    plot_points <- min ( plot_points, nrow(points_data))
    sample_show <- sample(1:nrow(points_data), plot_points)
    waterbb <- st_bbox(water)
    ynudge <- .03*(waterbb$ymax - waterbb$ymin)
    
    map_plot <- ggplot() +
      geom_sf(data = water, fill = "#eeeeee", size = 3, color = "#333333" )+
      geom_sf(data = pts[sample_show,], size = 3,
              aes(color = dist_to_water$min_dist[sample_show],
                  shape = dist_to_water$closest_water[sample_show]) ) +
      scale_color_gradient(low="darkgreen", high="red")+
      geom_sf_text(data = water, aes(label = paste0("V", 1:length(water))), size = 3, nudge_y = ynudge  ) +
      labs(color = "Distance to Water", shape = "Nearest Object", x ="", y = "")+
      theme_minimal()
    
    
    
    print(map_plot)
    
    out$plot <- map_plot
    
  }
  
  out
  
}

# manually validate measurements for a sample of points near one of the holes
rand_dist10 <- dist_points_to_water(500, plot_points = 500)
rand_dist10$pts %>%
  filter(abs(lat-5)<2, abs(lon-5) <2, dist_V1 != 0 ) # 2 unit square in the "chevron" hole, should have distances less than 1

# profile performance
# system.time({
#  rand_dist10_4 <- dist_points_to_water(10^4, plot_points = 200)
# })
#
# system.time({
#  rand_dist10_6 <- dist_points_to_water(10^6, plot_points = 200)
# })
## > seems fast enough
######################

######################
### EXAMPLE CODE FOR COMPUTING DISTANCE TO WATER
### WATER  = KML FILE
### POINTS = DEMO PROCESSED GPS DATA

# get demo coordinates of animals
demo_pts <- animaltracker::demo %>%
  mutate(index = 1:n()) %>%
  select(index, lat = Latitude, lon = Longitude, elev = Elevation, ani = Animal)


# get demo coordinates of water
kmz_coords <- getKMLcoordinates(kmlfile = unzip(zipfile = "test_data/PastureFiles/McIntyre Pasture.kmz",
                                                exdir = "test_data/PastureFiles"),
                                ignoreAltitude = TRUE)

# function to convert kml coordinates of water features to sf for mapping and calculations
kmz_to_sf <- function(kmz_element, shift = c(0,0)){
  if(!is.matrix(kmz_element)) {
    ## it's one point
    return( st_point(kmz_element, dim = "XY") + shift )
  }
  
  # it's a list of points
  if(kmz_element[1, 1] == kmz_element[nrow(kmz_element), 1] &
     kmz_element[1, 2] == kmz_element[nrow(kmz_element), 2]){
    ## it's a polygon
    return(st_polygon(list(as.matrix(kmz_element))) + shift )
    
  }
  else {
    ## it's a polygonal line
    # print(t(kmz_element))
    return(st_linestring(kmz_element, dim = "XY") + shift)
  }
}

#### demo water geometry features
## read from kml, shift to make more interesting :)
water_geoms <- lapply(kmz_coords, kmz_to_sf, shift =  c(.07,-.15))  # combine the sf list into a "sf collection"
# water_geoms$name <- paste0("V",1:length(water_geoms))

# move the big polygon
water_geoms[[5]] <- water_geoms[[5]] + c(-.05,.05)

bonus_water <- st_polygon(list(
  .02*matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2, byrow=TRUE)
))  + c(-117.22, 43.28)



water_geoms[["bonus"]] <- bonus_water
water_geoms <- st_sfc( water_geoms )

# compute min distances to water
# expanded bounding box for the water features
bb <- st_bbox(water_geoms)
pt_box <- list(xlim = c(bb[1]-.1, bb[3]+.1), ylim = c(bb[2]-.1, bb[4]+.1)) # expanded bounding box around the water

water_dists_test <- dist_points_to_water(points = demo_pts,  water = water_geoms, plot_points = 120)

water_dists_test$plot + ggsave( "R/scratch/explore_viz/water_distances.png")

### Visualize the results

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

ggplot() +
  geom_sf(data = states %>% filter(ID %in% c("idaho", "oregon", "nevada")), fill = "white" ) +
  geom_sf(data = water_geoms , aes(fill = , color = "black", size = 2) )+
  coord_sf(pt_box$xlim, pt_box$ylim , expand = TRUE) +
  theme_bw()


#### demo water geometry features
## read from kml, shift to make more interesting :)
water_geoms <- lapply(kmz_coords, kmz_to_sf, shift =  c(.07,-.15))  # combine the sf list into a "sf collection"

# move the big polygon
water_geoms[[5]] <- water_geoms[[5]] + c(-.05,.05)

bonus_water <- st_polygon(list(
  .02*matrix(c(0,0,1,0,1,1,0,1,0,0),ncol=2, byrow=TRUE)
))  + c(-117.22, 43.28)

water_geoms[["bonus"]] <- bonus_water
names(water_geoms) <- paste0("V",1:length(water_geoms))

water_geoms <- st_sfc( water_geoms )

# compute min distances to water
# expanded bounding box for the water features
bb <- st_bbox(water_geoms)
pt_box <- list(xlim = c(bb[1]-.1, bb[3]+.1), ylim = c(bb[2]-.1, bb[4]+.1)) # expanded bounding box around the water

water_dists_test <- dist_points_to_water(points = demo_pts,  water = water_geoms, plot_points = 120)

### VISUALIZE IN GGPLOT
water_dists_test$plot + ggsave( "R/scratch/explore_viz/water_distances.png")

### VISUALIZE IN LEAFLET
library(leaflet)

# follow https://rstudio.github.io/leaflet/markers.html to use custom icons for markers
# make a list of icons (ideally longer than we need)
custom_icon_list <- list("asterisk", "plus", "star", "heart", "ok", "map-marker",
                         "stop", "remove-circle", "minus-sign", "eye-open", "bell")

my_icons <- custom_icon_list[1:length(names(water_geoms))]
names(my_icons) <- names(water_geoms)
pt_markers <- water_dists_test$pts %>%
  select(lat, lon, closest_water) %>%
  mutate(closest_water = as.factor(closest_water),
         water_label = factor(closest_water, labels = my_icons[levels(closest_water)]))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = water_geoms[[5]], color = "blue") %>%
  addPolylines(data = water_geoms[[6]], color = "blue") %>%
  # centroids
  addAwesomeMarkers(data = water_geoms[c(5, 6, 10)] %>% st_centroid(),
                    icon = awesomeIcons(
                      icon =  as.character(my_icons[names(water_geoms)[c(5, 6, 10)]]),
                      iconColor = 'black',
                      markerColor = 'blue',
                      squareMarker = TRUE)) %>% 
  addAwesomeMarkers(data = water_geoms[c(1:4,7:9)],
                    icon = awesomeIcons(
                      icon =  as.character(my_icons[names(water_geoms)[c(1:4, 7,9)]]),
                      iconColor = 'black',
                      markerColor = 'blue',
                      squareMarker = TRUE
                    )) %>%
  addAwesomeMarkers(data = pt_markers,
                    icon = awesomeIcons(icon =  ~water_label),
                    clusterOptions = markerClusterOptions( freezeAtZoom = 16, spiderfyOnMaxZoom = TRUE)) 

