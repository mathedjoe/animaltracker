#'
#'Retrieve and save high resolution elevation data for the region of analysis from the internet
#'
#'@param latmin minimum latitude for bounding box (degrees)
#'@param latmax maximum latitude for bounding box (degrees)
#'@param lonmin minimum longitude for bounding box (degrees)
#'@param lonmax maximum longitude for bounding box (degrees)
#'@param out_dir folder path to save the elevation data
#'@param zoom level of zoom, defaults to 12
#'@param zone geographic zone, defaults to 11
#'@return elevation data as spatial points
#'@export
#'
get_elevation <- function(latmin, latmax, lonmin, lonmax, out_dir, zoom = 12, zone =11) {
  
  data_region <- sp::bbox(cbind(c(lonmin, lonmax), c(latmin,latmax))) # set a bounding box for retrieval of elev data
  
  elev <- elevatr::get_aws_terrain( data_region, z=zoom, prj = "+proj=longlat") # retrieve high res elev data
  
  elev2 <- raster::projectRaster(elev, crs = paste0("+proj=utm +zone=", zone, " ellps=WGS84") )
  
  elevpts <- raster::rasterToPoints(elev2, spatial=TRUE) # convert to spatial pts
  
  if(!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  }
  saveRDS(elevpts, file.path(out_dir, "elev_data.rds"))
  
  return(elevpts)
}

# get_elevation(latmin = 43.1,
#               latmax = 43.5,
#               lonmin = -117.6,
#               lonmax = -117, 
#               out_dir = "data/elevation") # doesn't work?

#'
#'Model elevation from GPS data (provided csv)
#'
#'@param csv_path path of csv GPS data
#'@param latmin minimum latitude for bounding box (degrees)
#'@param latmax maximum latitude for bounding box (degrees)
#'@param lonmin minimum longitude for bounding box (degrees)
#'@param lonmax maximum longitude for bounding box (degrees)
#'@param zoom level of zoom, defaults to 12
#'@param zone geographic zone, defaults to 11
#'@return modeled elevation data
#'@export
#'
model_animal_elevation <- function(csv_path, latmin, latmax, lonmin, lonmax, zoom = 12, zone = 11) {
  data_region <- sp::bbox(cbind(c(lonmin, lonmax), c(latmin,latmax))) 
  elev <- elevatr::get_aws_terrain( data_region, z=zoom, prj = "+proj=longlat")
  elev2 <- raster::projectRaster(elev, crs = paste0("+proj=utm +zone=", zone, " ellps=WGS84") )
  elevpts <- raster::rasterToPoints(elev2, spatial=TRUE)
  datapts <- read.csv(csv_path) %>%
    select(lon = Longitude, lat = Latitude, alt = Altitude )
  
  datapts <- read.csv(csv_path) %>%
    select(x = Longitude, y = Latitude)
  datapts <- rgdal::project(as.matrix(datapts), "+proj=utm +zone=11 ellps=WGS84")
  datapts <- as.data.frame(datapts)
  datapts$alt <- read.csv(csv_path)$Altitude
  coordinates(datapts) <- ~x+y
  
  datapts_elev <- nabor::knn(coordinates(elevpts), coordinates(datapts), k=1)
  
  datapts$elev <- elevpts$layer[ datapts_elev$nn.idx]
  return(datapts)
}


#'
#'Export modeled elevation data from existing animal data file
#'
#'@param rds_path animal tracking data file to model elevation from
#'@param out_path exported file path
#'@param latmin minimum latitude for bounding box (degrees)
#'@param latmax maximum latitude for bounding box (degrees)
#'@param lonmin minimum longitude for bounding box (degrees)
#'@param lonmax maximum longitude for bounding box (degrees)
#'@param zoom level of zoom, defaults to 12
#'@param zone geographic zone, defaults to 11
#'@return list of data frames with gps data augmented by elevation
#'@export
#'
export_animal_elevation <- function(rds_path, out_path, latmin, latmax, lonmin, lonmax, zoom = 12, zone =11) {
  anidata <- readRDS(rds_path)
  data_region <- sp::bbox(cbind(c(lonmin, lonmax), c(latmin,latmax))) # set a bounding box for retrieval of elev data
  elev <- elevatr::get_aws_terrain( data_region, z=zoom, prj = "+proj=longlat") # retrieve high res elev data
  elev2 <- raster::projectRaster(elev, crs = paste0("+proj=utm +zone=", zone, " ellps=WGS84") )
  elevpts <- raster::rasterToPoints(elev2, spatial=TRUE) # convert to spatial pts
  for ( i in 1:length(anidata) ){
    anidf <- anidata[[i]]
    datapts <- as.matrix(anidf[c("Longitude", "Latitude")] )
    colnames(datapts) <- c("x","y")
    datapts <- rgdal::project(as.matrix(datapts), "+proj=utm +zone=11 ellps=WGS84")
    datapts <- as.data.frame(datapts)
    datapts$alt <- anidf$Altitude
    coordinates(datapts) <- ~x+y
    
    datapts_elev <- nabor::knn(coordinates(elevpts), coordinates(datapts), k=1)
    
    anidf$Elevation <- elevpts$layer[ datapts_elev$nn.idx]
    
    anidata[[i]]<- anidf
    
  }
  saveRDS(anidata, out_path)
  return(anidata)
}

#'
#'Generate a histogram of the distribution of modeled elevation - measured altitude
#'
#'@param csv_path path of csv GPS data to model elevation from
#'@return histogram of the distribution of modeled elevation - measured altitude
#'@export
#'
histogram_animal_elevation <- function(csv_path) {
  datapts <- model_animal_elevation(csv_path)
  histogram <- ggplot(as.data.frame(datapts), aes(x = elev - alt)) +
    xlim(-100,100)+
    geom_histogram(aes(y=..density..), colour="blue", fill="lightblue", binwidth = 2 )+
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept = mean((elev-alt)[abs(elev-alt) <= 100])),col='blue',size=2)+
    labs(title = "Distribution of Modeled Elevation - Measured Altitude (meters)")+
    theme_minimal()
  return(histogram)
}


#
#Clean and merge GPS data, add elevation data, save to rds for use in a shiny app
#
#@param gps_data_dir directory for raw gps animal data files
#@param dem_data_dir directory for elevation data file (.tif)
#@return list of data frames containing gps and elevation data augmented
#