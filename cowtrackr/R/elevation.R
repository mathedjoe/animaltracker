#'
#'Generate spatial points data frame from existing GPS data (.rds)
#'
#'@param rds_path path of GPS data (.rds)
#'@return spatial points data frame
#'
get_spdf <- function(rds_path) {
  df <- readRDS(rds_path)
  spdf <- SpatialPointsDataFrame(coords = df[c("Longitude", "Latitude")], data = df,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  return(spdf)
}


#'
#'Estimate elevation data with USGS API from existing GPS data (.rds)
#'
#'@param rds_path path of GPS data (.rds)
#'@return elevation points based on GPS data
#'
estimate_elevation_usgs <- function(rds_path) {
  spdf <- get_spdf(rds_path)
  elev_points <- get_elev_point(spdf, src = "epqs") # ONE BY ONE (Super Slow!)
  return(elev_points)
}


#### add elevation data via Digital Elevation Model (DEM)


# read geoTIFF elevation map
# dem_file <- list.files(tif_dir, pattern=".tif", full.names = T)

# DEM <- raster(dem_file)
# plot(DEM, main="Digital Elevation Model for U.S.")

# extent(DEM)
# crs(DEM)

# DEM.spf <- as(DEM,"SpatialPixelsDataFrame") # takes a long time?

# DEM.gps <- projectRaster(DEM, crs = crs(spdf))


