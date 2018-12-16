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

#'
#'Estimate elevation data via Digital Elevation Model (DEM) from existing GPS data (.rds)
#'
#'@param rds_path path of GPS data
#'@param tif_dir local directory path of DEM files (.tif)
#'@return projection of elevation points on spatial points from GPS data
#'
estimate_elevation_dem <- function(rds_path, tif_dir) {
  spdf <- get_spdf(rds_path)
  dem_file <- list.files(tif_dir, pattern=".tif", full.names = T)
  DEM <- raster(dem_file)
  DEM.gps <- projectRaster(DEM, crs = crs(spdf))
  return(DEM.gps)
}




