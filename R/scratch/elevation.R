#'
#'Generate spatial points data frame from existing GPS data (.rds)
#'
#'@param rds_path path of GPS data (.rds)
#'@return spatial points data frame
#'
get_spdf <- function(rds_path) {
  df <- readRDS(rds_path)
  print(names(df))
  spdf <- sp::SpatialPointsDataFrame(coords = df[c("Longitude", "Latitude")], data = df,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  return(spdf)
}


#'
#'Estimate elevation data via Digital Elevation Model (DEM) from existing GPS data (.rds)
#'
#'@param rds_path path to the GPS data (.rds)
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

## Elevation data download benchmarking

library(dplyr)
library(rbenchmark)
library(animaltracker)

demo_raw <- demo %>% dplyr::select(-c("Elevation", "Slope", "Aspect"))

benchmark(lookup_elevation_aws(demo_raw, zoom = 1), replications = 1) # 4.81 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 2), replications = 1) # 5.05 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 3), replications = 1) # 5.52 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 4), replications = 1) # 5.23 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 5), replications = 1) # 5.4 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 6), replications = 1) # 5.59 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 7), replications = 1) # 5.32 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 8), replications = 1) # 5.4 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 9), replications = 1) # 5.41 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 10), replications = 1) # 7.38 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 11), replications = 1) # 14.53 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 12), replications = 1) # 28.9 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 13), replications = 1) # 103.69 seconds

benchmark(lookup_elevation_aws(demo_raw, zoom = 14), replications = 1) # 320.67 seconds

# zoom 14 is max?


