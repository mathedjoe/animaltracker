#'
#'Retrieve high resolution elevation data for the region of analysis from the internet
#'
#'@param latmin minimum latitude for bounding box (degrees)
#'@param latmax maximum latitude for bounding box (degrees)
#'@param lonmin minimum longitude for bounding box (degrees)
#'@param lonmax maximum longitude for bounding box (degrees)
#'@return elevation data as spatial points
#'
get_elevation <- function(latmin, latmax, lonmin, lonmax) {
  data_region <- sp::bbox(cbind(c(lonmin, lonmax), c(latmin,latmax))) # set a bounding box for retrieval of elev data

  elev <- get_aws_terrain( data_region, z=12, prj = "+proj=longlat") # retrieve high res elev data

  elev2 <- projectRaster(elev, crs = "+proj=utm +zone=11 ellps=WGS84")

  elevpts <- raster::rasterToPoints(elev2, spatial=TRUE) # convert to spatial pts
  return(elevpts)
}

#'
#'Save high resolution elevation data to computer as .rds
#'
#'@param out_path file path to save elevation data to
#'
export_elevation <- function(out_path) {
  saveRDS(get_elevation(), out_path)
}

#'
#'Model elevation from GPS data (provided csv)
#'
#'@param csv_path path of csv GPS data
#'@return modeled elevation data
#'
model_animal_elevation <- function(csv_path) {
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
#'
export_animal_elevation <- function(rds_path, out_path) {
  anidata <- readRDS(rds_path)

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
}

#'
#'Generate a histogram of the distribution of modeled elevation - measured altitude
#'
#'@param csv_path path of csv GPS data to model elevation from
#'@return histogram of the distribution of modeled elevation - measured altitude
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
