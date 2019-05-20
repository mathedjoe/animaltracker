#'
#'Add elevation data from public AWS terrain tiles to long/lat coordinates of animal gps data
#'
#'@param anidf animal tracking dataframe
#'@param zoom level of zoom, defaults to 11
#'@param get_slope logical, whether to compute slope (in degrees)
#'@param get_aspect logical, whether to compute aspect (in degrees)
#'@return original data frame, with terrain column(s) appended
#'@examples 
#' data(demo)
#' library(dplyr)
#' xelev <- lookup_elevation(demo, zoom = 11)
#' plot(xelev$Altitude, xelev$Elevation)
#' @export
lookup_elevation <- function(anidf, zoom = 11, get_slope=TRUE, get_aspect=TRUE) {
  
  # extract coordinates from the animal data
  locations <- anidf %>% dplyr::select(x = Longitude, y = Latitude)
  
  # retrieve terrain data for the region containing the animal data
  ## USGS DEM source = Amazon Web Services (https://aws.amazon.com/public-datasets/terrain/) terrain tiles.
  elev <- elevatr::get_elev_raster(locations, prj = "+proj=longlat", z=zoom)
  
  # convert terrain data to spatial pts
  elevpts <- raster::rasterToPoints(elev, spatial=TRUE)
  
  # determine nearest neighbors in the terrain data for the animal locations
  datapts_elev <- nabor::knn(data = sp::coordinates(elevpts), query = locations, k=1)
  
  # add Elevation and Slope columns to the animal data
  anidf$Elevation <- round(elevpts$layer[ datapts_elev$nn.idx ], 1)
  
  if(get_slope | get_aspect){
    elev_terr <- terrain(elev, opt=c('slope', 'aspect'), unit='degrees')
  }
  
  if(get_slope){
    slope <- elev_terr$slope
    slopepts <- raster::rasterToPoints(slope, spatial=TRUE)
    anidf$Slope <- round(slopepts$slope[ datapts_elev$nn.idx  ], 1)
  }
  
  if(get_aspect){
    aspect <- elev_terr$aspect
    aspectpts <- raster::rasterToPoints(aspect, spatial=TRUE)
    anidf$Aspect <- round(aspectpts$aspect[ datapts_elev$nn.idx  ], 1)
  }
  
  return(anidf)
}

#'
#'Generate a histogram of the distribution of modeled elevation - measured altitude
#'
#'@param datapts GPS data with measured Altitude and computed Elevation data
#'@return histogram of the distribution of modeled elevation - measured altitude
#'@examples 
#' library(dplyr)
#' xelev <- lookup_elevation(demo, zoom = 10)
#' histogram_animal_elevation(xelev)
#' @export
histogram_animal_elevation <- function(datapts) {
 require(ggplot2)
  histogram <- ggplot(datapts, aes(x = Elevation - Altitude)) +
    xlim(-100,100)+
    geom_histogram(aes(y=..density..), colour="blue", fill="lightblue", binwidth = 2 )+
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept = mean((Elevation-Altitude)[abs(Elevation-Altitude) <= 100])),col='blue',size=2)+
    labs(title = "Distribution of Modeled Elevation - Measured Altitude (meters)")+
    theme_minimal()
  return(histogram)
}


#'
#'Export modeled elevation data from existing animal data file
#'
#'@param rds_path animal tracking data file to model elevation from
#'@param out_path exported file path
#'@param zoom level of zoom, defaults to 12
#'@return list of data frames with gps data augmented by elevation
#'@export
#'
process_elevation <- function(rds_path, out_path, zoom = 12 ) {
  anidata <- readRDS(rds_path)
  
  for ( i in 1:length(anidata) ){
    print(noquote(paste("processing elevation data for file", i, "of", length(anidata))))
    anidata[[i]]<- lookup_elevation(anidata[[i]], ...)
    
  }
  saveRDS(anidata, out_path)
  return(anidata)
}

