calculate_slope <- function(data){
  
}


#############
# From raster package
#   https://www.rdocumentation.org/packages/raster/versions/2.8-19/topics/terrain

require(raster)
elevation <- getData('alt', country='USA')
x <- terrain(elevation[[1]], opt=c('slope', 'aspect'), unit='degrees')
plot(x)

cows <- readRDS("R/scratch/cow_data.rds")[[1]]


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

xx <- lookup_elevation(cows, get_aspect=TRUE, get_slope=FALSE)
