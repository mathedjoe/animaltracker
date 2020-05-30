# replace progress bar when downloading DEMs
library(animaltracker)
require(dplyr)
require(httr)
require(sp)
require(raster)

install.packages("pryr")
require(pryr)

# Custom aws tile fetcher
custom_get_aws_elevation <- function (locations, z, prj, ...) {
  
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff/"
  ll_geo <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  
  message("computing tiles for the given locations and zoom level")
  data_tilexy <- locations %>% 
    dplyr::mutate(
      lat_rad = y * pi/180, # convert degrees latitude to radians
      n = 2^z, # zoom is actually logarithm base 2
      tilex = (x + 180)/360 * n, # identify longitude gridline
      tiley = (1 - log(tan(lat_rad) + (1/cos(lat_rad)))/pi)/2 * n, # identify latitude grid line
      tilex = floor(tilex),
      tiley = floor(tiley),
      elevation = NA
    ) %>%
    dplyr::select(x, y, tilex, tiley, elevation)
  
  tiles <- data_tilexy %>%
    dplyr::select(tilex, tiley) %>%
    dplyr::filter(!duplicated(.))
  
  message("projecting locations to a spatial data frame")
  locations <- sp::SpatialPointsDataFrame(sp::coordinates(locations), 
                          proj4string = sp::CRS(prj), 
                          data =  data.frame(elevation = vector("numeric", nrow(locations)))  )
  
  ## DOWNLOAD TILES, EXTRACT ELEVATIONS
  message(paste("Downloading DEMs via", nrow(tiles), "tiles at Zoom =", z) )
  progbar <- txtProgressBar(min = 0, max = nrow(tiles), initial = 0, width = 60, style=3) 
  
  for (i in 1:nrow(tiles)){
    setTxtProgressBar(progbar,i)
    # Download this tile
    tmpfile <- tempfile()
    url <- paste0(base_url, z, "/", tiles$tilex[i], "/", tiles$tiley[i], ".tif")

    resp <- httr::GET(url, httr::write_disk(tmpfile, overwrite = TRUE), ...)
    if (httr::http_type(resp) != "image/tiff") {
      stop("API did not return tif", call. = FALSE)
    }
    
    tile_this <- raster::raster(tmpfile)
    raster::projection(tile_this) <- web_merc
    tile_this <- raster::projectRaster(tile_this, crs = CRS(prj) )
    
    ## update elevation for data in this tile
    data_isthis <- (data_tilexy$tilex == tiles$tilex[i]) & (data_tilexy$tiley == tiles$tiley[i])
    data_tilexy$elevation[data_isthis] <- raster::extract(tile_this, data_tilexy[data_isthis, c("x","y")])
  }
  return(data_tilexy %>% dplyr::select(x,y,elevation))
  
}



df <- demo_unfiltered %>% 
  dplyr::select(x = Longitude, y = Latitude)
object_size(df)
system.time({
  test_data <- custom_get_aws_elevation(df, prj = "+proj=longlat", z=12)
})

df_big <- data.frame(x = runif(10^6, -120, -110), y = runif(10^6, 40, 45))
object_size(df_big)

system.time({
  test_data_big6 <- custom_get_aws_elevation(df_big, prj = "+proj=longlat", z=6)
})

system.time({
  test_data_big9 <- custom_get_aws_elevation(df_big, prj = "+proj=longlat", z=9)
})
### quite reasonable

system.time({
  test_data_big12 <- custom_get_aws_elevation(df_big, prj = "+proj=longlat", z=12)
})
## nope!!