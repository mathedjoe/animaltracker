if(getRversion() >= '2.5.1') {
  globalVariables(c('ggplot2', 'Altitude', '..density..', 'DateMMDDYY', 'Status',
                    'QualFix', 'LatDir', 'LonDir', 'LatitudeFix', 'LatDirFix',
                    'LongitudeFix', 'LonDirFix', 'MagVar', 'MagVarDir'))
}

#'
#'Add elevation data from terrain tiles to long/lat coordinates of animal gps data
#'
#'@param elev elevation data as raster
#'@param anidf animal tracking dataframe
#'@param zoom level of zoom, defaults to 11
#'@param get_slope logical, whether to compute slope (in degrees), defaults to true
#'@param get_aspect logical, whether to compute aspect (in degrees), defaults to true
#'@return original data frame, with terrain column(s) appended
#'@export
lookup_elevation_file <- function(elev, anidf, zoom = 11, get_slope = TRUE, get_aspect = TRUE) {
  
  # extract coordinates from the animal data
  locations <- anidf %>% dplyr::select(x = Longitude, y = Latitude)
  
  # add Elevation column to the animal data
  anidf$Elevation <- raster::extract(elev, locations)
  
  if(get_slope | get_aspect){
    elev_terr <- raster::terrain(elev, opt=c('slope', 'aspect'), unit='degrees')
  }
  
  if(get_slope){
    slope <- elev_terr$slope
    anidf$Slope <- round(raster::extract(slope, locations), 1)
  }
  
  if(get_aspect){
    aspect <- elev_terr$aspect
    anidf$Aspect <- round(raster::extract(aspect, locations), 1)
  }
  return(anidf)
}



#'Add elevation data from public AWS terrain tiles to long/lat coordinates of animal gps data
#'
#'@param anidf animal tracking dataframe
#'@param zoom level of zoom, defaults to 11
#'@param get_slope logical, whether to compute slope (in degrees), defaults to true
#'@param get_aspect logical, whether to compute aspect (in degrees), defaults to true
#'@return original data frame, with Elevation column appended
#'@export
lookup_elevation_aws <- function(anidf, zoom = 11, get_slope = TRUE, get_aspect = TRUE) {
  
  
  # extract coordinates from the animal data
  locations <- anidf %>% dplyr::select(x = Longitude, y = Latitude)
  
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff/"
  ll_geo <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  prj <- "+proj=longlat"
  
  message("computing tiles for the given locations and zoom level")
  elev_data <- locations %>% 
    dplyr::mutate(
      lat_rad = y * pi/180, # convert degrees latitude to radians
      n = 2^zoom, # zoom refers to a logarithm base 2 number of gridlines
      tilex = (x + 180)/360 * n, # identify longitude gridline
      tiley = (1 - log(tan(lat_rad) + (1/cos(lat_rad)))/pi)/2 * n, # identify latitude grid line
      tilex = floor(tilex),
      tiley = floor(tiley),
      elevation = NA,
      slope = NA,
      aspect = NA
    ) %>%
    dplyr::select(x, y, tilex, tiley, elevation, slope, aspect)
  
  tiles <- elev_data %>%
    dplyr::select(tilex, tiley) %>%
    dplyr::filter(!duplicated(.))
  
  
  message("projecting locations to a spatial data frame")
  locations <- sp::SpatialPointsDataFrame(sp::coordinates(locations), 
                                          proj4string = sp::CRS(prj), 
                                          data =  data.frame(elevation = vector("numeric", nrow(locations)))  )
  
  ## DOWNLOAD TILES, EXTRACT ELEVATIONS
  message(paste("Downloading DEMs via", nrow(tiles), "tiles at Zoom =", zoom) )
  progbar <- progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = nrow(tiles), width = 60)
  
  for (i in 1:nrow(tiles)){
    progbar$tick()
    # Download this tile
    tmpfile <- tempfile()
    url <- paste0(base_url, zoom, "/", tiles$tilex[i], "/", tiles$tiley[i], ".tif")
    
    resp <- httr::GET(url, httr::write_disk(tmpfile, overwrite = TRUE))
    if (httr::http_type(resp) != "image/tiff") {
      stop("API did not return tif", call. = FALSE)
    }
    
    tile_this <- raster::raster(tmpfile)
    raster::projection(tile_this) <- web_merc
    tile_this <- raster::projectRaster(tile_this, crs = sp::CRS(prj) )
    
    ## update elevation for data in this tile
    data_isthis <- (elev_data$tilex == tiles$tilex[i]) & (elev_data$tiley == tiles$tiley[i])
    locations_this <- elev_data[data_isthis, c("x","y")]
    
    elev_data$elevation[data_isthis] <- raster::extract(tile_this, locations_this)
    
    # compute slope and aspect if requested
    if(get_slope | get_aspect){
      elev_terr <- raster::terrain( tile_this, opt=c('slope', 'aspect'), unit='degrees')
    }
    
    if(get_slope){
      elev_data$slope[data_isthis] <-  round(raster::extract(elev_terr$slope, locations_this), 1)
    }
    
    if(get_aspect){
      elev_data$aspect[data_isthis] <-  round(raster::extract(elev_terr$aspect, locations_this), 1)
      
    }
    
  }
  
  # add Elevation column to the animal data
  anidf$Elevation <- elev_data$elevation
  
  if(get_slope){
    anidf$Slope <- elev_data$slope
  }
  
  if(get_aspect){
    anidf$Aspect <- elev_data$aspect
  }

  return(anidf)
}




#'
#'Read an archive of altitude mask files and convert the first file into a raster object
#'
#'@param filename path of altitude mask file archive
#'@param exdir path to extract files 
#'@return the first altitude mask file as a raster object
#'@export 
read_zip_to_rasters <- function(filename, exdir = "inst/extdata/elev"){
  
  ff <- utils::unzip(filename, exdir=dirname(exdir))  
  f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.grd']

  rs <- raster::raster(f[[1]])
  
  raster::projection(rs) <- "+proj=longlat +datum=WGS84"
  
  return(rs)
  
}

#'
#'Read and process a Columbus P-1 data file containing NMEA records into a data frame
#'
#'@param filename path of Columbus P-1 data file
#'@return NMEA records in RMC and GGA formats as a data frame
#'@export
#'@examples
#'
#'read_columbus(system.file("extdata", "demo_columbus.TXT", package = "animaltracker"))
read_columbus <- function(filename){
  
  gps_raw <- readLines(filename)
  
  # parse nmea records, two lines at a time
  nmea_rmc <- grepl("^\\$GPRMC", gps_raw)
  nmea_gga <- grepl("^\\$GPGGA", gps_raw)
  
  #RMC via specs https://www.gpsinformation.org/dale/nmea.htm#RMC
  gps_rmc <- utils::read.table(text = gps_raw[nmea_rmc], sep = ",", fill = TRUE, as.is = TRUE)
  
  names(gps_rmc) <- c("RMCRecord", "Time", "Status", 
                      "Latitude", "LatDir","Longitude", "LonDir", 
                      "GroundSpeed", "TrackAngle","DateMMDDYY", 
                      "MagVar", "MagVarDir", "ChecksumRMC")
  
  #GGA via specs at https://www.gpsinformation.org/dale/nmea.htm#GGA
  gps_gga <- utils::read.table(text = gps_raw[nmea_gga], sep = ",", fill = TRUE, as.is = TRUE)
  
  names(gps_gga) <- c("GGARecord", "TimeFix", 
                      "LatitudeFix", "LatDirFix", "LongitudeFix", "LonDirFix",
                      "QualFix", "nSatellites", "hDilution", "Altitude", "AltitudeM", "Height", "HeightM",
                      "DGPSUpdate", "ChecksumGGA")
  

  df <- bind_cols(gps_rmc, gps_gga) %>% 
    dplyr::mutate(
      DateTimeChar = paste(DateMMDDYY, Time),
      Status = suppressWarnings(forcats::fct_recode(Status, Active ="A", Void="V")),
      QualFix = suppressWarnings(forcats::fct_recode(as.character(QualFix), 
                                    Invalid = '0', GPSFix = '1', DGPSFix = '2', PPSFix = '3',
                                    RealTimeKine = '4', FloatRTK = '5', EstDeadReck = '6', ManInpMode = '7', SimMode ='8'
      )) 
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      Latitude = deg_to_dec(Latitude, LatDir),
      Longitude = deg_to_dec(Longitude, LonDir),
      LatitudeFix = deg_to_dec(LatitudeFix, LatDirFix),
      LongitudeFix = deg_to_dec(LongitudeFix, LonDirFix),
      MagVar = deg_to_dec(MagVar, MagVarDir)
    ) %>% 
    dplyr::ungroup()
  return(df)
}



#'
#'Helper function for cleaning Columbus P-1 datasets.
#'Given lat or long coords in degrees and a direction, convert to decimal. 
#'
#'@param x lat or long coords in degrees
#'@param direction direction of lat/long
#'@return converted x
#'
deg_to_dec <- function(x, direction){
  xparts <- strsplit(as.character(x), "\\.")[[1]]
  deg <- as.numeric(substr(xparts[1], 1, nchar(xparts[1])-2))
  min <- as.numeric(substr(xparts[1], nchar(xparts[1])-1, nchar(xparts[1])))
  sec <- as.numeric(xparts[2])
    
  return(ifelse(direction %in% c("W", "S"), -1 , 1)*(deg + min/60 + sec/3600))
}

#'
#'Helper function for cleaning Columbus P-1 datasets.
#'Given lat and long coords in degree decimal, convert to radians and compute bearing.
#'
#'@param lat1 latitude of starting point
#'@param lon1 longitude of starting point
#'@param lat2 latitude of ending point
#'@param lon2 longitude of ending point
#'@return bearing computed from given coordinates
#'
calc_bearing <- function(lat1, lon1, lat2, lon2){
  lat1 <- lat1*(pi/180)
  lon1 <- lon1*(pi/180)
  lat2 <- lat2*(pi/180)
  lon2 <- lon2*(pi/180)
  
  bearing_radian <- atan2( sin(lon2-lon1)*cos(lat2) , cos(lat1)*sin(lat2) - sin(lat1)*cos(lat2)*cos(lon2-lon1) )
  
  return((bearing_radian * 180/pi +360 )%% 360)
}

#'
#'Reads a GPS dataset of unknown format at location filename 
#'
#'@param filename location of the GPS dataset
#'@return list containing the dataset as a df and the format
#'
read_gps <- function(filename){
  
  # get first line of data to determine data format
  data_row1 <- readLines(filename, 1, skipNul = TRUE)
  
  # determine data format
  
  data_type <- ifelse( grepl("^\\$GPRMC", data_row1), "columbus", "igotu")
  
  if(data_type == "columbus"){
    gps_data <- read_columbus(filename)
  }
  else {
    gps_data <- read.csv(filename, skipNul = TRUE, stringsAsFactors = FALSE)
  }
  
  return(list(df = gps_data, dtype = data_type))
}

#'
#'Generate a histogram of the distribution of modeled elevation - measured altitude
#'
#'@param datapts GPS data with measured Altitude and computed Elevation data
#'@return histogram of the distribution of modeled elevation - measured altitude
#'@examples
#'# Histogram of elevation - altitude for the demo data
#'
#'histogram_animal_elevation(demo)
#'@export
histogram_animal_elevation <- function(datapts) {
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
#'Process and optionally export modeled elevation data from existing animal data file
#'
#'@param zoom level of zoom, defaults to 11
#'@param get_slope logical, whether to compute slope (in degrees), defaults to True
#'@param get_aspect logical, whether to compute aspect (in degrees), defaults to True
#'@param in_path animal tracking data file to model elevation from
#'@param export logical, whether to export data with elevation, defaults to False
#'@param out_path .rds file path for processed data when export is True
#'@return list of data frames with gps data augmented by elevation
#'@export
#'
process_elevation <- function(zoom = 11, get_slope=TRUE, get_aspect=TRUE, in_path, export = FALSE, out_path = NULL) {
  anidata <- readRDS(in_path)
  
  for ( i in 1:length(anidata) ){
    message(noquote(paste("processing elevation data for file", i, "of", length(anidata))))
    anidata[[i]]<- lookup_elevation_aws(anidata[[i]], get_slope, get_aspect)
    
  }
  if(export & !is.null(out_path)) {
    saveRDS(anidata, out_path)
  }
  return(anidata)
}
