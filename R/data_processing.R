if(getRversion() >= '2.5.1') {
  globalVariables(c('ggplot2', 'Altitude', '..density..', 'DateMMDDYY', 'Status',
                    'QualFix', 'LatDir', 'LonDir', 'LatitudeFix', 'LatDirFix',
                    'LongitudeFix', 'LonDirFix', 'MagVar', 'MagVarDir', 'y', 'x',
                    'lat_rad', 'tilex', 'tiley', 'elevation', 'slope', 'aspect', '.',
                    'index', 'distance', 'water_object', 'min_dist'))
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
  
  # make a container for computed elevation data
  df_out <- anidf
  
  # extract coordinates from the animal data
  locations <- anidf %>% dplyr::select(x = Longitude, y = Latitude)
  
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff/"
  ll_geo <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  prj <- "+proj=longlat"
  
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
  
  locations <- sp::SpatialPointsDataFrame(sp::coordinates(locations), 
                                          proj4string = sp::CRS(prj), 
                                          data =  data.frame(elevation = vector("numeric", nrow(locations)))  )
  
  ## DOWNLOAD TILES, EXTRACT ELEVATIONS

  message(paste("Downloading DEMs via", nrow(tiles), "tiles at Zoom =", zoom) )

  withProgress( message = paste("Downloading & Processing DEMs, Zoom =", zoom), 
                value = 0, min = 0, max = nrow(tiles), {

    
    for (i in 1:nrow(tiles)){
      setProgress(i, detail = paste0(i,"/",nrow(tiles), " tiles processed"))
      
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
    
  })# end progress wrapper
  
  # add Elevation column to the animal data
  df_out$Elevation <- elev_data$elevation
  
  if(get_slope){
    df_out$Slope <- elev_data$slope
  }
  
  if(get_aspect){
    df_out$Aspect <- elev_data$aspect
  }

  return(df_out)
}

#'Add weather data to animal data from NOAA's Integrated Surface Database (ISD)
#'
#'@param anidf animal data frame cleaned by clean_location_data
#'@param selected_vars vector of desired weather variables, defaults to wind direction, wind speed, temperature, temperature dewpoint, and air pressure
#'@param search whether to search for closest stations
#'@param search_radius search radius to find closest weather station to lat/long in animal data, defaults to 100km
#'@param station weather station if search is FALSE
#'@param is_shiny whether this function is called from the shiny app, defaults to FALSE
#'@return original data frame, with selected weather variables appended
#'@export
#'
lookup_weather <- function(anidf, selected_vars = c("wind_direction", "wind_speed", "temperature", "temperature_dewpoint", "air_pressure"), 
                           search = TRUE, search_radius = 100, station = NULL, is_shiny = FALSE) {
  dates <- list(min = min(anidf$Date), max = max(anidf$Date))
  
  station_closest <- data.frame()
  # given a location, find the nearest station(s)
  if(search) {
    station_options <- isd_stations_search(lat = median(anidf$Latitude, na.rm=TRUE), 
                                           lon = median(anidf$Longitude, na.rm=TRUE), 
                                           radius = search_radius ) %>% 
      mutate(begin = as.Date(as.character(begin), format = "%Y%m%d"),
             end = as.Date(as.character(end), format = "%Y%m%d")) %>%
      filter(dates$min > begin, dates$max < end)
    station_closest <- station_options %>% slice(1)
  }
  else {
    station_closest <- station
  }
  
  
  if(nrow(station_closest) == 0){
    message(paste("No weather stations found with a search radius of", search_radius, "km. 
                  Please try again with a wider radius."))
    return(anidf)
  }
  # given dates, find the weather data from the station(s)
  data_years <- lubridate::year(dates$min):lubridate::year(dates$max)
  
  weather_raw <- data.frame()
  if(is_shiny) {
    withProgress(message = "Querying weather data from NOAA ISD", value = 0, min = 0, max = length(data_years), {
      i <- 1
      for(year in data_years) {
        message(paste("Now querying weather data for", year))
        weather_raw <- weather_raw %>% 
          dplyr::bind_rows(rnoaa::isd(station_closest$usaf, station_closest$wban, year))
        setProgress(i, detail = paste0(i, "/", length(data_years), " years queried"))
        i <- i + 1
      }
    })
  }
  else {
    for(year in data_years) {
      message(paste("Now querying weather data for", year))
      weather_raw <- weather_raw %>% 
        dplyr::bind_rows(rnoaa::isd(station_closest$usaf, station_closest$wban, year))
    }
  }
  
  weather_df <- weather_raw %>% 
    select(raw_date = date,  raw_time = time, 
           wind_direction, wind_speed, 
           temperature, temperature_dewpoint, 
           air_pressure ) %>% 
    mutate(date =  as.Date(as.character(raw_date), format = "%Y%m%d"),
           datetime = as.POSIXct(paste(raw_date, raw_time), format = "%Y%m%d %H%M", tz = "UTC"),
           datehr = lubridate::round_date(datetime, unit = "hour")
    ) %>% 
    mutate_at(vars(wind_direction, wind_speed, temperature, temperature_dewpoint, air_pressure), 
              function(x){ # convert strings to numeric format, remove NAs (indicated by 9999)
                xdata <- x
                xdata[xdata %in% c("999", "9999", "99999", "+9999")] <- NA
                as.numeric(xdata)
              }) %>%
    mutate_at(vars(temperature, temperature_dewpoint, air_pressure), function(x) x/10 ) %>%
    filter(datetime >= min(lubridate::round_date(anidf$DateTime-lubridate::hours(12), unit="hour"), na.rm=TRUE), 
           datetime <= max(lubridate::round_date(anidf$DateTime+lubridate::hours(12), unit="hour"), na.rm=TRUE),
           !is.na(datehr),
           !duplicated(datehr) # note: might be better to group_by(datehr) and aggregate/average
    )
  
  # build a time series of the weather data (hourly)
  
  date_time_seq <- seq.POSIXt(min(weather_df$datehr), 
                              max(weather_df$datehr), by = 'hour')
  
  
  ## create time series object with 3 columns: DateTime, Longitude, Latitude
  weather_ts <- data.frame(datehr = date_time_seq) %>% 
    dplyr::left_join( weather_df, by = "datehr") 
  
  # round the animal data to the nearest hour
  # left_join weather ts to the animal data
  
  anidf_aug <- anidf %>% 
    dplyr::mutate(datehr = lubridate::round_date(DateTime, unit= "hour")) %>%
    dplyr::left_join(weather_ts)
  
  return(anidf_aug)
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
#'@noRd
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
#'@noRd
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
#'@noRd
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

#'
#'Convert kml coordinates to sf for mapping and calculations
#'
#'@param kmz_element kmz object containing coordinates
#'@param shift optional length 2 vector of coordinates to shift kml by
#'@export
# 
kmz_to_sf <- function(kmz_element, shift = c(0,0)){
  if(!is.matrix(kmz_element)) {
    ## it's one point
    return( sf::st_point(kmz_element, dim = "XY") + shift )
  }
  
  # it's a list of points
  if(kmz_element[1, 1] == kmz_element[nrow(kmz_element), 1] &
     kmz_element[1, 2] == kmz_element[nrow(kmz_element), 2]){
    ## it's a polygon
    return(sf::st_polygon(list(as.matrix(kmz_element))) + shift )
    
  }
  else {
    ## it's a polygonal line
    return(sf::st_linestring(kmz_element, dim = "XY") + shift)
  }
}

#'
#'Find distance between n points and water objects
#'
#'@param points number of points to generate, or data.frame with lat/lon coordinates
#'@param xlim latitude bounds if points is a number
#'@param ylim longitude bounds if points is a number
#'@param water list of sf geoms representing water locations
#'@return data.frame containing coordinates and their corresponding closest water sources
#'@export
#'
dist_points_to_water <- function(points, xlim = c(0,25), ylim = c(0,25), water){
  
  if (length(points) == 1){
    points_data <- data.frame( lat = stats::runif(points, xlim[1], xlim[2]),
                               lon = stats::runif(points, ylim[1], ylim[2]))
    
  }
  else{
    points_data <- points
  }
  
  # convert point data to sf
  pts <- sf::st_as_sf(points_data, coords = c("lon", "lat"))
  
  # compute pairwise distance from each point to each water feature
  if( length(water) * nrow(points_data) > 10^6) {
    print(paste0("Warning: ", length(water) * nrow(points_data),
                 " distances to calculate, this may take a long time.")
    )
  }
  # pairwise distances
  dist_to_water <- sf::st_distance(pts, water)
  
  # find closest distance to water from pairwise distances
  dist_to_water <- bind_cols(points_data, as.data.frame(dist_to_water)) %>%
    dplyr::mutate(index = 1:n()) %>%
    tidyr::pivot_longer( contains('V'), 'water_object', 'dist', values_to = "distance") %>%
    dplyr::group_by(index) %>%
    dplyr::mutate(min_dist = min(distance),
           closest_water = water_object[distance == min_dist]) %>%
    tidyr::pivot_wider( names_from = water_object, values_from = distance, names_prefix = "dist_") %>%
    dplyr::ungroup() %>% dplyr::select(-index)
  
  return(dist_to_water)
}