if(getRversion() >= '2.5.1') {
  globalVariables(c('ggplot2', 'Altitude', '..density..'))
}

#'
#'Add elevation data from terrain tiles to long/lat coordinates of animal gps data
#'
#'@param elev elevation data as terrain tiles
#'@param anidf animal tracking dataframe
#'@param zoom level of zoom, defaults to 11
#'@param get_slope logical, whether to compute slope (in degrees), defaults to true
#'@param get_aspect logical, whether to compute aspect (in degrees), defaults to true
#'@return original data frame, with terrain column(s) appended
#'@examples
#'# Add elevation data to demo data frame
#'\donttest{
#'\dontrun{
#'## Get elevation
#'elev <- read_zip_to_rasters(system.file("extdata", "elev/USA_msk_alt.zip", package="animaltracker"))
#'
#'## Lookup with slope and aspect
#'lookup_elevation(elev, read.csv(system.file("extdata", 
#'"demo_aug19/Bannock_2017_101_1149.csv", package = "animaltracker"), skipNul = TRUE), 
#'zoom = 11, get_slope = TRUE, get_aspect = TRUE)
#'}
#'}
#'@export
lookup_elevation <- function(elev, anidf, zoom = 11, get_slope = TRUE, get_aspect = TRUE) {
  
  # extract coordinates from the animal data
  locations <- anidf %>% dplyr::select(x = Longitude, y = Latitude)
  
  # convert terrain data to spatial pts
  elevpts <- raster::rasterToPoints(elev, spatial=TRUE)
  
  # determine nearest neighbors in the terrain data for the animal locations
  datapts_elev <- nabor::knn(data = sp::coordinates(elevpts), query = locations, k=1)
  
  # add Elevation and Slope columns to the animal data
  anidf$Elevation <- round(elevpts$USA1_msk_alt[ datapts_elev$nn.idx ], 1)
  
  if(get_slope | get_aspect){
    elev_terr <- raster::terrain(elev, opt=c('slope', 'aspect'), unit='degrees')
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



#'Add elevation data from public AWS terrain tiles to long/lat coordinates of animal gps data
#'
#'@param anidf animal tracking dataframe
#'@param zoom level of zoom, defaults to 12
#'@param get_slope logical, whether to compute slope (in degrees), defaults to true
#'@param get_aspect logical, whether to compute aspect (in degrees), defaults to true
#'@return original data frame, with Elevation column appended
#'@export
lookup_elevation_aws <- function(anidf, zoom = 12, get_slope = TRUE, get_aspect = TRUE) {
  
  
  # extract coordinates from the animal data
  locations <- anidf %>% dplyr::select(x = Longitude, y = Latitude)
  
  # retrieve terrain data for the region containing the animal data
  ## DEM source = Amazon Web Services (https://aws.amazon.com/public-datasets/terrain/) terrain tiles.
  elev <- elevatr::get_elev_raster(locations, prj = "+proj=longlat", z=zoom)
  
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
#'
read_columbus <- function(filename){
  
  gps_raw <- readLines(filename)
  
  # parse nmea records, two lines at a time
  nmea_rmc <- grepl("^\\$GPRMC", gps_raw)
  nmea_gga <- grepl("^\\$GPGGA", gps_raw)
  
  #RMC via specs https://www.gpsinformation.org/dale/nmea.htm#RMC
  gps_rmc <- read.table(text = gps_raw[nmea_rmc], sep = ",", fill = TRUE, as.is = TRUE)
  
  names(gps_rmc) <- c("RMCRecord", "Time", "Status", 
                      "Latitude", "LatDir","Longitude", "LonDir", 
                      "GroundSpeed", "TrackAngle","DateMMDDYY", 
                      "MagVar", "MagVarDir", "ChecksumRMC")
  
  #GGA via specs at https://www.gpsinformation.org/dale/nmea.htm#GGA
  gps_gga <- read.table(text = gps_raw[nmea_gga], sep = ",", fill = TRUE, as.is = TRUE)
  
  names(gps_gga) <- c("GGARecord", "TimeFix", 
                      "LatitudeFix", "LatDirFix", "LongitudeFix", "LonDirFix",
                      "QualFix", "nSatellites", "hDilution", "Altitude", "AltitudeM", "Height", "HeightM",
                      "DGPSUpdate", "ChecksumGGA")
  

  df <- bind_cols(gps_rmc, gps_gga) %>% 
    dplyr::mutate(
      DateTimeChar = paste(DateMMDDYY, Time),
      Status = forcats::fct_recode(Status, Active ="A", Void="V"),
      QualFix = forcats::fct_recode(as.character(QualFix), 
                                    Invalid = '0', GPSFix = '1', DGPSFix = '2', PPSFix = '3',
                                    RealTimeKine = '4', FloatRTK = '5', EstDeadReck = '6', ManInpMode = '7', SimMode ='8'
      ) 
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
#'Given lat/long coords in degrees and a direction, convert to decimal. 
#'
#'@param x lat/long coords in degrees
#'@param direction direction of lat/long
#'@return converted x
#'
deg_to_dec <- function(x, direction){
  xparts <- strsplit(as.character(x), "\\.")[[1]]
  deg <- as.numeric(substr(xparts[1], 1, nchar(xparts[1])-2))
  min <- as.numeric(substr(xparts[1], nchar(xparts[1])-1, nchar(xparts[1])))
  sec <- as.numeric(xparts[2])
    
    # TO DO: VECTORIZE THIS
  return(ifelse(direction %in% c("W", "S"), -1 , 1)*(deg + min/60 + sec/3600))
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
#'Export modeled elevation data from existing animal data file
#'
#'@param elev elevation data as terrain tiles
#'@param zoom level of zoom, defaults to 11
#'@param get_slope logical, whether to compute slope (in degrees), defaults to true
#'@param get_aspect logical, whether to compute aspect (in degrees), defaults to true
#'@param in_path animal tracking data file to model elevation from
#'@param out_path exported file path, .rds
#'@return list of data frames with gps data augmented by elevation
#'@examples
#'# Export elevation data from demo .rds datasets
#'\donttest{
#'\dontrun{
#'## Get elevation
#'elev <- read_zip_to_rasters(system.file("extdata", "elev/USA_msk_alt.zip", package="animaltracker"))
#'
#'## Process and export
#'process_elevation(elev, zoom = 11, get_slope = TRUE, get_aspect = TRUE, 
#'in_path = system.file("extdata", "demo_aug19.rds", 
#'package = "animaltracker"), out_path = "demo_aug19_elev.rds")
#'
#'}
#'}
#'@export
#'
process_elevation <- function(elev, zoom = 11, get_slope=TRUE, get_aspect=TRUE, in_path, out_path) {
  anidata <- readRDS(in_path)
  
  for ( i in 1:length(anidata) ){
    print(noquote(paste("processing elevation data for file", i, "of", length(anidata))))
    anidata[[i]]<- lookup_elevation(elev, anidata[[i]], get_slope, get_aspect)
    
  }
  saveRDS(anidata, out_path)
  return(anidata)
}

