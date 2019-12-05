setwd("R/scratch/columbus_gps")

sample_data_file <- "30192350.TXT"

read_gps <- function(data_file){
  
  # get first line of data to determine data format
  data_row1 <- readLines(data_file, 1)
  
  # determine data format
  
  data_type <- ifelse( grepl("^\\$GPRMC", data_row1), "columbus_gps", "i_got_u")
  
  if(data_type == "columbus_gps"){
    gps_data <- read_columbus( data_file )
  }
  
}

read_columbus <- function(data_file){
  
  require(dplyr)
  
  gps_raw <- readLines(data_file)
  
  # parse nmea records, two lines at a time
  nmea_rmc <- grepl("^\\$GPRMC", gps_raw)
  nmea_gga <- grepl("^\\$GPGGA", gps_raw)
  
  #RMC via specs https://www.gpsinformation.org/dale/nmea.htm#RMC
  gps_rmc <- read.table(text = gps_raw[nmea_rmc], sep = ",", fill = TRUE, as.is = TRUE)
  
  names(gps_rmc) <- c("rmc_record", "time_utc", "status", 
                      "lat", "lat_dir","lon", "lon_dir", 
                      "ground_speed", "track_angle","date_MMDDYY", 
                      "mag_var", "mag_var_dir", "checksum_rmc" )
  
  #GGA via specs at https://www.gpsinformation.org/dale/nmea.htm#GGA
  gps_gga <- read.table(text = gps_raw[nmea_gga], sep = ",", fill = TRUE, as.is = TRUE)
  
  names(gps_gga) <- c("gga_record", "fix_time_utc", 
                      "fix_lat", "fix_lat_dir", "fix_lon", "fix_lon_dir",
                      "fix_qual", "n_sat", "h_dilution", "alt", "alt_M", "height", "height_M",
                      "dgps_update", "checksum_gga")
  
  gps_merged <- bind_cols(gps_rmc, gps_gga)
  
  
  
  # custom function to extract lat/lon data, convert to decimal
  deg_to_dec <- function(x, dir){
    xparts <-  strsplit(as.character(x), "\\.")[[1]]
    deg <- as.numeric(substr(xparts[1], 1, nchar(xparts[1])-2))
    min <- as.numeric(substr(xparts[1], nchar(xparts[1])-1, nchar(xparts[1])))
    sec <- as.numeric(xparts[2])
    
    ifelse(dir %in% c("W", "S"), -1 , 1)*(deg + min/60 + sec/3600)
    
  }
  
  
  gps_cleaned <- gps_merged %>%
    # exclude unneeded information
    select(-c( rmc_record, checksum_rmc, gga_record, alt_M, height_M, dgps_update, checksum_gga) ) %>%

    mutate(
      date_time_char = paste(date_MMDDYY, time_utc),
      date_time_utc = as.POSIXct(date_time_char, format = "%d%m%y %H%M%S", tz="GMT"),
      status = forcats::fct_recode(status, Active ="A", Void="V"),
      lat = deg_to_dec(lat, lat_dir),
      lon = deg_to_dec(lon, lon_dir),
      lat_fix = deg_to_dec(fix_lat, fix_lat_dir),
      lon_fix = deg_to_dec(fix_lon, fix_lon_dir),
      mag_var = deg_to_dec(mag_var, mag_var_dir),
      fix_qual = forcats::fct_recode(as.character(fix_qual), 
                                     Invalid = '0', GPSFix = '1', DGPSFix = '2', PPSFix = '3',
                                     RealTimeKine = '4', FloatRTK = '5', EstDeadReck = '6', ManInpMode = '7', SimMode ='8'
                                     )

    ) %>% 
    select(
      date_time_utc, lat, lon, alt, n_sat, ground_speed, 
      track_angle, h_dilution, height, status, lat_fix, lon_fix, mag_var
    )
  
  return(list( merged = gps_merged, cleaned = gps_cleaned) )
  
}


xx <- read_gps(sample_data_file)$cleaned

write.csv(xx, "sample_processed_data.csv")
