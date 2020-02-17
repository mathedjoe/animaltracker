
if(getRversion() >= '2.5.1') {
  globalVariables(c('dplyr', 'tibble', 'forecast',
                    'Time', 'Altitude', 'Distance', 'TimeDiff', 'Course',
                    'CourseDiff', 'DistGeo', 'RateFlag', 'CourseFlag', 'DistanceFlag',
                    'TotalFlags', 'TimeFlag', 'DuplicateDateFlag', 'RMCRecord',
                    'ChecksumRMC', 'GGARecord', 'AltitudeM', 'HeightM', 'DGPSUpdate',
                    'ChecksumGGA', 'DateTimeChar', 'nSatellites', 'GroundSpeed',
                    'TrackAngle', 'hDilution', 'Height', 'Status', 'LatitudeFix',
                    'LongitudeFix', 'MagVar', 'Satelite'))
}

#'
#'Generate metadata for a directory of animal data files
#'
#'@param data_dir directory of animal data files
#'@return list of data info as a list of animal IDs and GPS units
#'@examples
#'# Get metadata for demo directory
#'
#'get_file_meta(system.file("extdata", "demo_nov19", package = "animaltracker"))
#'@export
#'
get_file_meta <- function(data_dir){
  file_names <- list.files(data_dir, pattern = "*.(csv|txt|TXT)", recursive = TRUE, full.names = TRUE)
  
  gps_units <- gsub("(.*)(20)([0-9]{2}\\_)(.*)(\\_{1}.*)(\\.(csv|txt|TXT))","\\4", file_names)
  
  ani_ids <- gsub("(.*)(20)([0-9]{2}\\_)(.*\\_)(.*)(\\.(csv|txt|TXT))","\\5", file_names)
  
  # assign random ids to missing animal ids
  ani_ids_na <- ani_ids == "anixxxx"
  ani_ids[ani_ids_na] <- sample(1000:9999, size = sum(ani_ids_na), replace = FALSE)
  ani_ids[ani_ids_na] <- paste0("R", ani_ids[ani_ids_na])
  
  return(list(ani = ani_ids, gps = gps_units))
}

#'
#'Cleans a raw animal GPS dataset, implementing a standardized procedure to remove impossible values
#'
#'@param df data frame in standardized format (e.g., from a raw spreadsheet)
#'@param dtype data type, iGotU or Columbus P-1
#'@param filters filter bad data points, defaults to true
#'@param aniid identification code for the animal
#'@param gpsid identification code for the GPS device
#'@param maxrate maximum rate of travel (meters/minute) between consecutive points
#'@param maxcourse maximum distance (meters) between consecutive points
#'@param maxdist maximum geographic distance (meters) between consecutive points
#'@param maxtime maximum time (minutes) between consecutive points 
#'@param tz_in input time zone, defaults to UTC
#'@param tz_out output time zone, defaults to UTC
#'@return df of clean animal GPS data
#'@examples
#'# Clean a data frame from csv
#'
#'## Read igotU data
#'bannock_df <- read.csv(system.file("extdata", "demo_nov19/Bannock_2017_101_1149.csv", 
#'package = "animaltracker"), skipNul=TRUE)
#'
#'## Clean and filter
#'clean_location_data(bannock_df, dtype = "igotu", filters = TRUE, aniid = 1149, 
#'gpsid = 101, maxrate = 84, maxdist = 840, maxtime = 100)
#'
#'## Clean without filtering
#'clean_location_data(bannock_df, dtype = "igotu", filters = FALSE, aniid = 1149, 
#'gpsid = 101, maxrate = 84, maxdist = 840, maxtime = 100)
#'
#'# Clean a data frame from txt
#'
#'## Read Columbus P-1 data
#'columbus_df <- read_columbus(system.file("extdata", "demo_columbus.TXT", 
#'package = "animaltracker"))
#'
#'## Clean and filter
#'clean_location_data(columbus_df, dtype = "columbus", filters = TRUE, aniid = 1149, 
#'gpsid = 101, maxrate = 84, maxdist = 840, maxtime = 100)
#'@export
#'
clean_location_data <- function(df, dtype, filters = TRUE, 
                                aniid = NA, gpsid = NA, 
                                maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=100, tz_in = "UTC", tz_out = "UTC"){
  # make sure quantitative columns are read in properly
  df <- df %>% 
    dplyr::mutate(Latitude = as.numeric(Latitude),
                  Longitude = as.numeric(Longitude))
  if(dtype == "columbus") {
    df <- df %>%
      # exclude unneeded information
      dplyr::select(-c( RMCRecord, ChecksumRMC, GGARecord, AltitudeM, HeightM, DGPSUpdate, ChecksumGGA) ) %>%
      dplyr::mutate( 
        DateTime = lubridate::with_tz(as.POSIXct(DateTimeChar, format = "%d%m%y %H%M%OS", tz = tz_in), tz = tz_out),
        Date = NA,
        Time = strftime(DateTime, format="%H:%M:%OS", tz=tz_out),
        Course = calc_bearing(dplyr::lag(Latitude, 1, default = first(Latitude)), dplyr::lag(Longitude, 1, default = first(Longitude)), Latitude, Longitude),
        Distance = geosphere::distGeo(cbind(Longitude, Latitude), 
                                      cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) )))
      ) %>%
      dplyr::select(
        Date, Time, DateTime, Latitude, Longitude, Altitude, nSatellites, GroundSpeed, 
        TrackAngle, hDilution, Height, Status, LatitudeFix, LongitudeFix, MagVar, Course, Distance
      ) 
  }
  if(dtype == "igotu") {
    # avoid re-creating columns if a dataset is cleaned multiple times
    if(!("Order" %in% colnames(df))) {
      df <- df %>% tibble::add_column(Order = df$Index, .before = "Index")
    }
    if(!("Rate" %in% colnames(df))) {
      df <- df %>% tibble::add_column(Rate = NA, .after = "Distance")
    }
    if(!("CourseDiff" %in% colnames(df))) {
      df <- df %>% tibble::add_column(CourseDiff = NA, .after = "Course")
    }
    df <- df %>%
      dplyr::mutate(
        nSatellites = nchar(as.character(Satelite)) - nchar(gsub("X", "", as.character(Satelite))),
        DateTime = lubridate::with_tz(lubridate::ymd_hms(paste(Date, Time), tz=tz_in, quiet = TRUE), tz=tz_out),
        Time = strftime(DateTime, format="%H:%M:%S", tz=tz_out), # reclassify Date as a Date variable
        Distance = as.numeric(Distance),
        Course = as.numeric(Course)
      )
  }
  
  if(!("TimeDiff" %in% colnames(df))) {
    df <- df %>% tibble::add_column(TimeDiff = NA, .after = "DateTime")
  }
  if(!("TimeDiffMins" %in% colnames(df))) {
    df <- df %>% tibble::add_column(TimeDiffMins = NA, .after = "TimeDiff")
  }
  
  df <- df %>% 
    dplyr::mutate(
      GPS = gpsid,
      Animal = aniid,
      Animal = as.factor(Animal),
      Date = strftime(DateTime, format="%Y-%m-%d", tz=tz_out)# reclassify Date as a Date variable
    )
  
  if(filters) {
    df <- df %>% 
      dplyr::filter(!is.na(DateTime), !is.na(Date), !is.na(Time), nSatellites > 0) %>% 
      dplyr::distinct(DateTime, .keep_all = TRUE) # remove duplicate timestamps
  }

  df <- df %>% 
    dplyr::mutate(
      TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > maxtime), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), # compute sequential time differences (in seconds)
      TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))),
      Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0), # compute rate of travel (meters/min), default to 0 to prevent divide by 0 error
      CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                     cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) ))), #compute geodesic distance between points
      RateFlag = 1*(Rate > maxrate), # flag any data points representing too fast travel
      CourseFlag = 1*(CourseDiff >= maxcourse),
      DistanceFlag = 1*(DistGeo >= maxdist)# compute sequential time differences (in mins)
    ) 
  
    if(filters) {
      df <- df %>%
        dplyr::mutate(TotalFlags = RateFlag + CourseFlag + DistanceFlag ) %>%
        dplyr::filter(TotalFlags < 2,
                      !DistanceFlag ) %>%
        dplyr::mutate( # recalculate columns affected by filtering
          TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > maxtime), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), 
          TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))),
          Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0),
          CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
          DistGeo = geosphere::distGeo(cbind(Longitude, Latitude),
                                       cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude))))
        ) %>%
        dplyr::select(-contains("Flag")) # remove flags after use
      
      if(dtype == "columbus") {
        df <- df %>% 
          dplyr::mutate(Distance = DistGeo)
      }
    }
    else {
      df <- df %>% 
        dplyr::mutate(
          TimeFlag = 1*(is.na(DateTime) | is.na(Date) | is.na(Time))
        ) %>%
        tibble::add_column(DuplicateDateFlag = 1*duplicated(df$DateTime)) %>%
        dplyr::mutate(TotalFlags = RateFlag + CourseFlag + DistanceFlag + TimeFlag + DuplicateDateFlag)
    }
    
  return(as.data.frame(df))
}



#'
#'Cleans all animal GPS datasets (in .csv format) in a chosen directory. Optionally exports the clean data as spreadsheets, a single .rds data file, or as a list of data frames
#'
#'@param data_dir directory of GPS tracking files (in csv)
#'@param cleaned_filename full name of output file (ending in .rds), defaults to data/animal_data.rds
#'@param cleaned_dir directory to save the processed GPS datasets as spreadsheets (.csv), defaults to data/processed
#'@param tz_in input time zone, defaults to UTC
#'@param tz_out output time zone, defaults to UTC
#'@return list of cleaned animal GPS datasets
#'@examples
#'# Clean all animal GPS .csv datasets in the demo directory
#'\donttest{
#'\dontrun{
#'clean_export_files(system.file("extdata", "demo_nov19", package = "animaltracker"), 
#'cleaned_filename = "ex_animal_data.rds", cleaned_dir = "clean_export_ex", tz = "UTC")
#'}
#'}
#'@export
#'
clean_export_files <- function(data_dir, cleaned_filename = "animal_data.rds", cleaned_dir = "processed", tz_in = "UTC", tz_out = "UTC") {
  data_files <- list.files(data_dir, pattern = "*.(csv|txt|TXT)", recursive = TRUE, full.names = T)
  data_info <- get_file_meta(data_dir)
  
  data_sets <- list()
  
  # create empty folder to save processed data
  if(!dir.exists(cleaned_dir)){
    dir.create(cleaned_dir, recursive = TRUE)
  }
  else{
    unlink(file.path(cleaned_dir, "*"))
  }
  
  for (i in 1:length(data_files) ){
    
    current_file <- read_gps(data_files[i])
    df <- current_file$df
    dtype <- current_file$dtype
    
    ## remove any extra copies of the header row
  
    df$Latitude <- as.numeric(df$Latitude)
    df<- df[!is.na(df$Latitude),]
    
    df<-as.data.frame(df)
    
    aniid <- data_info$ani[i]
    gpsid <- data_info$gps[i]
    
    if(data_files[i] == aniid) {
      aniid <- paste0("Unknown_", gsub(paste0(data_dir, "(.*).(csv|txt|TXT)"), "\\1", data_files[i]))
    }
    
    if(data_files[i] == gpsid) {
      gpsid <- paste0("Unknown_", gsub(paste0(data_dir, "(.*).(csv|txt|TXT)"), "\\1", data_files[i]))
    }
    
    nstart <- nrow(df)
    
    print(paste("processing ", nstart, "data points for animal #",aniid, "with gps unit #", gpsid, "..."))
    
    ### REMOVE BAD DATA POINTS (as described on pages 26-39 of Word Doc)
    df<- clean_location_data(df, dtype,
                             aniid = aniid, 
                             gpsid = gpsid, 
                             maxrate = 84, maxcourse = 100, maxdist = 840, maxtime = 100, tz_in = tz_in, tz_out = tz_out)
   
    print(paste("...", nstart - nrow(df), "points removed"))
    print(paste("...total distance traveled =", round(sum(df$DistGeo)/1000, 1), "km"))
    print(paste("...saving", nrow(df), "good data points"))
    
    if(!is.null(cleaned_dir)){
      utils::write.csv(df, file.path(cleaned_dir, paste0(aniid,".csv")), row.names = FALSE)
      pts <- df[c("Longitude", "Latitude")]
      output=sp::SpatialPointsDataFrame(coords=pts,proj4string=sp::CRS("+init=epsg:4326"),
                                        
                                        data=df)
      
      suppressWarnings(rgdal::writeOGR(obj=output,dsn=cleaned_dir,layer= paste0("layer_", i,"_", substr(aniid, 1, nchar(aniid)-4)),
                      
                      driver="ESRI Shapefile"))
    }
    
    
    # add df to the list of data
    data_sets[[paste0("ani",aniid)]] <- df
  }
  if(grepl("\\.rds", cleaned_filename)){
    
    saveRDS(data_sets, cleaned_filename )
  }
  data_sets
}

#'
#'Add big files to a .gitignore file
#'
#'@param data_dir directory of animal data files
#'@return None
#'@examples
#'# Detect large files in the demo directory and add to the .gitignore file
#'\dontrun{
#'dev_add_to_gitignore(system.file("extdata", "demo_nov19", package = "animaltracker"))
#'}
#'@export
#'
dev_add_to_gitignore <- function(data_dir) {
  allfiles <- list.files(data_dir, full.names = TRUE, recursive=T)
  bigfiles <- allfiles[sapply(allfiles, file.size) > 9*10^6]
  bigfiles <- gsub("^\\.","**",bigfiles)
  fileignore <- c("**/elevation/**","*.zip","*.tar", "*.tar.gz", bigfiles )
                  
  
  fileConn<-file(".gitignore")
  writeLines(fileignore, fileConn)
  close(fileConn)
}

