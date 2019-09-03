
if(getRversion() >= '2.5.1') {
  globalVariables(c('dplyr', 'tibble', 'forecast',
                    'Time', 'Altitude', 'Distance', 'TimeDiff', 'Course',
                    'CourseDiff', 'DistGeo', 'RateFlag', 'CourseFlag', 'DistanceFlag',
                    'TotalFlags', 'TimeFlag', 'DuplicateDateFlag'))
}

#'
#'Generate metadata for a directory of animal data files
#'
#'@param data_dir directory of animal data files
#'@return list of data info as a list of animal IDs and GPS units
#'@examples
#'# Get metadata for demo directory
#'
#'get_file_meta(system.file("extdata", "demo_aug19", package = "animaltracker"))
#'@export
#'
get_file_meta <- function(data_dir){
  data_files <- list.files(data_dir, pattern="*.csv") 
  
  gps_units <- gsub("(.*)(20)([0-9]{2}\\_)(.*)(\\_{1}.*)(\\.csv)","\\4",data_files)
  
  ani_ids <- gsub("(.*)(20)([0-9]{2}\\_)(.*\\_)(.*)(\\.csv)","\\5",data_files)
  
  # assign random ids to missing animal ids
  ani_ids_na <- ani_ids == "anixxxx"
  ani_ids[ani_ids_na] <- sample(1000:9999, size=sum(ani_ids_na), replace=F)
  ani_ids[ani_ids_na] <- paste0("R", ani_ids[ani_ids_na])
  
  return(list(ani = ani_ids, gps = gps_units))
}

#'
#'Cleans a raw animal GPS dataset, implementing a standardized procedure to remove impossible values
#'
#'@param df data frame in standardized format (e.g., from a raw spreadsheet)
#'@param autocleans automatically clean data with ts_clean, defaults to false - currently experimental
#'@param filters filter bad data points, defaults to true
#'@param aniid identification code for the animal
#'@param gpsid identification code for the GPS device
#'@param maxrate maximum rate of travel (meters/minute) between consecutive points
#'@param maxcourse maximum distance (meters) between consecutive points
#'@param maxdist maximum geographic distance (meters) between consecutive points
#'@param maxtime maximum time (minutes) between consecutive points 
#'@param timezone time zone, defaults to UTC
#'@examples
#'# Clean a data frame from csv
#'
#'## Read data frame
#'bannock_df <- read.csv(system.file("extdata", "demo_aug19/Bannock_2017_101_1149.csv", 
#'package = "animaltracker"))
#'
#'## Clean and filter
#'clean_location_data(bannock_df, autocleans = FALSE, filters = TRUE, aniid = 1149, 
#'gpsid = 101, maxrate = 84, maxdist = 840, maxtime = 100, timezone = "UTC")
#'
#'## Clean without filtering
#'clean_location_data(bannock_df, autocleans = FALSE, filters = FALSE, aniid = 1149, 
#'gpsid = 101, maxrate = 84, maxdist = 840, maxtime = 100, timezone = "UTC")
#'@export
#'
clean_location_data<- function (df, autocleans = FALSE, filters = TRUE, 
                                aniid = NA, gpsid = NA, 
                                maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=100, timezone = "UTC"){
  df <- df %>% 
    tibble::add_column(Order = df$Index, .before="Index")%>%  # add Order column
    tibble::add_column(Animal = aniid, .after="Index") %>%      # add Animal column 
    tibble::add_column(GPS = gpsid, .after="Animal") %>%      # add Animal column 
    tibble::add_column(DateTime = NA, .after="GPS") %>%      # add Date/Time column
    tibble::add_column(TimeDiff = NA, .after="DateTime") %>% 
    tibble::add_column(TimeDiffMins = NA, .after="TimeDiff") %>%
    tibble::add_column(Rate = NA, .after="Distance") %>%
    tibble::add_column(CourseDiff = NA, .after="Course") %>%
    dplyr::mutate(
      Animal = as.factor(Animal), # reclassify Animal column as a categorical (factor) variable
      DateTime = as.POSIXct(paste(Date, Time), "%Y/%m/%d %H:%M:%S", tz=timezone), # reclassify Date as a Date variable
      Date = as.Date(Date, "%Y/%m/%d"), # reclassify Date as a Date variable
      Time = as.character(Time)
    )
  if(autocleans) {
    df <- df %>% 
      dplyr::mutate(
        Latitude = forecast::tsclean(Latitude),
        Longitude = forecast::tsclean(Longitude),
        Altitude = forecast::tsclean(Altitude),
        Distance = forecast::tsclean(Distance)
      )
  }
  if(filters) {
    df <- df %>% 
      dplyr::filter(!is.na(DateTime), !is.na(Date), !is.na(Time)) %>% # filter missing time slots before calculating differences
      dplyr::distinct(DateTime, .keep_all = TRUE) # remove duplicate timestamps
  }
  df <- df %>% 
    dplyr::mutate(
      TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > maxtime), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), # compute sequential time differences (in seconds)
      TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))), # compute sequential time differences (in mins)
      Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0), # compute rate of travel (meters/min), default to 0 to prevent divide by 0 error
      CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                   cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) ))), #compute geodesic distance between points
      RateFlag = 1*(Rate > maxrate), # flag any data points representing too fast travel
      CourseFlag = 1*(CourseDiff >= maxcourse) ,
      DistanceFlag = 1*(DistGeo >= maxdist )
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
      dplyr::select(-c("RateFlag", "CourseFlag", "DistanceFlag", "TotalFlags")) # remove flags after use
  }
  else {
    df <- df %>% 
      dplyr::mutate(
        TimeFlag = 1*(is.na(DateTime) || is.na(Date) || is.na(Time))
      ) %>%
      tibble::add_column(DuplicateDateFlag = 1*duplicated(df$DateTime)) %>%
      dplyr::mutate(TotalFlags = RateFlag + CourseFlag + DistanceFlag + TimeFlag + DuplicateDateFlag)
  }
}


#'
#'Cleans all animal GPS datasets (in .csv format) in a chosen directory. Optionally exports the clean data as spreadsheets, a single .rds data file, or as a list of data frames
#'
#'@param data_dir directory of GPS tracking files (in csv)
#'@param cleaned_filename full name of output file (ending in .rds), defaults to data/animal_data.rds
#'@param cleaned_dir directory to save the processed GPS datasets as spreadsheets (.csv), defaults to data/processed
#'@param tz timezone for cleaned data, defaults to UTC
#'@examples
#'# Not run:
#'# Clean all animal GPS .csv datasets in the demo directory
#'
#'clean_export_files(system.file("extdata", "demo_aug19", package = "animaltracker"), 
#'cleaned_filename = "ex_animal_data.rds", cleaned_dir = "inst/extdata/demo_aug19", tz = "UTC")
#'
#'# End(Not run)
#'@export
#'
clean_export_files <- function(data_dir, cleaned_filename = "animal_data.rds", cleaned_dir = "processed", tz = "UTC") {
  data_files <- list.files(data_dir, pattern="*.csv", full.names=T)
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
    
    df <- read.csv(data_files[i], skipNul = T, as.is=T)
    
    ## remove any extra copies of the header row
  
    df$Latitude <- as.numeric(df$Latitude)
    df<- df[!is.na(df$Latitude),]
    
    df<-as.data.frame(df)
    
    aniid <- data_info$ani[i]
    gpsid <- data_info$gps[i]
    nstart <- nrow(df)
    
    print(paste("processing ", nstart, "data points for animal #",aniid, "with gps unit #", gpsid, "..."))
    
    ### REMOVE BAD DATA POINTS (as described on pages 26-39 of Word Doc)
    df<- clean_location_data(df, 
                             aniid = aniid, 
                             gpsid = gpsid, 
                             maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=100, timezone = tz)
   
    print(paste("...", nstart - nrow(df), "points removed"))
    print(paste("...total distance traveled =", round(sum(df$DistGeo)/1000, 1), "km"))
    print(paste("...saving", nrow(df), "good data points"))
    
    if(!is.null(cleaned_dir)){
      utils::write.csv(df, file.path(cleaned_dir, paste0(aniid,".csv")), row.names=F)
      pts <- df[c("Longitude", "Latitude")]
      output=sp::SpatialPointsDataFrame(coords=pts,proj4string=sp::CRS("+init=epsg:4326"),
                                        
                                        data=df)
      
      rgdal::writeOGR(obj=output,dsn=cleaned_dir,layer= substr(aniid, 1, nchar(aniid)-4) ,
                      
                      driver="ESRI Shapefile")
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
#'@examples
#'# Not run:
#'# Detect large files in the demo directory and add to the .gitignore file
#'
#'dev_add_to_gitignore(system.file("extdata", "demo_aug19", package = "animaltracker"))
#'
#'# End(Not run)
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

