#'
#'Generate metadata for a directory of animal data files
#'
#'@param data_dir directory of animal data files
#'@return list of data info as a list of animal IDs and GPS units
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
#'@param aniid identification code for the animal
#'@param gpsid identification code for the GPS device
#'@param maxrate maximum rate of travel (meters/minute) between consecutive points
#'@param maxcourse maximum distance (meters) between consecutive points
#'@param maxdist maximum geographic distance (meters) between consecutive points
#'@param maxtime maximum time (minutes) between consecutive points 
#'@param timezone time zone, defaults to UTC
#'@export
#'
clean_location_data<- function (df, aniid = NA, gpsid = NA, maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=100, timezone = "UTC"){
  require(dplyr)
  require(tibble)
  df %>% 
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
    ) %>%
    dplyr::filter(!is.na(DateTime), !is.na(Date), !is.na(Time)) %>% # filter missing time slots before calculating differences
    dplyr::distinct(DateTime, .keep_all = TRUE) %>% # remove duplicate timestamps
    dplyr::mutate(
      TimeDiff = as.numeric(DateTime - dplyr::lag(DateTime,1,default=first(DateTime))), # compute sequential time differences (in seconds)
      TimeDiffMins = as.numeric(difftime(DateTime,dplyr::lag(DateTime,1,default=first(DateTime)), units="mins")), # compute sequential time differences (in mins)
      Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0), # compute rate of travel (meters/min), default to 0 to prevent divide by 0 error
      CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                   cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) ))), #compute geodesic distance between points
      
      RateFlag = 1*(Rate > maxrate), # flag any data points representing too fast travel
      CourseFlag = 1*(CourseDiff >= maxcourse) ,
      DistanceFlag = 1*(DistGeo >= maxdist ),
      TotalFlags = RateFlag + CourseFlag + DistanceFlag
    ) %>%
    dplyr::filter(!is.na(Longitude), !is.na(Latitude),
                  Latitude != 0, Longitude !=0,
                  TotalFlags < 2,
                  TimeDiffMins < maxtime,
                  !DistanceFlag ) %>%
    dplyr::mutate( # recalculate columns affected by filtering
      TimeDiff = as.numeric(DateTime - dplyr::lag(DateTime,1,default=first(DateTime))),
      TimeDiffMins = as.numeric(difftime(DateTime, dplyr::lag(DateTime,1,default=first(DateTime)), units="mins")),
      Rate = ifelse(TimeDiffMins != 0, Distance/TimeDiffMins, 0),
      CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
      DistGeo = geosphere::distGeo(cbind(Longitude, Latitude),
                                   cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude))))
    ) %>%
    dplyr::select(-c("RateFlag", "CourseFlag", "DistanceFlag", "TotalFlags")) # remove flags after use
}


#'
#'Cleans all animal GPS datasets (in .csv format) in a chosen directory. Optionally exports the clean data as spreadsheets, a single .rds data file, or as a list of data frames
#'
#'@param data_dir directory of GPS tracking files (in csv)
#'@param cleaned_filename full name of output file (ending in .rds), defaults to data/animal_data.rds
#'@param cleaned_dir directory to save the processed GPS datasets as spreadsheets (.csv), defaults to data/processed
#'@param tz timezone for cleaned data, defaults to UTC
#'@export
#'
clean_export_files <- function(data_dir, cleaned_filename = "data/animal_data.rds", cleaned_dir = "data/processed", tz = "UTC") {
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
      write.csv(df, file.path(cleaned_dir, paste0(aniid,".csv")), row.names=F)
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

