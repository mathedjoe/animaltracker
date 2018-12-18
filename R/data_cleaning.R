#'
#'Generate metadata for a directory of animal data files
#'
#'@param data_dir directory of animal data files
#'@return list of data info as a list of animal IDs and GPS units
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
#'Cleans all animal GPS datasets in a chosen directory and exports them as a single .rds file
#'
#'@param data_dir directory of GPS tracking files (in csv)
#'@param out_path name of output file, must end in .rds
#'
# 
# example usage:
# clean_export_files( "C:/Users/joechampion/Google Drive/Research/GPS_Arispe_2018/data/to_process", "data/cowGPS.rds")

clean_export_files <- function(data_dir, out_path, processed_dir = "data/processed") {
  data_files <- list.files(data_dir, pattern="*.csv", full.names=T)
  data_info <- get_file_meta(data_dir)
  timezone <- "UTC"
  
  window <- list(latmax = 43.3464, lonmin = -117.2305, latmin = 43.2472, lonmax=-117.101 )
    
  data_sets <- list()
  
  # create empty folder to save processed data
  if(!dir.exists(processed_dir)){
    dir.create(processed_dir, recursive = TRUE)
  }
  else{
    unlink(file.path(processed_dir, "*"))
  }
  
  for (i in 1:length(data_files) ){
    df <- read.csv(data_files[i], skipNul = T)
    aniid <- data_info$ani[i]
    gpsid <- data_info$gps[i]
    nstart <- nrow(df)
    
    print(paste("processing ", nstart, "data points for animal #",aniid, "with gps unit #", gpsid, "..."))
    
    ### REMOVE BAD DATA POINTS (as described on pages 26-39 of Word Doc)
    df<- df %>% 
      tibble::add_column(Order = df$Index, .before="Index")%>%  # add Order column
      tibble::add_column(Animal = aniid, .after="Index") %>%      # add Animal column 
      tibble::add_column(GPS = gpsid, .after="Animal") %>%      # add Animal column 
      tibble::add_column(DateTime = NA, .after="GPS") %>%      # add Date/Time column
      tibble::add_column(TimeDiff = NA, .after="DateTime") %>% 
      tibble::add_column(TimeDiffMins = NA, .after="TimeDiff") %>%
      tibble::add_column(Rate = NA, .after="Distance") %>%
      tibble::add_column(CourseDiff = NA, .after="Course") %>%
      dplyr::mutate(Animal = as.factor(Animal))  %>%                     # reclassify Animal column as a categorical (factor) variable
      dplyr::mutate(DateTime = as.POSIXct(paste(Date, Time), "%Y/%m/%d %H:%M:%S", tz=timezone)) %>%  # reclassify Date as a Date variable
      dplyr::mutate(Date = as.Date(Date, "%Y/%m/%d"))  %>%            # reclassify Date as a Date variable
      dplyr::mutate(TimeDiff = as.numeric(DateTime - dplyr::lag(DateTime,1))) %>%  # compute sequential time differences (in seconds)
      dplyr::mutate(TimeDiffMins = as.numeric(difftime(DateTime,dplyr::lag(DateTime,1), units="mins")))  %>% # compute sequential time differences (in mins)
      dplyr::mutate(Rate = Distance/TimeDiffMins) %>% # compute rate of travel (meters/min)
      dplyr::mutate(CourseDiff = abs(Course - dplyr::lag(Course,1))) %>%
      dplyr::mutate(DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                      cbind(dplyr::lag(Longitude,1), dplyr::lag(Latitude, 1))
      ) ) %>% #compute geodesic distance between points
      dplyr::mutate(RateFlag = 1*(Rate > 84)) %>%  # flag any data points representing too fast travel
      dplyr::mutate(CourseFlag = 1*(CourseDiff >= 100) ) %>%
      dplyr::mutate(DistanceFlag = 1*(DistGeo >= 840 )) %>%
      dplyr::mutate(TotalFlags = RateFlag + CourseFlag + DistanceFlag) %>%
      dplyr::filter(!is.na(DateTime), TotalFlags < 2, 
                    Latitude!=0, Longitude !=0,
                    TimeDiffMins < 100,
                    Altitude > 2700/3.3, Altitude< 6000/3.3, # lower and upper limits (converted from feet to meters)
                    Latitude >= window$latmin,  Latitude <= window$latmax,
                    Longitude >= window$lonmin,  Longitude <= window$lonmax,
                    !DistanceFlag ) 
   
    print(paste("...", nstart - nrow(df), "points removed"))
    print(paste("...total distance traveled =", round(sum(df$DistGeo)/1000, 1), "km"))
    print(paste("...saving", nrow(df), "good data points"))
    
    
    write.csv(df, file.path(processed_dir, paste0(aniid,".csv")), row.names=F)
    
    # add df to the list of data
    data_sets[[paste0("ani",aniid)]] <- df
}
  saveRDS(data_sets, out_path)
}

#'
#'Add big files to a .gitignore file
#'
#'@param data_dir directory of animal data files
#'
add_to_gitignore <- function(data_dir) {
  allfiles <- list.files(data_dir, full.names = TRUE, recursive=T)
  bigfiles <- allfiles[sapply(allfiles, file.size) > 9*10^6]
  bigfiles <- gsub("^\\.","**",bigfiles)
  fileignore <- c("**/elevation/**","*.zip","*.tar", "*.tar.gz", bigfiles )
                  
  
  fileConn<-file(".gitignore")
  writeLines(fileignore, fileConn)
  close(fileConn)
}
