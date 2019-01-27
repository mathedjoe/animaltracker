###
# DATA STORAGE PROCESS
# 1. new data is uploaded through the app 
#   (user uploads files (maybe in a zip folder) or points to a folder on their computer)
# 2. clean the data one file at a time
# 3. save meta data
#     (fileid, filename, site, date min/max, cows, min/max lat/longitude, storage location (rds))
# 4. cleaned data is stored in an rds (e.g., 50 files of data per rds)
# 5. cache.rds keeps track of recent data frames used by the shiny app
# 6. current.df is the current set of data used by the shiny app
#
# functions:
#    clean_batch
#       - clean_df
#       - get_meta
#       - save_meta
#    get_data_from_meta
#       - options to select days/times/cows/sites
#       - check if cached
#    save_to_cache
#   
# the shiny app needs to use some of these functions

#'
#'Cleans a directory of animal data files and stores them in .rds files
#'
#'@param data_dir location of animal data files, in list format
#'@return df of metadata for animal file directory
#'
clean_batch <- function(data_dir) {
  #initialize empty meta
  meta_df <- data.frame(matrix(ncol = 9, nrow = 0))
  meta_cols <- c("file_id", "file_name", "site", "ani_id", "min_date", "max_date", "min_lat", "max_lat", "storage")
  colnames(meta_df) <- meta_cols
  
  dir_name <- gsub(".zip", "", data_dir$name)

  data_files <- unzip(data_dir$datapath, exdir="temp")
  data_files <- list.files("temp", pattern ="*.csv", recursive = T, full.names = T)
  
  data_sets <- list()
  num_saved_rds <- 0
 
  rds_name <- paste0(dir_name, ".rds")
  
  gps_units <- gsub("(.*)(20)([0-9]{2}\\_)(.*)(\\_{1}.*)(\\.csv)","\\4",data_files)
  ani_ids <- gsub("(.*)(20)([0-9]{2}\\_)(.*\\_)(.*)(\\.csv)","\\5",data_files)
  
  ani_ids <- make.unique(ani_ids, sep="_")

  data_info <- list(ani = ani_ids, gps = gps_units)

  for(i in 1:length(data_files)) {
    filestr <- gsub(paste0("(temp)(\\/)", dir_name, "(\\/)"), "", data_files[i])
    
    site <- ifelse( grepl("\\_", filestr), tolower(sub("\\_.*","", filestr)), "Unknown")
    
    df <- read.csv(data_files[i], skipNul = T)
    
    aniid <- data_info$ani[i]
    gpsid <- data_info$gps[i]
    
    if(data_files[i] == aniid) {
      aniid <- paste0("Unknown (", filestr, ")")
    }
    
    if(data_files[i] == gpsid) {
      gpsid <- paste0("Unknown")
    }
    
    #on every 50th data file, increment file name counter and wipe data_sets
    if(i %% 50 == 0 & i > 49) {
      saveRDS(data_sets, rds_name)
      num_saved_rds <- num_saved_rds + 1
      rds_name <- paste0(data_dir, num_saved_rds, ".rds")
      data_sets <- list()
    }
    # clean df
    df_out <- clean_df(df, aniid, gpsid)
    
    # get meta from df
    file_meta <- get_meta(df_out, i, data_files[i], site, aniid, rds_name)
    # save meta to the designated meta df
    meta_df <- save_meta(meta_df, file_meta)
    # add cleaned df to the list of data
    data_sets[[paste0("ani",aniid)]] <- df_out
    
  } #for loop
  #save remaining data files
  saveRDS(data_sets, rds_name)
  unlink("temp", recursive = T)
  return(meta_df)
}

#'
#'Clean animal data frame
#'
#'@param df raw input data frame
#'@param ani_id animal ID (from meta) 
#'@param gps_id GPS ID (from meta)
#'@return cleaned data frame
#'
clean_df <- function(df, ani_id, gps_id) {
  timezone <- "UTC"
  window <- list(latmax = 43.3464, lonmin = -117.2305, latmin = 43.2472, lonmax=-117.101 )
  nstart <- nrow(df)
  ### REMOVE BAD DATA POINTS (as described on pages 26-39 of Word Doc)
  df<- df %>% 
    tibble::add_column(Order = df$Index, .before="Index")%>%  # add Order column
    tibble::add_column(Animal = ani_id, .after="Index") %>%      # add Animal column 
    tibble::add_column(GPS = gps_id, .after="Animal") %>%      # add Animal column 
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
 
  return(df)
}

#'
#'Generate metadata for an animal data frame -
#'filename, site, date min/max, animals, min/max lat/longitude, storage location 
#'
#'@param df clean animal data frame 
#'@param file_id ID number of .csv source of animal data frame
#'@param file_name .csv source of animal data frame
#'@param site physical source of animal data
#'@param ani_id ID of animal found in data frame
#'@param storage_loc .rds storage location of animal data frame
#'@return df of metadata for animal data frame 
get_meta <- function(df, file_id, file_name, site, ani_id, storage_loc) {
   return(data.frame(file_id = file_id, 
            file_name = file_name, 
            site = site, 
            ani_id = ani_id, 
            min_date = min(df$Date), 
            max_date = max(df$Date), 
            min_lat = min(df$Latitude), 
            max_lat = max(df$Latitude),
            min_long = min(df$Longitude), 
            max_long = max(df$Longitude), 
            storage = storage_loc))
}

#'
#'Save metadata to a data frame and return it
#'
#'@param meta_df the data frame to store metadata in
#'@param file_meta meta for a .csv file generated by get_meta
#'
save_meta <- function(meta_df, file_meta) {
  meta_df <- rbind(meta_df, file_meta)
  return(meta_df)
}

#'
#'Get animal data set from specified meta
#'
#'@param meta_df data frame of specified meta
#'@param min_date minimum date specified by user
#'@param max_date maximum date specified by user
#'
get_data_from_meta <- function(meta_df, min_date, max_date) {
  meta_df$storage <- as.character(meta_df$storage)
  rds_files <- list(unique(meta_df$storage))
  current_df <- data.frame()
  for(file_name in rds_files) {
    current_rds <- readRDS(file_name)
    for(df in current_rds) {
      current_df <- rbind(current_df, df)
    }
  }
  current_df <- current_df %>%
    dplyr::filter(Animal %in% meta_df$ani_id,
           Date <= max_date,
           Date >= min_date)
  
  # add elevation data
  current_df <- lookup_elevation(current_df)
  
  return(current_df)
}

