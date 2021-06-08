
if(getRversion() >= '2.5.1') {
  globalVariables(c('dbscan', 'dplyr', 'tibble', 'forecast', 'tsbox', 'imputeTS', 'tidyverse',
                    'Time', 'Altitude', 'Distance', 'TimeDiff', 'Course',
                    'CourseDiff', 'DistGeo', 'RateFlag', 'CourseFlag', 'DistFlag',
                    'TotalFlags', 'TimeFlag', 'DuplicateDateFlag', 'RMCRecord',
                    'ChecksumRMC', 'GGARecord', 'AltitudeM', 'HeightM', 'DGPSUpdate',
                    'ChecksumGGA', 'DateTimeChar', 'nSatellites', 'GroundSpeed',
                    'TrackAngle', 'hDilution', 'Height', 'Status', 'LatitudeFix',
                    'LongitudeFix', 'MagVar', 'Satelite', 'MegaRateFlag',  'Keep',
                    '.', 'DistFlag'))
}

#'
#'Generate metadata for a directory of animal data files
#'
#'@param data_dir directory of animal data files
#'@return list of data info as a list of animal IDs and GPS units/ka
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
#'@param prep reformat columns if all required columns are not present, defaults to True
#'@param filters filter bad data points, defaults to true
#'@param aniid identification code for the animal
#'@param gpsid identification code for the GPS device
#'@param maxrate maximum rate of travel (meters/minute) between consecutive points
#'@param maxcourse maximum distance (meters) between consecutive points
#'@param maxdist maximum geographic distance (meters) between consecutive points
#'@param maxtime maximum time (minutes) between consecutive points 
#'@param tz_in input time zone, defaults to UTC
#'@param tz_out output time zone, defaults to UTC
#'@return data frame of clean animal GPS data
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
clean_location_data <- function(df, dtype, 
                                prep = TRUE, filters = TRUE, 
                                aniid = NA, gpsid = NA, 
                                maxrate = 84, maxcourse = 100, maxdist = 840, maxtime=60*60, tz_in = "UTC", tz_out = "UTC",
                                dbscan_enable=FALSE, kalman = FALSE, kalman_min_lat=43, kalman_max_lat=44, kalman_min_lon=-117, kalman_max_lon=-116, kalman_max_timestep=300,
                                dbscan_knn_eps, dbscan_knn_k, dbscan_interp) {
  if(prep) {
    # make sure quantitative columns are read in properly
    df <- df %>% 
      dplyr::mutate(Latitude = as.numeric(Latitude),
                    Longitude = as.numeric(Longitude))
    if(dtype == "columbus" & ("RMCRecord" %in% colnames(df))) {
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
      ) %>% 
      filter(!is.na(Date))
  }
  if(filters) {
    
    df <- df %>% 
      dplyr::filter(!is.na(DateTime), !is.na(Date), !is.na(Time), nSatellites > 0) %>% 
      dplyr::distinct(DateTime, .keep_all = TRUE) # remove duplicate timestamps
  }

  ## special function for processing gps data with igotu (protocol from Colt Knight)
  process_gps_igotu <- function(df_igotu){
    df_igotu %>%
      dplyr::mutate(
        TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="secs")) > maxtime), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), # compute sequential time differences (in seconds)
        TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))),
        DistGeo = geosphere::distGeo(cbind(Longitude, Latitude), 
                                     cbind(dplyr::lag(Longitude,1,default=first(Longitude)), dplyr::lag(Latitude,1,default=first(Latitude) ))), #compute geodesic distance between points
        DistGeo = ifelse(DistGeo > 10^6, 0, DistGeo), 
        Rate = DistGeo/TimeDiffMins, # compute rate of travel (meters/min)
        CourseDiff = abs(Course - dplyr::lag(Course,1,default=first(Course))),
        ### implement filtering rules
        TimeFlag =1*(TimeDiff == 0),
        RateFlag = 1*(Rate >= maxrate | is.na(Rate)), # flag any data points representing too fast travel
        MegaRateFlag = 1*(Rate >= 10*maxrate | is.na(Rate)), # flag any data with severe rates
        CourseFlag = 1*(CourseDiff >= maxcourse), # flag any data with large change in course
        DistFlag = 1*(DistGeo >= maxdist | ( (TimeDiffMins!=0) & DistGeo/TimeDiffMins > maxrate) ), # flag any large change in distance
        TotalFlags = RateFlag + CourseFlag + DistFlag,
        Keep = 1*(TotalFlags < 2 & !DistFlag & !MegaRateFlag & !TimeFlag) # implement key filtering rule
    ) 
  }
  
  df <- process_gps_igotu(df)
   
  
    if(filters) {
      df <- df %>%
        dplyr::filter( as.logical(Keep) ) %>%
        process_gps_igotu(.) %>%
        dplyr::filter( as.logical(Keep) ) %>%
        dplyr::select(-contains("Flag"), -Keep) # remove flags after use
      
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
        dplyr::mutate(TotalFlags = RateFlag + CourseFlag + DistFlag + TimeFlag + DuplicateDateFlag)
    }
  
  # After all other processing has been done, apply new filters
  if(kalman) {
    df <- kalman(df, min_longitude=kalman_min_lon, max_longitude=kalman_max_lon,
                 min_latitude=kalman_min_lat, max_latitude=kalman_max_lat,
                 max_timestep=kalman_max_timestep)
  }
  
  if(dbscan_enable) {
    df <- dbscan_api(df, dbscan_knn_eps=dbscan_knn_eps, dbscan_knn_k=dbscan_knn_k,
                     dbscan_interp=dbscan_interp)
  }
  
  return(as.data.frame(df))
}



#'
#'Cleans all animal GPS datasets (in .csv format) in a chosen directory. Optionally exports the clean data as spreadsheets, a single .rds data file, or as a list of data frames
#'
#'@param data_dir directory of GPS tracking files (in csv)
#'@param tz_in input time zone, defaults to UTC
#'@param tz_out output time zone, defaults to UTC
#'@param export logical, whether to export the clean data, defaults to False
#'@param cleaned_filename full name of output file (ending in .rds) when export is True
#'@param cleaned_dir directory to save the processed GPS datasets as spreadsheets (.csv) when export is True
#'@return list of cleaned animal GPS datasets
#'@examples
#'# Clean all animal GPS .csv datasets in the demo directory
#'
#'clean_export_files(system.file("extdata", "demo_nov19", package = "animaltracker"))
#
#'@export
#'
clean_export_files <- function(data_dir, tz_in = "UTC", tz_out = "UTC", export = FALSE, cleaned_filename = NULL, cleaned_dir = NULL) {
  data_files <- list.files(data_dir, pattern = "*.(csv|txt|TXT)", recursive = TRUE, full.names = T)
  data_info <- get_file_meta(data_dir)
  
  data_sets <- list()
  
  # create empty folder to save processed data
  if(export) {
    if(!dir.exists(cleaned_dir)){
      dir.create(cleaned_dir, recursive = TRUE)
    }
    else{
      unlink(file.path(cleaned_dir, "*"))
    }
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
    
    message(paste("processing ", nstart, "data points for animal #",aniid, "with gps unit #", gpsid, "..."))
    
    ### REMOVE BAD DATA POINTS (as described on pages 26-39 of Word Doc)
    df <- clean_location_data(df, dtype,
                             aniid = aniid, 
                             gpsid = gpsid, 
                             maxrate = 84, maxcourse = 100, maxdist = 840, maxtime = 100, tz_in = tz_in, tz_out = tz_out)
    
    message(paste("...", nstart - nrow(df), "points removed"))
    message(paste("...total distance traveled =", round(sum(df$DistGeo)/1000, 1), "km"))
    message(paste("...saving", nrow(df), "good data points"))
    
    if(export & !is.null(cleaned_dir)){
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
  if(export & !is.null(cleaned_filename)) {
    if(grepl("\\.rds", cleaned_filename)){
      saveRDS(data_sets, cleaned_filename )
    }
  }
  return(data_sets)
}

#'
#'Add big files to a .gitignore file
#'
#'@param data_dir directory of animal data files
#'@return None
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

# Implements Kalman filter for filtering
kalman <- function(cattle_df, min_longitude=-117, max_longitude=-116, min_latitude=43, max_latitude=44, max_timestep=300) {
  ## read sample data
  ## - label outliers using a 'window' on longitudes and latitudes
  cattle_df <- cattle_df %>%
    mutate(DateTime = paste(Date, Time),
           DateTime = as.POSIXct(DateTime),
           isOutlier = Longitude < min_longitude | Longitude > max_longitude | Latitude < min_latitude | Latitude > max_latitude
    ) %>%
    arrange(DateTime)

  ## convert the irregular measurement data to a time series (equally spaced measurements)
  ##  - for each date in the sample, build a sequence of equally spaced times at
  ##  - use one second intervals
  ##  - this will make a MUCH larger data set, with many NA values for missing measurements
  date_time_seq <- lapply( unique( cattle_df$Date[!cattle_df$isOutlier] ),
                           function(this_date){
                             df_day <- cattle_df %>%
                               filter(Date == this_date, !isOutlier)
                             # make an equally-spaced sequence of date-times for this date
                             seq.POSIXt(min(df_day$DateTime),
                                        max(df_day$DateTime), by = 'sec')
                           }
  )  %>% Reduce(c, .)
  
  ## create time series object with 3 columns: DateTime, Longitude, Latitude
  cattle_ts <- data.frame(DateTime = date_time_seq) %>%
    left_join(cattle_df, by = "DateTime") %>%
    mutate(Longitude = ifelse(isOutlier, NA, Longitude),
           Latitude = ifelse(isOutlier, NA, Latitude)) %>%
    select(DateTime, Longitude, Latitude) %>%
    tsbox::ts_df() # uses library tsbox
  
  ## fill in missing data using kalman smoothing
  # do not attempt to fill-in more than maxgap measurements (e.g., 5 minutes)
  cattle_ts_smoothed <- imputeTS::na_kalman(cattle_ts,
                                            model = "StructTS", smooth = TRUE, type = "trend",
                                            maxgap = 60*5) %>%
    rename(Latitude_Clean = Latitude, Longitude_Clean = Longitude) %>%
    filter(!is.na(Latitude_Clean), !is.na(Longitude_Clean)) %>%
    left_join(cattle_df %>% filter(!isOutlier), by = "DateTime") %>%
    mutate(Date = as.factor(as.character(as.Date(DateTime))) )

  return(cattle_ts_smoothed)
}

dbscan_api <- function(df, dbscan_knn_eps, dbscan_knn_k, dbscan_interp) {
  cluster_analyze <- function(data, animal, date, knn_k = dbscan_knn_k, knn_eps = dbscan_knn_eps) {
    df <- data %>% 
      filter(Date == date, Animal == animal) %>%
      arrange(Index) %>% 
      select(Longitude, Latitude) 
    
    res <- dbscan(df, eps = knn_eps, minPts = knn_k)
    
    data_out <- df %>%
      mutate(cluster = as.factor(ifelse(res$cluster ==0, NA, res$cluster))) 
    
    list(data = data_out, plot = plot_out)
  }
  
  # set-up neighbor search radii
  # about 111 km per degree latitude
  # cows = 84 m max travel between measurements
  # cars = 80 mi/h max (= 129 km/h)
  cow_eps <- 84/(111*1000)
  
  car_eps <- 128.75 /(6*111)
  
  clusters <- cluster_analyze(df, sampling_combos$Animal[28], sampling_combos$Date[28], knn_eps = cow_eps)
  
  clusters_car <- cluster_analyze(df, sampling_combos$Animal[2], sampling_combos$Date[2], 
                                     knn_eps = car_eps*.48)
  
  ## Optimizing epsilon for DBSCAN algorithm
  
  optimize_eps <- function(data, animal, date, knn_k = dbscan_knn_k) {
    df <- data %>% 
      filter(Date == date, Animal == animal) %>%
      arrange(Index) %>% 
      select(Longitude, Latitude) 
    
    index <- 1:nrow(df) 
    knn_dists <- sort(kNNdist(df, k = knn_k)) # sort nearest neighbor distances in ascending order
    dists_spline <- smooth.spline(x = index, y = knn_dists, df = 20) # interpolate data indices and kNN distances
    curvature <- predict(dists_spline, x = index, deriv = 2) # get second derivative of interpolating spline
    return(knn_dists[which.max(curvature$y)]) # return the kNN distance at the index of the maximum second derivative
  }
  
  # Get overview of data by Animal and Date
  df_summary <- df %>% 
    dplyr::group_by(Animal, Date) %>% 
    dplyr::summarise(n = n())
  
  return(df_summary)
  
  #eps_estimates <- c()
  
  # Get epsilon estimates via Monte Carlo simulation
  #for(i in 1:100) {
    #sample_i <- df_summary[sample(nrow(df_summary), 30), ]
    #eps_estimate <- lapply( 1:nrow(sample_i), 
                            #function(j){
                              #optimize_eps(df, sample_i$Animal[j], sample_i$Date[j])
                            #} 
    #)
    #eps_estimates <- c(eps_estimates, median(unlist(eps_estimate)))
  #}
  
  #eps_final <- median(eps_estimates)
  
  ###########
  ## try OPTICS algorithm
  ## helps identify eps value for dbscan
  #xdf <- df %>% 
    #filter(Date == "2018-05-24", Animal == "011") %>%
    #select(Longitude, Latitude)
  
 # res <- optics(xdf, minPts = 10)
  
  #res2 <- extractDBSCAN(res, eps_cl = .2)
  
  #data_out <- xdf %>%
    #mutate(cluster = as.factor(ifelse(res2$cluster ==0, NA, res2$cluster))) 
}