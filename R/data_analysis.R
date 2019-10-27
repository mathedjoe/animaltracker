
### Summary Functions

if(getRversion() >= '2.5.1') {
  globalVariables(c('GPS', 'Altitude', 'Distance', 'Course',
                    'n.x', 'n.y', 'meanLat.x', 'meanLat.y', 'sdLat.x', 'sdLat.y',
                    'meanLong.x', 'meanLong.y', 'sdLong.x', 'sdLong.y', 'meanDist.x',
                    'meanDist.y', 'sdDist.x', 'sdDist.y', 'meanCourse.x', 'meanCourse.y',
                    'sdCourse.x', 'sdCourse.y', 'meanRate.x', 'meanRate.y', 'sdRate.x',
                    'sdRate.y', 'meanElev.x', 'meanElev.y', 'sdElev.x', 'sdElev.y',
                    'nDiff', 'meanLatDiff', 'meanLongDiff', 'sdLongDiff', 'meanDistDiff',
                    'sdDistDiff', 'meanCourseDiff', 'sdCourseDiff', 'meanRateDiff',
                    'sdRateDiff', 'meanElevDiff', 'sdLatDiff', 'sdElevDiff',
                    'avg', 'Data', 'obs'))
}

#'
#'Summarise a number of animal datasets by GPS unit
#'
#'@param rds_path Path of .rds cow data file to read in
#'@return summary statistics for animals by GPS unit
#'@examples
#'# Read in .rds of demo data and summarise by GPS unit
#'
#'summarise_unit(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
summarise_unit <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  summary <- anidata %>% 
    group_by(GPS) %>%
    summarize( nani = length(unique(Animal)),
               meanAlt = mean(Altitude),
               sdAlt = stats::sd(Altitude),
               minAlt = min(Altitude),
               maxAlt = max(Altitude))
  return(summary)
}

#'
#'Determines the GPS measurement time value difference values
#'roughly corresponding to quantiles with .05 intervals.
#'
#'@param rds_path Path of .rds animal data file to read in
#'@return approximate time difference values corresponding to quantiles (.05 intervals)
#'@examples
#'# Read in .rds of demo data and calculate time difference quantiles
#'
#'quantile_time(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
quantile_time <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  quantile <- quantile(anidata$TimeDiffMins, probs = seq(0,1,.05))
  return(quantile)
}

#'
#'Get summary statistics for a single column in an animal data frame
#'
#'@param df animal data frame
#'@param col column to get summary stats for, as a string
#'@return data frame of summary stats for col
#'@examples
#'# Get summary statistics for Distance column of demo data
#'
#'summarise_col(demo, Distance)
#'
#'@export
summarise_col <- function(df, col) {
  col <- enquo(col)
  summary <- df %>%
    dplyr::group_by(Animal) %>%
    dplyr::summarise(
      N = n(),
      Mean = mean(!!col),
      Median = stats::median(!!col),
      SD = stats::sd(!!col),
      Variance = stats::var(!!col),
      Q1 = stats::quantile(!!col, 0.25),
      Q3 = stats::quantile(!!col, 0.75),
      IQR = stats::IQR(!!col),
      Range = (max(!!col)-min(!!col)),
      Min = min(!!col),
      Max = max(!!col)
    )
  return(summary)
}

### Plotting Functions

#'
#'Generates a boxplot to visualize the distribution of altitude
#'by GPS.
#'
#'@param rds_path Path of .rds animal data file to read in
#'@return overall boxplot of altitude by GPS
#'@examples
#'# Boxplot of altitude for demo data .rds
#'
#'boxplot_altitude(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
boxplot_altitude <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- dplyr::bind_rows(ani)
  plot <- ggplot(anidata, aes(x=GPS, y=Altitude))+
    geom_boxplot()+
    theme_minimal()
  return(plot)
}


#'
#'Generates a histogram to visualize the distribution of time
#'between GPS measurements.
#'
#'@param rds_path Path of .rds cow data file to read in
#'@return distribution of time between GPS measurements, as a histogram
#'@examples
#'# Histogram of GPS measurement time differences for demo data .rds
#'
#'histogram_time(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
histogram_time <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  plot <- ggplot(anidata, aes(x=TimeDiffMins))+
    geom_histogram( col= "white") + 
    ggtitle("Distribution of Time Between GPS Measurements" )+ 
    theme_minimal()
  return(plot)
}

#'
#'Generates a histogram to visualize the distribution of time between
#'GPS measurements by GPS unit.
#'
#'@param rds_path Path of .rds animal data file to read in
#'@return distribution of time between GPS measurements by GPS unit, as a histogram
#'@examples
#'# Histogram of GPS measurement time differences by GPS unit for demo data .rds
#'
#'histogram_time_unit(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
histogram_time_unit <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  plot <- ggplot(anidata, aes(x=TimeDiffMins))+
    geom_histogram( col= "white") + 
    facet_wrap(~GPS)+
    ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+ 
    theme_minimal()
  return(plot)
}

#'
#'Generates a boxplot to visualize the distribution of time between
#'GPS measurements by GPS unit.
#'
#'@param rds_path Path of .rds animal data file to read in
#'@return distribution of time between GPS measurements by GPS unit, as a boxplot
#'@examples
#'# Boxplot of GPS measurement time differences for demo data .rds
#'
#'boxplot_time_unit(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
boxplot_time_unit <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  plot <- ggplot(anidata, aes(x=GPS, y=TimeDiffMins))+
    geom_boxplot() + 
    coord_flip()+
    ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+ 
    theme_minimal()
  return(plot)
}

#'
#'Generates a QQ plot to show the distribution of time between GPS measurements.
#' 
#'@param rds_path Path of .rds animal data file to read in
#'@return quantile-quantile plot to show distribution of time between GPS measurements
#'@examples
#'# QQ plot of GPS measurment time differences for demo data .rds
#'
#'qqplot_time(system.file("extdata", "demo_aug19.rds", package = "animaltracker"))
#'@export
#'
qqplot_time <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  plot <- ggplot(anidata, aes(sample = TimeDiffMins)) +
  stat_qq()
  return(plot)
}

#'
#'Compares two animal data frames and calculates summary statistics. 
#'GPS, date, lat, long, course, distance, rate, elevation column names should match. 
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@param gps_out desired file name of .csv output summary by GPS collar
#'@param date_out desired file name of .csv output summary by date
#'@return list containing gps_out and date_out as dfs
#'@examples
#'# Compare and summarise unfiltered demo cows to filtered 
#'\donttest{
#'\dontrun{
#'## Get elevation
#'elevfile <- system.file("extdata/elev", "USA_msk_alt.zip", package="animaltracker")
#'elev <- read_zip_to_rasters( elevfile )
#'
#'## Get elevation data for unfiltered demo
#'unfiltered_elev <- lookup_elevation(elev, demo_unfiltered, zoom=11, 
#'get_slope=FALSE, get_aspect=FALSE)
#'
#'## Get elevation data for filtered demo
#'filtered_elev <- lookup_elevation(elev, demo_filtered, zoom=11, get_slope=FALSE, get_aspect=FALSE)
#'
#'## Compare and summarise
#'compare_summarise_data(unfiltered_elev, filtered_elev, "ex_gps_compare.csv", "ex_date_compare.csv")
#'}
#'}
#'@export
#'
compare_summarise_data <- function(correct, candidate, gps_out, date_out) {
  
  correct_gps_summary <- correct %>% 
    summarise_anidf(GPS, Latitude, Longitude, Distance, Course, Rate, Elevation)
 
  correct_date_summary <- correct %>% 
    summarise_anidf(Date, Latitude, Longitude, Distance, Course, Rate, Elevation)
  
  candidate_gps_summary <- candidate %>% 
    summarise_anidf(GPS, Latitude, Longitude, Distance, Course, Rate, Elevation)
  
  candidate_date_summary <- candidate %>% 
    summarise_anidf(Date, Latitude, Longitude, Distance, Course, Rate, Elevation)
  
  gps_summary <- join_summaries(correct_gps_summary, candidate_gps_summary, by_str="GPS")
  date_summary <- join_summaries(correct_date_summary, candidate_date_summary, by_str="Date")
  
  utils::write.csv(gps_summary, gps_out, row.names = F)
  utils::write.csv(date_summary, date_out, row.names = F)
  
  return(list(GPS = gps_summary, Date = date_summary))
}

#'
#'Calculates summary statistics for an animal data frame
#'
#'@param anidf the animal data frame
#'@param by column to group by, null if daily=T
#'@param lat latitude column
#'@param long longitude column
#'@param dist distance column
#'@param course course column
#'@param rate rate column
#'@param elev elevation column
#'@param daily whether to group by both GPS and Date for daily summary, defaults to False
#'@examples
#'# Summary of demo data by date
#'
#'summarise_anidf(demo, Date, Latitude, Longitude, Distance, Course, Rate, Elevation, daily=FALSE)
#'
#'@export
#'
summarise_anidf <- function(anidf, by, lat, long, dist, course, rate, elev, daily=F) {
  by <- dplyr::enquo(by)
  lat <- dplyr::enquo(lat)
  long <- dplyr::enquo(long)
  dist <- dplyr::enquo(dist)
  course <- dplyr::enquo(course)
  rate <- dplyr::enquo(rate)
  elev <- dplyr::enquo(elev)
  if(daily) {
    anidf <- anidf %>% 
      dplyr::group_by(GPS, Date)
  }
  else {
    anidf <- anidf %>% 
      dplyr::group_by(!!by) 
  }
  anidf %>% 
    dplyr::summarise(n = n(),
                     meanLat = mean(!!lat),
                     sdLat = stats::sd(!!lat),
                     meanLong = mean(!!long),
                     sdLong = stats::sd(!!long),
                     meanDist = mean(!!dist),
                     sdDist = stats::sd(!!dist),
                     meanCourse = mean(!!course),
                     sdCourse = stats::sd(!!course),
                     meanRate = mean(!!rate),
                     sdRate = stats::sd(!!rate),
                     meanElev = mean(!!elev),
                     sdElev = stats::sd(!!elev))
}

#'
#'Joins two animal data frame summaries by a column and appends differences
#'
#'@param correct_summary summary df of reference dataset, returned by summarise_anidf
#'@param candidate_summary summary df of dataset to be compared to reference, returned by summarise_anidf
#'@param by_str column to join by as a string, null if daily=T
#'@param daily whether to group by both GPS and Date for daily summary, defaults to False
#'@examples
#'# Join date summaries of unfiltered and filtered demo data
#'\donttest{
#'\dontrun{
#'## Get elevation 
#'elevfile <- system.file("extdata/elev", "USA_msk_alt.zip", package="animaltracker")
#'elev <- read_zip_to_rasters(elevfile )
#'
#'## Get elevation data for unfiltered demo
#'unfiltered_elev <- lookup_elevation(elev, demo_unfiltered, zoom=11, 
#'get_slope=FALSE, get_aspect=FALSE)
#'
#'## Get elevation data for filtered demo
#'filtered_elev <- lookup_elevation(elev, demo_filtered, zoom=11, get_slope=FALSE, get_aspect=FALSE)
#'
#'## Summarise unfiltered demo by date
#'unfiltered_summary <- summarise_anidf(unfiltered_elev, Date, Latitude, Longitude, 
#'Distance, Course, Rate, Elevation, daily=FALSE)
#'
#'## Summarise filtered demo by date
#'filtered_summary <- summarise_anidf(filtered_elev, Date, Latitude, Longitude, 
#'Distance, Course, Rate, Elevation, daily=FALSE)
#'
#'## Join
#'join_summaries(unfiltered_summary, filtered_summary, "Date", daily=F)
#'
#'}
#'}
#'@export
#'
#'
join_summaries <- function(correct_summary, candidate_summary, by_str, daily=F) {
  if(daily) {
    summary_all <- dplyr::full_join(correct_summary, candidate_summary, by=c("GPS", "Date"))
  }
  else {
    summary_all <- dplyr::full_join(correct_summary, candidate_summary, by=by_str)
  }
   summary_all <- summary_all %>% 
    # create difference columns
    dplyr::mutate(nDiff = n.x - n.y) %>% 
    dplyr::mutate(meanLatDiff = meanLat.x - meanLat.y) %>% 
    dplyr::mutate(sdLatDiff = sqrt((sdLat.x)^2 + (sdLat.y)^2)) %>% 
    dplyr::mutate(meanLongDiff = meanLong.x - meanLong.y) %>% 
    dplyr::mutate(sdLongDiff = sqrt((sdLong.x)^2 + (sdLong.y)^2)) %>% 
    dplyr::mutate(meanDistDiff = meanDist.x - meanDist.y) %>% 
    dplyr::mutate(sdDistDiff = sqrt((sdDist.x)^2 + (sdDist.y)^2)) %>% 
    dplyr::mutate(meanCourseDiff = meanCourse.x - meanCourse.y) %>% 
    dplyr::mutate(sdCourseDiff = sqrt((sdCourse.x)^2 + (sdCourse.y)^2)) %>% 
    dplyr::mutate(meanRateDiff = meanRate.x - meanRate.y) %>% 
    dplyr::mutate(sdRateDiff = sqrt((sdRate.x)^2 + (sdRate.y)^2)) %>% 
    dplyr::mutate(meanElevDiff = meanElev.x - meanElev.y) %>% 
    dplyr::mutate(sdElevDiff = sqrt((sdElev.x)^2 + (sdElev.y)^2)) 
    # reorder summary columns
   if(daily) {
     summary_all %>% 
       dplyr::select(GPS, Date,
                     n.x, n.y, nDiff,
                     meanLat.x, meanLat.y, meanLatDiff,
                     sdLat.x, sdLat.y, sdLatDiff,
                     meanLong.x, meanLong.y, meanLongDiff,
                     sdLong.x, sdLong.y, sdLongDiff,
                     meanDist.x, meanDist.y, meanDistDiff,
                     sdDist.x, sdDist.y, sdDistDiff,
                     meanCourse.x, meanCourse.y, meanCourseDiff,
                     sdCourse.x, sdCourse.y, sdCourseDiff,
                     meanRate.x, meanRate.y, meanRateDiff,
                     sdRate.x, sdRate.y, sdRateDiff,
                     meanElev.x, meanElev.y, meanElevDiff,
                     sdElev.x, sdElev.y, sdElevDiff)
   }
   else {
     summary_all %>% 
     dplyr::select(1,
                   n.x, n.y, nDiff,
                   meanLat.x, meanLat.y, meanLatDiff,
                   sdLat.x, sdLat.y, sdLatDiff,
                   meanLong.x, meanLong.y, meanLongDiff,
                   sdLong.x, sdLong.y, sdLongDiff,
                   meanDist.x, meanDist.y, meanDistDiff,
                   sdDist.x, sdDist.y, sdDistDiff,
                   meanCourse.x, meanCourse.y, meanCourseDiff,
                   sdCourse.x, sdCourse.y, sdCourseDiff,
                   meanRate.x, meanRate.y, meanRateDiff,
                   sdRate.x, sdRate.y, sdRateDiff,
                   meanElev.x, meanElev.y, meanElevDiff,
                   sdElev.x, sdElev.y, sdElevDiff)
   }
}

#'
#'Compares summary statistics from two datasets as side-by-side violin plots
#'
#'@param df_summary data frame of summary statistics from both datasets to be compared
#'@param by GPS or Date
#'@param col_name variable in df_summary to be used for the y-axis, as a string
#'@param out file name to save plot
#'@return side-by-side violin plots
#'@examples
#'# Violin plot comparing unfiltered and filtered demo data summaries by date for a single variable
#'\donttest{
#'\dontrun{
#'## Get elevation
#'elevfile <- system.file("extdata/elev", "USA_msk_alt.zip", package="animaltracker")
#'elev <- read_zip_to_rasters(elevfile)
#'
#'## Get elevation data for unfiltered demo
#'unfiltered_elev <- lookup_elevation(elev, demo_unfiltered, zoom=11, 
#'get_slope=FALSE, get_aspect=FALSE)
#'
#'## Get elevation data for filtered demo
#'filtered_elev <- lookup_elevation(elev, demo_filtered, zoom=11, get_slope=FALSE, get_aspect=FALSE)
#'
#'## Summarise unfiltered demo
#'unfiltered_summary <- summarise_anidf(unfiltered_elev, Date, Latitude, Longitude, 
#'Distance, Course, Rate, Elevation, daily=FALSE)
#'
#'## Summarise filtered demo
#'filtered_summary <- summarise_anidf(filtered_elev, Date, Latitude, Longitude, 
#'Distance, Course, Rate, Elevation, daily=FALSE)
#'
#'## Join
#'summary <- join_summaries(unfiltered_summary, filtered_summary, "Date", daily=FALSE)
#'
#'## Violin plot
#'
#'violin_compare(summary, Date, "meanElev", "ex_elev_violin.png")
#'
#'}
#'}
#'@export
#'
violin_compare <- function(df_summary, by, col_name, out) {
  by <- dplyr::enquo(by)
  
  df_summary <- df_summary %>% 
    dplyr::select(!!by, paste0(col_name, ".x"), paste0(col_name, ".y")) %>% 
    tidyr::gather("source", obs, -!!by) %>% 
    dplyr::mutate(source = gsub(paste0(col_name, "\\."), "", source)) %>% 
    dplyr::mutate(source = gsub("x", "Correct", source)) %>% 
    dplyr::mutate(source = gsub("y", "Candidate", source))
  
  violin <- ggplot(df_summary, aes(x = source, y=obs, fill=source)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    scale_x_discrete(limits=c("Correct", "Candidate")) +
    xlab("Data") +
    ylab(col_name) + 
    theme_minimal() +
    theme(legend.position = "none") 
  
  ggsave(out, violin)
  
  return(violin)
}

#'
#'Compares moving averages of a variable for two datasets over time, grouped by GPS
#'GPS, Date, and col columns should match
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@param col variable to plot the moving average for
#'@param out file name to save plot
#'@return faceted line plot of moving averages over time grouped by GPS
#'@examples
#'# Faceted line plot comparing moving averages over time 
#'# grouped by GPS for unfiltered and filtered demo data
#'\donttest{
#'\dontrun{
#'## Set distance as the y axis
#'line_compare(demo_unfiltered, demo_filtered, Distance, "ex_line_dist.png")
#'}
#'}
#'@export
#'
line_compare <- function(correct, candidate, col, out) {
  col_name <- dplyr::enquo(col)
  
  correct <- correct %>% 
    dplyr::group_by(GPS, Date) %>% 
    dplyr::summarise(avg = mean(!!col_name)) %>% 
    dplyr::mutate(Data = "Correct")
  
  candidate <- candidate %>% 
    dplyr::group_by(GPS, Date) %>% 
    dplyr::summarise(avg = mean(!!col_name)) %>% 
    dplyr::mutate(Data = "Candidate")
  
  plot_data <- dplyr::bind_rows(correct, candidate)
  
  line <- ggplot(plot_data, aes(x=Date, y=avg, group=Data, color=Data)) +
    geom_line() +
    ylab(paste0("Mean ", deparse(substitute(col)))) +
    scale_color_discrete(guide = guide_legend(reverse = T)) +
    facet_wrap(vars(GPS))
  
  ggsave(out, line)
  
  return(line)
}

#'
#'Compares two animal datasets and calculates daily summary statistics by GPS
#'GPS, date, lat, long, course, distance, rate, elevation column names should match. 
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@param out desired file name of .csv output summary 
#'@return summary df
#'@examples
#'# Compare and summarise unfiltered demo cows to filtered, grouped by both Date and GPS
#'\donttest{
#'\dontrun{
#'## Get elevation
#'elevfile <- system.file("extdata/elev", "USA_msk_alt.zip", package="animaltracker")
#'elev <- read_zip_to_rasters( elevfile )
#'
#'## Get elevation data for unfiltered demo
#'unfiltered_elev <- lookup_elevation(elev, demo_unfiltered, zoom=11, 
#'get_slope=FALSE, get_aspect=FALSE)
#'
#'## Get elevation data for filtered demo
#'filtered_elev <- lookup_elevation(elev, demo_filtered, zoom=11, get_slope=FALSE, get_aspect=FALSE)
#'
#'## Compare and summarise
#'compare_summarise_daily(unfiltered_elev, filtered_elev, "ex_compare_daily.csv")
#'}
#'}
#'@export
#'
compare_summarise_daily <- function(correct, candidate, out) {
  correct_summary <- correct %>% 
    summarise_anidf(NULL, Latitude, Longitude, Distance, Course, Rate, Elevation, daily=T)
  
  candidate_summary <- candidate %>% 
    summarise_anidf(NULL, Latitude, Longitude, Distance, Course, Rate, Elevation, daily=T)
  
  summary_all <- join_summaries(correct_summary, candidate_summary, daily=T)
  
  utils::write.csv(summary_all, out, row.names = F)
  
  return(summary_all)
}


#'
#'Joins and reformats two animal data frames for the purpose of flag comparison
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@return joined and reformatted df
#'@export
#'
compare_flags <- function(correct, candidate) {
    correct <- correct %>% dplyr::mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S"))
      # dplyr::mutate(Date = as.Date(DateTime, format="%Y-%m-%d"))
    candidate <- candidate %>% dplyr::mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S"))  
      # dplyr::mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) 
    joined <- dplyr::full_join(correct, candidate, by=c( "DateTime", "GPS")) %>% 
      dplyr::select(DateTime, GPS,
                  Latitude.x, Latitude.y, Longitude.x, Longitude.y,
                  Distance.x, Distance.y, Rate.x, Rate.y,
                  Course.x, Course.y,
                  Elevation.x, Elevation.y, Slope.x, Slope.y,
                  RateFlag, CourseFlag, DistanceFlag, TotalFlags) %>% 
    dplyr::mutate( Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
    tibble::add_column(TimeDiff = NA, .after="DateTime") %>% 
    tibble::add_column(TimeDiffMins = NA, .after="TimeDiff") %>%
    tibble::add_column(cumDist.x=NA, .after="Distance.x") %>% 
    tibble::add_column(cumDist.y=NA, .after="Distance.y") %>%
    dplyr::group_by(GPS, Date) %>% 
    dplyr::arrange(DateTime, .by_group = TRUE) %>% 
    dplyr::mutate(Distance.y = dplyr::lag(Distance.y,1), 
                  Distance.x = ifelse(is.na(Distance.x), 0, Distance.x),
                  Distance.y = ifelse(is.na(Distance.y), 0, Distance.y),
                  
                  cumDist.x = cumsum(Distance.x),
                  cumDist.y = cumsum(Distance.y),
                  
                  TimeDiff = ifelse((is.na(dplyr::lag(DateTime,1)) | as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins")) > 100), 0, as.numeric(DateTime - dplyr::lag(DateTime,1))), 
                  TimeDiffMins = ifelse(TimeDiff == 0, 0, as.numeric(difftime(DateTime, dplyr::lag(DateTime,1), units="mins"))),
                  Dropped.x = ifelse(!is.na(Latitude.x), 0, 1),
                  Dropped.y = ifelse((TotalFlags < 2 & !DistanceFlag), 0, 1)) %>% 
    dplyr::ungroup()
    return(as.data.frame(joined))
}


#'
#'Alternative implementation of the robust peak detection algorithm by van Brakel 2014 
#'Classifies data points with modified z-scores greater than max_score as outliers ccording to Iglewicz and Hoaglin 1993
#'
#'@param df_comparison output of compare_flags 
#'@param lag width of interval to compute rolling median and MAD, defaults to 5
#'@param max_score modified z-score cutoff to classify observations as outliers, defaults to 3.5
#'@return df with classifications
#'@export
#'
detect_peak_modz <- function(df_comparison, lag=5, max_score=3.5) {
  peak_comparison <- df_comparison %>% 
    dplyr::group_by(GPS, Date) %>% 
    dplyr::arrange(DateTime, .by_group = TRUE) %>% 
    dplyr::mutate(
      cumDistLower = zoo::rollmedianr(cumDist.y, lag, fill=NA) - max_score*zoo::rollapplyr(cumDist.y, lag, stats::mad, fill=NA)/0.6745,
      cumDistUpper = zoo::rollmedianr(cumDist.y, lag, fill=NA) + max_score*zoo::rollapplyr(cumDist.y, lag, stats::mad, fill=NA)/0.6745,
      cumDistSignal = ifelse(0.6745*(abs(cumDist.y - zoo::rollmedianr(cumDist.y, lag, fill=NA))/zoo::rollapplyr(cumDist.y, lag, stats::mad, fill=NA)) > max_score, 1, 0),
      RateLower = zoo::rollmedianr(Rate.y, lag, fill=NA) - max_score*zoo::rollapplyr(Rate.y, lag, stats::mad, fill=NA)/0.6745,
      RateUpper = zoo::rollmedianr(Rate.y, lag, fill=NA) + max_score*zoo::rollapplyr(Rate.y, lag, stats::mad, fill=NA)/0.6745,
      RateSignal = ifelse(0.6745*(abs(Rate.y - zoo::rollmedianr(Rate.y, lag, fill=NA))/zoo::rollapplyr(Rate.y, lag, stats::mad, fill=NA)) > max_score, 1, 0)
    ) %>% 
    dplyr::ungroup()
  return(as.data.frame(peak_comparison))
}

#'
#'Train a k-nearest neighbors model on a comparison data frame to classify points that should be dropped
#'
#'@param df_comparison output of compare_flags
#'@param normalize_type how to normalize the data, defaults to standardize
#'@param train proportion of the data used for training
#'@return model and test performance as a list
#'@export
#'
detect_peak_knn <- function(df_comparison, normalize_type = "standardize", train = 2/3) {
  df_imp <- df_comparison %>% 
    ffimp() %>% 
    dplyr::select(time = TimeDiff,
         lat = Latitude.x,
         lon = Longitude.x,
         dist = Distance.y,
         rate = Rate.y,
         course = Course.y,
         elev = Elevation.x,
         slope = Slope.x,
         drop = Dropped.x) %>%
    dplyr::mutate(drop = factor(drop))
  
  df_imp <- normalizeFeatures(df_imp$data, target = "drop", method = normalize_type)
  
  set.seed(0)
  
  train_rows <- sample(1:nrow(df_imp), floor(nrow(df_imp)*train))
  test_rows <- setdiff(1:nrow(df_imp), train_rows)
  
  task_imp <- mlr::makeClassifTask(data = df_imp %>% select(dist, rate, drop), target = "drop", positive = "1")
  
  knn.learner <- mlr::makeLearner("classif.knn", predict.type="response")
  
  fmodel_knn <- mlr::train(knn.learner, task_imp, subset = train_rows)
  
  fpmodel_knn <- mlr::predict(fmodel_knn, task_imp, subset = test_rows)
  
  return(list(fmodel_knn, calculateROCMeasures(fpmodel_knn)))
}


#'
#'Feed-forward imputation on a comparison data frame
#'Helper function for detect_peak_knn
#'
#'@param df_comparison output of compare_flags 
#'@return imputed data frame
#'
ffimp <- function(df_comparison) {
  df_comparison %>% 
    dplyr::group_by(GPS, Date) %>%
    dplyr::mutate(Distance.y = ifelse(is.na(Distance.y), dplyr::lag(Distance.y, 1, dplyr::lead(Distance.y, 1)), Distance.y),
                  Rate.y = ifelse(is.na(Rate.y), dplyr::lag(Rate.y, 1, dplyr::lead(Rate.y, 1)), Rate.y),
                  Course.y = ifelse(is.na(Course.y), dplyr::lag(Course.y, 1, dplyr::lead(Course.y, 1)), Course.y),
                  Elevation.x = ifelse(is.na(Elevation.x), dplyr::lag(Elevation.x, 1, dplyr::lead(Elevation.x, 1)), Elevation.x),
                  Slope.x = ifelse(is.na(Slope.x), dplyr::lag(Slope.x, 1, dplyr::lead(Slope.x, 1)), Slope.x)) %>% 
    dplyr::ungroup()
}
