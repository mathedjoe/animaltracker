
### Summary Functions

#'
#'Summarize by GPS unit
#'
#'@param rds_path Path of .rds cow data file to read in
#'@return summary statistics for animals by GPS unit
#'@export
#'
summarize_unit <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
  summary <- anidata %>% 
    group_by(GPS) %>%
    summarize( nani = length(unique(Animal)),
               meanAlt = mean(Altitude),
               sdAlt = sd(Altitude),
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
summarize_col <- function(df, col) {
  summary <- df %>%
    dplyr::group_by(Animal) %>%
    dplyr::summarise(
      N = n(),
      Mean = mean(!! sym(col)),
      Median = median(!! sym(col)),
      SD = sd(!! sym(col)),
      Variance = var(!! sym(col)),
      Q1 = quantile(!! sym(col), 0.25),
      Q3 = quantile(!! sym(col), 0.75),
      IQR = IQR(!! sym(col)),
      Range = (max(!! sym(col))-min(!! sym(col))),
      Min = min(!! sym(col)),
      Max = max(!! sym(col))
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
#'@export
#'
boxplot_altitude <- function(rds_path) {
  ani <- readRDS(rds_path)
  anidata <- bind_rows(ani) 
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
#'Compares two animal datasets and calculates summary statistics 
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@param gps_out desired file name of .csv output summary by GPS collar
#'@param date_out desired file name of .csv output summary by date
#'@export
#'
compare_summarise_data <- function(correct, candidate, gps_out, date_out) {
  
  correct_gps_summary <- correct %>% 
    dplyr::rename(GPS = collar) %>%
    summarise_anidf(GPS, Latitude, Longitude, distancetr, Course, RATE, Elevation)
  
  candidate_gps_summary <- candidate %>% 
    summarise_anidf(GPS, Latitude, Longitude, Distance, Course, Rate, Elevation)
  
  correct_date_summary <- correct %>% 
    dplyr::rename(Date = date) %>% 
    dplyr::mutate(Date = as.Date(Date)) %>% 
    summarise_anidf(Date, Latitude, Longitude, distancetr, Course, RATE, Elevation)
  
  candidate_date_summary <- candidate %>% 
    summarise_anidf(Date, Latitude, Longitude, Distance, Course, Rate, Elevation)
  
  gps_summary <- join_summaries(correct_gps_summary, candidate_gps_summary, by="GPS")
  date_summary <- join_summaries(correct_date_summary, candidate_date_summary, by="Date")
  
  write.csv(gps_summary, gps_out, row.names = F)
  write.csv(date_summary, date_out, row.names = F)
}

#'
#'Helper function for compare_summarise_data
#'Calculates summary statistics for an animal dataset
#'
#'@param anidf the animal dataset
#'@param by column to group by
#'@param lat latitude column
#'@param long longitude column
#'@param dist distance column
#'@param course course column
#'@param rate rate column
#'@param elev elevation column
#'
#'
summarise_anidf <- function(anidf, by, lat, long, dist, course, rate, elev) {
  by <- dplyr::enquo(by)
  lat <- dplyr::enquo(lat)
  long <- dplyr::enquo(long)
  dist <- dplyr::enquo(dist)
  course <- dplyr::enquo(course)
  rate <- dplyr::enquo(rate)
  elev <- dplyr::enquo(elev)
  anidf %>% 
    dplyr::group_by(!!by) %>% 
    dplyr::summarise(n = n(),
                     meanLat = mean(!!lat),
                     sdLat = sd(!!lat),
                     meanLong = mean(!!long),
                     sdLong = sd(!!long),
                     meanDist = mean(!!dist),
                     sdDist = sd(!!dist),
                     meanCourse = mean(!!course),
                     sdCourse = sd(!!course),
                     meanRate = mean(!!rate),
                     sdRate = sd(!!rate),
                     meanElev = mean(!!elev),
                     sdElev = sd(!!elev))
}

#'
#'Helper function for compare_summarise_data
#'Joins two animal dataset summaries by a column and appends differences
#'
#'@param correct_summary summary df of reference dataset, returned by summarise_anidf
#'@param candidate_summary summary df of dataset to be compared to reference, returned by summarise_anidf
#'@param by column to join by 
#'
join_summaries <- function(correct_summary, candidate_summary, by) {
  dplyr::full_join(correct_summary, candidate_summary, by=by) %>% 
    # create difference columns
    dplyr::mutate(nDiff = n.x - n.y) %>% 
    dplyr::mutate(meanLatDiff = meanLat.x - meanLat.y) %>% 
    dplyr::mutate(sdLatDiff = sdLat.x - sdLat.y) %>% 
    dplyr::mutate(meanLongDiff = meanLong.x - meanLong.y) %>% 
    dplyr::mutate(sdLongDiff = sdLong.x - sdLong.y) %>% 
    dplyr::mutate(meanDistDiff = meanDist.x - meanDist.y) %>% 
    dplyr::mutate(sdDistDiff = sdDist.x - sdDist.y) %>% 
    dplyr::mutate(meanCourseDiff = meanCourse.x - meanCourse.y) %>% 
    dplyr::mutate(sdCourseDiff = sdCourse.x - sdCourse.y) %>% 
    dplyr::mutate(meanRateDiff = meanRate.x - meanRate.y) %>% 
    dplyr::mutate(sdRateDiff = sdRate.x - sdRate.y) %>% 
    dplyr::mutate(meanElevDiff = meanElev.x - meanElev.y) %>% 
    dplyr::mutate(sdElevDiff = sdElev.x - sdElev.y) %>% 
    # reorder summary columns
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

# 127590, 130036

violin_gps_compare <- function(correct, candidate, col) {
  col <- dplyr::enquo(col)
  correct <- correct %>% 
    dplyr::mutate(GPS = as.factor(GPS)) %>% 
    dplyr::mutate(Data = "Correct") %>% 
    dplyr::select(GPS, !!col, Data)
  candidate <- candidate %>% 
    dplyr::mutate(GPS = as.factor(GPS)) %>% 
    dplyr::mutate(Data = "Candidate") %>% 
    dplyr::select(GPS, !!col, Data)
  plot_data <- dplyr::bind_rows(correct, candidate)
  ggplot(plot_data, aes(x=GPS, y=!!col, fill=Data)) +
    geom_boxplot()
}

line_compare_dev <- function(correct, candidate, col) {
  col_name <- dplyr::enquo(col)
  correct <- correct %>% 
    dplyr::rename(GPS = collar) %>% 
    dplyr::mutate(Date = as.Date(date)) %>% 
    dplyr::mutate(Data = "Correct") %>% 
    dplyr::group_by(GPS, Date) %>% 
    dplyr::mutate(avg = mean(!!col_name)) %>% 
    dplyr::select(GPS, Date, Data, avg) %>% 
    dplyr::distinct()
  candidate <- candidate %>% 
    dplyr::mutate(Data = "Candidate") %>% 
    dplyr::group_by(GPS, Date) %>% 
    dplyr::mutate(avg = mean(!!col_name)) %>% 
    dplyr::select(GPS, Date, Data, avg) %>% 
    dplyr::distinct()
  plot_data <- dplyr::bind_rows(correct, candidate)
  ggplot(plot_data, aes(x=Date, y=avg, group=Data, color=Data)) +
    geom_line() +
    ylab(paste0("Mean ", deparse(substitute(col)))) +
    facet_wrap(vars(GPS))
}

line_compare <- function(correct, candidate, col) {
  col_name <- dplyr::enquo(col)
  correct <- correct %>% 
    dplyr::mutate(Date = as.Date(date)) %>%
    dplyr::group_by(Date) %>% 
    dplyr::summarise(avg = mean(!!col_name)) %>% 
    dplyr::mutate(Data = "Correct") 
  candidate <- candidate %>% 
    dplyr::group_by(Date) %>% 
    dplyr::summarise(avg = mean(!!col_name)) %>% 
    dplyr::mutate(Data = "Candidate")
  plot_data <- dplyr::bind_rows(correct, candidate)
  ggplot(plot_data, aes(x=Date, y=avg, group=Data, color=Data)) +
    geom_line() +
    ylab(paste0("Mean ", deparse(substitute(col)))) 
}
