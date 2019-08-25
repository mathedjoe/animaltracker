
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
#'Compares two animal datasets and calculates summary statistics. 
#'GPS, date, lat, long, course, distance, rate, elevation column names should match. 
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@param gps_out desired file name of .csv output summary by GPS collar
#'@param date_out desired file name of .csv output summary by date
#'@return list containing gps_out and date_out as dfs
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
  
  gps_summary <- join_summaries(correct_gps_summary, candidate_gps_summary, by="GPS")
  date_summary <- join_summaries(correct_date_summary, candidate_date_summary, by="Date")
  
  write.csv(gps_summary, gps_out, row.names = F)
  write.csv(date_summary, date_out, row.names = F)
  
  return(list(GPS = gps_summary, Date = date_summary))
}

#'
#'Helper function for compare_summarise_data and compare_summarise_daily
#'Calculates summary statistics for an animal dataset
#'
#'@param anidf the animal dataset
#'@param by column to group by, null if daily=T
#'@param lat latitude column
#'@param long longitude column
#'@param dist distance column
#'@param course course column
#'@param rate rate column
#'@param elev elevation column
#'@param daily whether to group by both GPS and Date for daily summary, defaults to False
#'
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
#'@param by column to join by , null if daily=T
#'@param daily whether to group by both GPS and Date for daily summary, defaults to False
#'
join_summaries <- function(correct_summary, candidate_summary, by, daily=F) {
  if(daily) {
    summary_all <- dplyr::full_join(correct_summary, candidate_summary, by=c("GPS", "Date"))
  }
  else {
    summary_all <- dplyr::full_join(correct_summary, candidate_summary, by=by)
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
#'Compares two animal datasets and calculates daily summary statistics. 
#'GPS, date, lat, long, course, distance, rate, elevation column names should match. 
#'
#'@param correct reference df
#'@param candidate df to be compared to the reference
#'@param gps_out desired file name of .csv output summary 
#'@return summary df
#'@export
#'
compare_summarise_daily <- function(correct, candidate, out) {
  correct_summary <- correct %>% 
    summarise_anidf(NULL, Latitude, Longitude, Distance, Course, Rate, Elevation, daily=T)
  
  candidate_summary <- candidate %>% 
    summarise_anidf(NULL, Latitude, Longitude, Distance, Course, Rate, Elevation, daily=T)
  
  summary_all <- join_summaries(correct_summary, candidate_summary, daily=T)
  
  write.csv(summary_all, out, row.names = F)
  
  return(summary_all)
}
