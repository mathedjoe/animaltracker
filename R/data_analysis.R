
### Summary Functions

#'
#'Summarize by GPS unit
#'
#'@param rds_path Path of .rds cow data file to read in
#'@return summary statistics for cows by GPS unit
#'
summarize_unit <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  summary <- cdata %>% 
    group_by(GPS) %>%
    summarize( ncows = length(unique(Cow)),
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
#'@param rds_path Path of .rds cow data file to read in
#'@return approximate time difference values corresponding to quantiles (.05 intervals)
#'
quantile_time <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  quantile <- quantile(cdata$TimeDiffMins, probs = seq(0,1,.05))
  return(quantile)
}

### Plotting Functions

#'
#'Generates a boxplot to visualize the distribution of altitude
#'by GPS.
#'
#'@param rds_path Path of .rds cow data file to read in
#'@return overall boxplot of altitude by GPS
#'
boxplot_altitude <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  plot <- ggplot(cdata, aes(x=GPS, y=Altitude))+
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
#'
histogram_time <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  plot <- ggplot(cdata, aes(x=TimeDiffMins))+
    geom_histogram( col= "white") + 
    ggtitle("Distribution of Time Between GPS Measurements" )+ 
    theme_minimal()
  return(plot)
}

#'
#'Generates a histogram to visualize the distribution of time between
#'GPS measurements by GPS unit.
#'
#'@param rds_path Path of .rds cow data file to read in
#'@return distribution of time between GPS measurements by GPS unit, as a histogram
#'
histogram_time_unit <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  plot <- ggplot(cdata, aes(x=TimeDiffMins))+
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
#'@param rds_path Path of .rds cow data file to read in
#'@return distribution of time between GPS measurements by GPS unit, as a boxplot
#'
boxplot_time_unit <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  plot <- ggplot(cdata, aes(x=GPS, y=TimeDiffMins))+
    geom_boxplot() + 
    coord_flip()+
    ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+ 
    theme_minimal()
  return(plot)
}

#'
#'Generates a QQ plot to show the distribution of time between GPS measurements.
#' 
#'@param rds_path Path of .rds cow data file to read in
#'@return quantile-quantile plot to show distribution of time between GPS measurements
#'    
qqplot_time <- function(rds_path) {
  cows <- readRDS(rds_path)
  cdata <- bind_rows(cows) 
  plot <- ggplot(cdata, aes(sample = TimeDiffMins)) +
  stat_qq()
  return(plot)
}

