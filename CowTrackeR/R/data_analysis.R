#'
#' This generates a statistical summary of the GPS unit
#' Summarize by GPS unit
#'
#' @param cow_data_.rdsfile gives the path to the file (cow data) that is read in
#' @return generates the statistical summary for cows
#'

summarize_GPS_unit <- function(cow_data_.rdsfile){
  cows<- readRDS(cow_data_.rdsfile)
  cdata <- bind_rows(cows)
  statistical_summary <- cdata %>%
    group_by(GPS) %>%
    summarize( ncows = length(unique(cow)),
               meanAlt = mean(Altitude),
               sdAlt = sd(Altitude),
               minAlt = min(Altitude),
               maxAlt = max(Altitude))
  return(statistical_summary)
}


#'
#'
#' Generating Boxplot of the Altitude by GPS
#' @param cow_data_.rdsfile reads in the cow data
#' @return a boxplot is generated that visualizes altitude by GPS
#'

Altitude_by_GPS <- function(cow_data_.rdsfile){
  cows <- readRDS(cow_data_.rdsfile)
  cdata <- bind_rows(cows)
  boxplot <- ggplot(cdata, aes(x=GPS, y=Altitude))+
    geom_boxplot()+
    theme_minimal()
  return(boxplot)
}

#'
#'
#'Generate a plot to summarize time between GPS measurements
#'@param cow_data_.rdsfile reads in cow data
#'@return generates a histogram to show time between GPS measurements
#'

distribution_of_time <- function(cow_data_.rdsfile){
  cows <- readRDS(cow_data_.rdsfile)
  cdata <- bind_rows(cows)
  histogram <- ggplot(cdata, aes(x=TimeDiffMins))+
    geom_histogram( col= "white") +
    ggtitle("Distribution of Time Between GPS Measurements" )+
    theme_minimal()
  return(histogram)
}

#'
#'
#'Generate a plot to visualize time between GPS measurements by GPS unit
#'@param cow_data_.rdsfile
#'@return generates a histogram visualizing ditribution of time by GPS unit
#'

Time_by_GPS_unit <- function(cow_data_.rdsfile){
  cows <- readRDS(cow_data_.rdsfile)
  cdata <- bind_rows(cows)
  histogram <- ggplot(cdata, aes(x=TimeDiffMins))+
    geom_histogram( col= "white") +
    facet_wrap(~GPS)+
    ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+
    theme_minimal()
  return(histogram)
}

#'
#'
#'Generates a boxplot to visualize the Time between GPS measurements by GPS Unit
#'@param cow_data_.rdsfile reads in the cow data file
#'@return generates a boxplot
#'

Time_GPS_unit_boxplot <- function (cow_data_.rdsfile){
  cows <- readRDS(cow_data_.rdsfile)
  cdata <- bind_rows(cows)
  boxplot <- ggplot(cdata, aes(x=GPS, y=TimeDiffMins))+
    geom_boxplot() +
    coord_flip()+
    ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+
    theme_minimal()
  return(boxplot)
}


#'
#'
#'QQ PLOT to show distribution of Time between GPS Measurements
#'@param cow_data_.rdsfile reads in the cow data
#'@return generates a quantile - quantile plot that gives the two probability distribution
#'

qq_plot <- function (cow_data_.rdsfile){
  cows <- readRDS(cow_data_.rdsfile)
  cdata <- bind_rows(cows)
  qq <- ggplot(cdata, aes(sample = TimeDiffMins)) +
    stat_qq()

  quantile(cdata$TimeDiffMins, probs = seq(0,1,.05))
  return(qq)
}

