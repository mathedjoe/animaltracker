# bring in helper functions
source('R/data_analysis.R')
library(dplyr)
library(ggplot2)

df_correct <- read.csv('inst/extdata/correct.csv', stringsAsFactors = F) %>% 
  dplyr::rename(GPS = collar) %>% 
  dplyr::rename(Date = date) %>% 
  dplyr::mutate(Date = as.Date(Date))

df_candidate <- read.csv('inst/extdata/candidate.csv', stringsAsFactors = F) %>% 
  dplyr::mutate(Date = as.Date(Date))

dfs <- compare_summarise_data(df_correct, 
                       df_candidate, 
                       'inst/extdata/processed/gps_arizona.csv', 
                       'inst/extdata/processed/date_arizona.csv'
                       )

# summary by gps
summary(dfs$GPS)
# close match between lat, lon, course, 
# approx 90-170 less rows (out of ~6400-7500)
# underestimates distance, rate, less variable
# overestimates elevation, less variable

# summary by date
summary(dfs$Date)
# less dates
# close match between lat, lon, course
# underestimates distance, rate, less variable
# overestimates elevation, less variable

# Violin plot comparisons by date

# plot number of observations by date
violin_compare(dfs$Date, Date, "n", "inst/extdata/processed/n_date_arizona.png")

# plot mean distance by date
violin_compare(dfs$Date, Date, "meanDist", "inst/extdata/processed/meanDist_date_arizona.png")
# underestimates distance by ~6m on average
# roughly symmetrical vs. skewed right 

# plot mean elevation by date
violin_compare(dfs$Date, Date, "meanElev", "inst/extdata/processed/meanElev_date_arizona.png")
# overestimates elevation by ~23m on average
# both bimodal ~1690, 1975m vs. ~1680, 1940m

# plot mean rate by date
violin_compare(dfs$Date, Date, "meanRate", "inst/extdata/processed/meanRate_date_arizona.png")
# ~29 less dates on average
# 