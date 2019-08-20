# bring in helper functions
source('R/data_analysis.R')
library(dplyr)

df_correct <- read.csv('inst/extdata/correct.csv', stringsAsFactors = F)
df_candidate <- read.csv('inst/extdata/candidate.csv', stringsAsFactors = F)

dfs<- compare_summarise_data(df_correct, 
                       df_candidate, 
                       'inst/extdata/processed/gps_arizona.csv', 
                       'inst/extdata/processed/date_arizona.csv'
                       )

# summary by gps
summary(dfs$gps)
# close match between lat, lon, course, 
# approx 90-170 less rows (out of ~6400-7500)
# underestimates distance, rate, less variable
# overestimates elevation, less variable

# summary by date
summary(dfs$date)
# less dates
# close math between lat, lon, course
# underestimates distance, rate, less variable
# overestimates elevation, less variable


plotdata <- dfs$date %>% 
  dplyr::select(Date, n.x, n.y) %>%
  tidyr::gather("source","n", -Date)
require(ggplot2)

ggplot(plotdata, aes(x = source, y=n )) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_minimal()

ggplot(dfs$date %>% 
         dplyr::select(Date, meanDist.x, meanDist.y) %>%
         tidyr::gather("source","meanDist", -Date) %>%
         dplyr::mutate(source = gsub("meanDist\\.","", source)),
       aes(x = source, y=meanDist )) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_minimal()

line_compare_dev(df_correct %>% dplyr::rename(Date = date), df_candidate, "Distance")
