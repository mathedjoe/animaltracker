elev <- read.csv("R/scratch/nsatellites/sample_arizona_withelev.csv")
flags <- read.csv("R/scratch/nsatellites/sample_arizona_withflags.csv")
library(dplyr)

df <- inner_join(elev, flags) %>% filter(grepl("4/.*/2018", Date) ) 

df %>% 
  select(Index, DateTime, Latitude, Longitude, Altitude, Elevation, 
          nSatellites, DistGeo, RateFlag, DistanceFlag, TotalFlags, HorizPositError = EHPE ) %>%
  write.csv("R/scratch/nsatellites/arizona_data.csv", row.names=F)
