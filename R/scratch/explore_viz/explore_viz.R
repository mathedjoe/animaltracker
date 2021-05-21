## Exploratory analysis of processed GPS data
# 2020-01-20
# by joechampion@boisestate.edu

## load packages
library(dplyr)
library(ggplot2)

## read data
setwd("R/scratch/explore_viz") # set working directory for this session

data_file <- list.files(pattern="*.csv") # just one csv file in this folder
data_raw <- read.csv(data_file)


## select and prepare  data

df <- data_raw %>% 
  mutate_if(is.factor, as.character) %>% # convert factors to character
  mutate(nSat = nchar(Satelite) - nchar( gsub("X", "", Satelite))) %>% # compute n satellites
  select(LocationID, DateTime, TimeDiffMins,
         Latitude, Longitude, EHPE, nSat, Speed, Rate, 
         Distance, DistGeo, Altitude, Elevation, Slope, Aspect
  ) %>%
  mutate(
    nSatBin = cut(nSat, breaks = c(0, 4:7, Inf), right=FALSE, labels = c(0, 4:6, "7+")),
    DateTime = as.POSIXct(DateTime),
    RateGeo = DistGeo/TimeDiffMins
  ) %>% 
  filter( abs(1 - Distance/DistGeo) < .1 ) # drop any data with more than 10% distance error


############
## question: is there a relationship between EHPE and nSat?

# statistical summary
df %>% 
  group_by(nSatBin) %>% 
  summarize( count = n(), 
             mean = mean(EHPE),
             sd = sd(EHPE),
             min = min(EHPE),
             max = max(EHPE)
             )

# plot
df %>% 
  ggplot(aes(factor(nSatBin), EHPE)) +
  geom_violin( scale = "width") + 
  geom_jitter(height = 0, width=0.1) + # display data points with some scattering
  theme_minimal()

############
## question: is there a relationship between Altitude error and nSat?

# statistical summary
df %>% 
  group_by(nSatBin) %>% 
  summarize( count = n(), 
             mean = mean(Elevation - Altitude),
             sd = sd(Elevation - Altitude),
             min = min(Elevation - Altitude),
             max = max(Elevation - Altitude)
  )

# plot
df %>% 
  ggplot(aes(factor(nSatBin), Elevation - Altitude)) +
  geom_violin(scale = "width" ) + 
  geom_jitter(height = 0, width=0.1) + # display data points with some scattering
  theme_minimal()

############
## question: is there a relationship between Slope and Rate?
# slope = physical steepness of the terrain 
# rate = rate of travel of the animal

# time series plot
df %>% 
  ggplot(aes(x = DateTime)) +
  geom_line(aes(y = Slope), color="blue") + 
  geom_smooth(aes(y = Slope), color="blue4") +
  geom_line(aes(y = Rate), color = "red") + 
  geom_smooth(aes(y = Rate), color = "red4") +
  ylab("Slope (blue) and Rate (red)")+
  theme_minimal()

# basic scatter plot
df %>% 
  ggplot(aes(Slope, Rate)) +
  geom_point() +
  theme_minimal()

# 2d histogram (density plot)
df %>% 
  ggplot( aes(x=Slope, y=Rate) ) +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()

