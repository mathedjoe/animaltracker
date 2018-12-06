cows <- readRDS("cow_data.rds")
# merge cow data into one data frame

cdata <- bind_rows(cows) 


# summarize by GPS unit

cdata %>% 
  group_by(GPS) %>%
  summarize( ncows = length(unique(Cow)),
             meanAlt = mean(Altitude),
             sdAlt = sd(Altitude),
             minAlt = min(Altitude),
             maxAlt = max(Altitude))

# overall boxplot of Altitude by GPS
ggplot(cdata, aes(x=GPS, y=Altitude))+
  geom_boxplot()+
  theme_minimal()

# summarize time between GPS measurements

ggplot(cdata, aes(x=TimeDiffMins))+
  geom_histogram( col= "white") + 
  ggtitle("Distribution of Time Between GPS Measurements" )+ 
  theme_minimal()


ggplot(cdata, aes(x=TimeDiffMins))+
  geom_histogram( col= "white") + 
  facet_wrap(~GPS)+
  ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+ 
  theme_minimal()

ggplot(cdata, aes(x=GPS, y=TimeDiffMins))+
  geom_boxplot() + 
  coord_flip()+
  ggtitle("Distribution of Time Between GPS Measurements by GPS Unit" )+ 
  theme_minimal()

## QQ PLOT to show distribution of Time between GPS Measurements 
ggplot(cdata, aes(sample = TimeDiffMins)) +
  stat_qq() 

quantile(cdata$TimeDiffMins, probs = seq(0,1,.05))
