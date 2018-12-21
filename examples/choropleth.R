library(ggplot2)
library(mapproj)

state_raw <- read.delim("state.txt")

write.csv(x=state_raw, file="state.csv")

states <- map_data("state")

opioid_deaths <- read.csv("state.csv")

opioid_deaths <- opioid_deaths[opioid_deaths$Notes=="Total",]

opioid_deaths <-subset(opioid_deaths, select=c(State, Crude.Rate))

opioid_deaths  <- opioid_deaths[!(opioid_deaths$Crude.Rate=="Unreliable"),]

write.csv(opioid_deaths, "test.csv")
opioid_deaths$Crude.Rate <- as.numeric(as.character(opioid_deaths$Crude.Rate))

names(opioid_deaths) <- c("region", "Opioid-Related Deaths per 100,000")

opioid_deaths$region <- tolower(opioid_deaths$region)

mapped_data <- merge(states, opioid_deaths, sort = TRUE, by = "region", all=TRUE)

ggplot(mapped_data, aes(long, lat)) +
  geom_polygon(color="white",aes(group=group, fill=mapped_data$`Opioid-Related Deaths per 100,000`))  +
  scale_fill_continuous(name='Opioid-Related Deaths per 100,000', low="thistle2", high="darkred") +
  theme_void()

write.csv(opioid_deaths, "totals.csv")

