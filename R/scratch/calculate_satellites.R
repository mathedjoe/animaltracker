library(dplyr)
library(animaltracker)

df <- demo %>% 
  mutate(nSatelites = nchar(Satelite) - nchar( gsub("X", "", Satelite)))
  