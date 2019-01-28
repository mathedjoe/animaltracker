cowdata <- readRDS("data/animal_data.rds")
cowdata <- dplyr::bind_rows(cowdata, id = NULL)


library(ggplot2)
library(tidyverse)

# Heatmaps
ggplot(cowdata, 
       aes (x = Longitude,  y = Latitude, fill = Altitude))+
  geom_tile()+
  scale_fill_gradientn(colors = c("white", "green", "red"))+
  theme_minimal()


# Box plots
ggplot(cowdata %>% 
         mutate(Cow = fct_reorder(Cow, Altitude, .desc=TRUE)), 
       aes(x = Cow,  y = Altitude, fill= GPS)) + 
  geom_boxplot() +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin plots
ggplot(cowdata %>% 
         filter(Rate < 3, Cow!="9959"), 
       aes(x = Cow, y = Rate, fill = GPS)) +
  geom_violin( ) + 
  geom_boxplot(width=.2, outlier.color = NA) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
