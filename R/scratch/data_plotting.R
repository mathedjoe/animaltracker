cowdata <- readRDS("data/animal_data.rds")
cowdata <- dplyr::bind_rows(cowdata, id = NULL)


library(ggplot2)
library(tidyverse)

# Heatmap of Altitude
mybreaks <- list(x = round( seq(min(cowdata$Longitude), max(cowdata$Longitude), length.out = 10 ),3),
              y = round( seq(min(cowdata$Latitude), max(cowdata$Latitude), length.out = 10 ),3))
ggplot(cowdata %>% 
         mutate( LongBin = cut_number(Longitude, 100, 
                                      labels= round( seq(min(Longitude), max(Longitude), length.out = 100 ),3)
                                      ),
                 LatBin = cut_number(Latitude, 100, 
                                     labels=round( seq(min(Latitude), max(Latitude), length.out = 100 ), 3)
                                     )) %>%
         group_by(LongBin, LatBin, Cow) %>%
         summarize(Altitude = median(Altitude)), 
       aes (x = LongBin,  y = LatBin, fill = Altitude))+
  geom_tile()+
  scale_fill_gradientn(colors = c("white", "green", "red"))+
  scale_x_discrete( breaks = mybreaks$x) +
  scale_y_discrete( breaks = mybreaks$y) +
  coord_equal()+
  theme_minimal()

# Heatmap of Time Spent
mybreaks <- list(x = round( seq(min(cowdata$Longitude), max(cowdata$Longitude), length.out = 10 ),3),
                 y = round( seq(min(cowdata$Latitude), max(cowdata$Latitude), length.out = 10 ),3))
ggplot(cowdata %>% 
         mutate( LongBin = cut_number(Longitude, 100, 
                                      labels= round( seq(min(Longitude), max(Longitude), length.out = 100 ),3)
         ),
         LatBin = cut_number(Latitude, 100, 
                             labels=round( seq(min(Latitude), max(Latitude), length.out = 100 ), 3)
         )) %>%
         group_by(LongBin, LatBin, Cow) %>%
         summarize(Npoints = n()), 
       aes (x = LongBin,  y = LatBin, fill = Npoints))+
  geom_tile()+
  scale_fill_gradientn(colors = c("white", "green", "red"))+
  scale_x_discrete( breaks = mybreaks$x) +
  scale_y_discrete( breaks = mybreaks$y) +
  coord_equal()+
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
