library(tidyverse)

source("geom_bag.R")

df_demo <- data.frame(
  xpre = c(rnorm(100, 10,4), rnorm(100, -5,2), rnorm(100, -10, 3), rnorm(100, 3,1)),
  ypre = c(rnorm(100, 7,3), rnorm(100, 2,4), rnorm(100, -8, 3), rnorm(100, -5,1))
) %>%
  mutate(
    xpost = xpre + rnorm(400, 3, 1),
    ypost = ypre + rnorm(400, 5, 2),
    grouppre = factor(case_when( xpre > 0 & ypre > 0 ~ "Q1",
                                 xpre < 0 & ypre > 0 ~ "Q2",
                                 xpre < 0 & ypre < 0 ~ "Q3",
                                 xpre > 0 & ypre < 0 ~ "Q4"
    ))
  )

df_centers <- df_demo %>%
  group_by(grouppre) %>%
  summarize( xmean_pre = mean(xpre),
             ymean_pre = mean(ypre),
             xmean_post = mean(xpost),
             ymean_post = mean(ypost))

colors4_custom <- c("#1b9e77", "#d95f02", "#7570b3", "#a6761d")

df_demo %>%
  ggplot(aes(color = grouppre, fill = grouppre)) +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_point( aes(x = xpre, y = ypre), shape = 21, alpha = 0.1) +
  geom_point( aes(x = xpost, y = ypost), shape = 21, alpha = 0.5) +
  geom_bag(aes(x = xpre, y = ypre), prop = 0.9, alpha = 0.1, size = 1) + # enclose PRE points
  geom_bag(aes(x = xpost, y = ypost), prop = 0.9, alpha = .4, size = 2) + # enclose POST points
  geom_point(data = df_centers,  aes(x = xmean_pre, y = ymean_pre), size =3)+
  geom_point(data = df_centers,  aes(x = xmean_post, y = ymean_post), size =3)+
  geom_segment(data = df_centers,
               aes(x = xmean_pre, y = ymean_pre, xend = xmean_post, yend = ymean_post),
               arrow = arrow(), size = 1.5, color = "black")+
  scale_discrete_manual( aesthetics = c("fill", "color"), values = colors4_custom) +
  coord_equal()+
  theme_minimal() +
  labs(x = "xvals", y= "yvals")+
  theme(legend.position = "none")

# TODO: Visualize cow data with geom_bag
# TODO: Determine whether a point is in the convex hull or not
# TODO: Compare outliers from this method to app

test_ani_keep <- test_ani %>% group_by(Date) %>% 
  summarise(prop_keep = sum(Keep ==1)/n()) %>%
  arrange(prop_keep) 

test_ani_day <- test_ani %>% 
  filter(Date == test_ani_keep$Date[1],
         Longitude < 0)

test_ani_sample <- test_ani %>% 
  filter(Date %in% test_ani_keep$Date[1:4])

p <- test_ani_day %>% 
  ggplot(aes(x = Longitude, y = Latitude)) +
  geom_point() + 
  geom_bag(prop = 0.7, alpha = 0.3) +
  #lims(x = c(-110, -100), y = c(30, 33)) + 
  coord_equal() +
  theme_minimal()

polygon <- ggplot_build(p)$data[[2]] %>% select(x, y)

pts_in_poly <- point.in.polygon(test_ani_day$Longitude, test_ani_day$Latitude, polygon$x, polygon$y) > 0 

p + 
  geom_point(aes(color = pts_in_poly))
  