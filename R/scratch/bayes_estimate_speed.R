library(dplyr)
library(animaltracker)

test_ani <- read.csv("Riggs_March19_79.csv", skipNul = TRUE) %>% 
  clean_location_data(dtype = "igotu", filters = FALSE, aniid = 79)

bayes_estimate <- data.frame()

dates <- unique(test_ani$Date)
n <- 0
p <- list()

for(date in dates) {
  date_i <- test_ani %>% filter(Date == date)
  n <- n + nrow(date_i)
  n_a <- c()
  for(a in 0:99) {
    interval_i <- date_i %>% filter(Rate < a+1 && Rate >= a)
    n_a <- c(n_a, nrow(interval_i))
  }
  print(n_a)
  p[date] <- n_a/n
}
