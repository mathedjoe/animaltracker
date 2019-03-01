# install.packages("GSODR")
library(GSODR)

mystations <- nearest_stations(-27.56, 151.95, 100)

length(mystations) # should be 17

sample(x = c(0,1), size = 100, prob=c(.88, .12), replace=TRUE)

# create a vector of 0s
x<- rep(0,6322)

# replace every nth 0 with a 1, where n = .12*length(x)
x [ seq(1, length(x), round(.12*length(x)))] <- 1