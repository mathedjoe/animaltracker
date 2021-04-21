library(ggplot2)
library(KFKSDS)
library(stsm)
library(timeSeries)
library(FKF)



cattle.data <- read.csv(file = "test2.csv")

ggplot(cattle.data, aes(y = Latitude, x = Longitude)) + geom_line() +
  xlim(-116, -117) + ylim(43, 44)

latVec <- cattle.data[["Latitude"]]  # by column name
longVec <- cattle.data[["Longitude"]]  # by column name
timeVec <- cattle.data[["Time"]]

#c() Combine Values Into A Vector Or List
#y is the data set
data_1 <- data.frame(latVec,longVec, timeVec)     # Create example data fra
data_2 <-ts(data_1)

m <- stsm::stsm.model(model = "llm+seas", y = data_2,
                      pars = c("var1" = 2, "var2" = 15, "var3" = 30))
ss <- stsm::char2numeric(m)
KF(latVec, ss, convergence = c(0.001, length(latVec)), t0 = 1)