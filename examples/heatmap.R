library(ggplot2)

weekday <- read.csv("weekday.csv")

weekday <- weekday[!(weekday$Weekday==""),]

weekday <- weekday[!(weekday$Notes=="Total"),]
weekday <- weekday[!(weekday$Multiple.Cause.of.death=="Opioids and related analgesics"),]

weekday <- subset(weekday, select=c(Weekday, Multiple.Cause.of.death, Deaths))

weekday$Multiple.Cause.of.death <- reorder(weekday$Multiple.Cause.of.death,weekday$Deaths)

ggplot(data=weekday, mapping=aes(x=weekday$Weekday,y=weekday$Multiple.Cause.of.death,fill=weekday$Deaths)) +
  geom_tile() +
  scale_x_discrete(limits=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  scale_fill_gradient(low="#FFCCFF",high="#660066",name="Deaths (1999-2016)") +
  ylab(label="Multiple Cause of Death") +
  xlab(label="Day of the Week")
