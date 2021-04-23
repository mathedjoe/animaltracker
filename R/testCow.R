
library(ggplot2)
library(gganimate)


testData <- read.csv("DW_063_55_6_18First.csv")
head(testData)

min(testData$Latitude)
max(testData$Latitude)

min(testData$Longitude)
max(testData$Longitude)




#dateConvert <- as.Date(testData$Time)
#dateConvert <- testData$Time

#df1$state_and_code = paste(df1$State,df1$State_code)

DateandTimeString = paste(testData$Date, testData$Time)

DateandTimeString

#as.POSIXct(DateandTimeString,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone())
DateandTimeFormat <- as.POSIXct(DateandTimeString,format="%Y/%m/%d %H:%M:%S",tz=Sys.timezone())

dataGraph <- ggplot(testData, aes(y=Latitude,x=Longitude))+ geom_point()+ transition_time(DateandTimeFormat) + ylim(31,35) + xlim(-113,-106)+ease_aes('linear')+ labs(title = "Time: {frame_time}")+shadow_wake(wake_length = 0.1, alpha = FALSE)

animate(dataGraph,fps = 10,duration = 30)




