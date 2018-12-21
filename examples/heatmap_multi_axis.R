place <- read.csv("place.csv")

place <- place[!(place$Notes=="Total"),]

place <- subset(place, select=c("Place.of.Death","Multiple.Cause.of.death","Deaths"))

place <- place[1:48,]

place$Place.of.Death <- reorder(place$Place.of.Death,place$Deaths)

ggplot(data=place, mapping=aes(x=place$Place.of.Death,y=place$Multiple.Cause.of.death,fill=place$Deaths)) +
  geom_tile() +
  scale_fill_gradient(low="whitesmoke",high="violetred4",name="Deaths (1999-2016)",breaks=seq(0,100000,20000)) +
  ylab(label="Multiple Cause of Death") +
  xlab(label="Place of Death") +
  scale_x_discrete(position="top") +
  theme(axis.text.x=element_text(angle = 45, hjust = 0))

write.csv(place,"place.csv")
