heroin_age <- read.csv("heroin_age.csv")

options(scipen=999)

heroin_age <- heroin_age[!(heroin_age$Age=="991 - NEVER USED"),]

heroin_age <- heroin_age[!(heroin_age$Age=="Overall"),]

heroin_age <- subset(heroin_age, select = c(Age,Weighted.Count))

heroin_age$Age <- as.numeric(as.character(heroin_age$Age))

heroin_age$Weighted.Count <- as.numeric(as.character(heroin_age$Weighted.Count))

ggplot(heroin_age, aes(x=heroin_age$Age,y=heroin_age$Weighted.Count)) +
  geom_col(aes(fill=heroin_age$Weighted.Count)) +
  scale_fill_gradient(low="green", high="darkgreen",guide = FALSE) +
  scale_x_continuous(breaks=seq(0,60,5)) +
  xlab("Age When First Tried Heroin") +
  ylab("Estimated Number of Americans") 


