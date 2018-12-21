library(ggplot2)
library(RColorBrewer)

causes <- read.csv("causes.csv")

causes <- causes[!(causes$Notes=="Total"),]

causes <- causes[!(causes$Crude.Rate=="Unreliable"),]

causes <- causes[1:12,]

causes <- subset(causes, select=c("Multiple.Cause.of.death", "Deaths"))

causes$Deaths <- as.numeric(as.character(causes$Deaths))

names(causes) <- c("Multiple Cause of Death", "Number of Deaths")

causes$`Multiple Cause of Death` <- reorder(causes$`Multiple Cause of Death`,causes$`Number of Deaths`)

ggplot(causes, aes(x=causes$`Multiple Cause of Death`,y=causes$`Number of Deaths`)) +
  geom_col(aes(fill=causes$`Number of Deaths`)) +
  scale_fill_gradient2(high="red2",guide = FALSE) +
  xlab("Multiple Cause of Death") +
  ylab("Number of Deaths (1999-2016)") +
  coord_flip()


