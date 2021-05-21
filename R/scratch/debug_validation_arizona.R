correct <- read.csv("inst/extdata/correct_val.csv", skipNul = TRUE)
candidate <- read.csv("inst/extdata/candidate_val.csv", skipNul = TRUE)

mydf<- compare_flags(correct, candidate)


mydf %>% mutate (hasLat = !is.na(Latitude.x)) %>% group_by(Dropped, hasLat) %>% summarize ( n = n())

# %>% filter (!is.na(Dropped), !Dropped)

df_today <- get_column(mydf, "Cumulative Distance (+Flags)", date = "2018-03-05")


ggplot(data=, 
       aes(x=DateTime, y=VAR, group=Source, color=Source)) +
  geom_line(aes(size = Source)) +
  scale_color_discrete(guide = guide_legend(reverse = T)) +
  scale_size_manual(values=c(2, 1))+
  facet_wrap(vars(GPS), ncol=3) +
  theme_minimal()
