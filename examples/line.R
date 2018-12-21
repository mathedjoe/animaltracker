library(ggplot2)

year_state_full <- year_state_full[year_state_full$Notes=="Total",]

year_state_full <- subset(year_state_full, select=c(Year,State,Crude.Rate))

state_filter <- function(state) {
  state_df <- year_state_full[year_state_full$State==state,]
  state_df <- subset(state_df, select=c(Year, Crude.Rate))
  state_df$Crude.Rate <- as.numeric(as.character(state_df$Crude.Rate))
  state_df$Year <- as.numeric(as.character(state_df$Year))
  return(state_df)
}

national <- state_filter("")
national <- national[1:18,]

utah <- state_filter("Utah")
wv <- state_filter("West Virginia")
nevada <- state_filter("Nevada")
nm <- state_filter("New Mexico")
ri <- state_filter("Rhode Island")
maryland <- state_filter("Maryland")
kentucky <- state_filter("Kentucky")
ohio <- state_filter("Ohio")
mass <- state_filter("Massachusetts")
dc <- state_filter("District of Columbia")
delaware <- state_filter("Delaware")
washington <- state_filter("Washington")
oklahoma <- state_filter("Oklahoma")
nh <- state_filter("New Hampshire")
alaska <- state_filter("Alaska")
oregon <- state_filter("Oregon")

ggplot() +
  geom_line(data=national,aes(x=national$Year, y=national$Crude.Rate,color="National") ) +
  geom_line(data=wv, aes(x=wv$Year, y=wv$Crude.Rate,color="West Virginia")) +
  geom_line(data=maryland, aes(x=maryland$Year, y=maryland$Crude.Rate,color="Maryland")) +
  geom_line(data=ohio, aes(x=ohio$Year, y=ohio$Crude.Rate,color="Ohio")) +
  geom_line(data=mass, aes(x=mass$Year, y=mass$Crude.Rate,color="Massachusetts")) +
  geom_line(data=dc, aes(x=dc$Year, y=dc$Crude.Rate,color="Washington D.C.")) +
  scale_color_discrete(breaks=c("West Virginia", "Maryland", "Washington D.C.", "Ohio", "Massachusetts", "National")) +
  labs(color="State") +
  xlab('Year') +
  ylab('Opioid-Related Deaths per 100,000')
  

