##libraries##
library(ggplot2)
library(plyr)
library(dplyr)


##load data##
teams <- read.csv('Teams.csv')
seasons <- read.csv('Seasons.csv')
rsdr <- read.csv('RegularSeasonDetailedResults.csv')
tdr <- read.csv('TourneyDetailedResults.csv')
seeds <- read.csv('TourneySeeds.csv')
slots <- read.csv('TourneySlots.csv')

# % of wins by H/A/N
#get frequency
rs_loc_freq <- as.data.frame(count(rsdr$Wloc))
#get percentage
rs_loc_freq['percentage'] <- 100/sum(rs_loc_freq$freq)*rs_loc_freq$freq
rs_loc_freq['percentage_r'] <- signif(rs_loc_freq$percentage,digits=3)

#calculate center of each segment for label
rs_loc_freq['pos'] <- cumsum(rs_loc_freq$percentage) - rs_loc_freq$percentage/2
#set levels for segments for labeling
rs_loc_freq$x <- factor(rs_loc_freq$x, levels=rev(rs_loc_freq$x))

#build pie chart
p<- ggplot(rs_loc_freq, aes(x="", y=percentage_r, fill=x)) + geom_bar(stat="identity")
pie <- p+coord_polar("y") + 
  ggtitle("Season: Percentage of games won at each location") + 
  scale_fill_brewer("Blues") +
  guides(fill=guide_legend(title='Location')) + 
  geom_text(aes(y=pos, label=percentage_r), size=4)

pie

  
