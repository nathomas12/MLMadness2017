#load libraries

library(ggplot2)
library(dplyr)

#load data
seeds <- read.csv("TourneySeeds.csv")
teams <- read.csv("Teams.csv")
seasondata <- read.csv("RegularSeasonDetailedResults.csv")
seasondatacompact<-read.csv("RegularSeasonCompactResults.csv")
tourneydata <- read.csv("TourneyDetailedResults.csv")
tourneydatacompact<-read.csv("TourneyCompactResults.csv")
submission <- read.csv("sample_submission.csv")

# data wrangling for seed distance
seeds$num_seed <- as.numeric(substr(seeds$Seed,2,3))
seeds$region<- as.character(substr(seeds$Seed,1,1))

#data wrangling for team 1 and team 2
tourneydatacompact$team1<- pmin(tourneydatacompact$Wteam,tourneydatacompact$Lteam)
tourneydatacompact$team2<- pmax(tourneydatacompact$Wteam,tourneydatacompact$Lteam)

#build per-game statistics
seasondata$WfgPercent<- seasondata$Wfgm/seasondata$Wfga
seasondata$LfgPercent<- seasondata$Lfgm/seasondata$Lfga
seasondata$WftPercent<- seasondata$Wftm/seasondata$Wfta
seasondata$LftPercent<- seasondata$Lftm/seasondata$Lfta

winnerHistory<-seasondata[,c("Season","Wteam","Wscore","WfgPercent","WftPercent")]
winnerHistory$victory<-1
loserHistory<-seasondata[,c("Season","Lteam","Lscore","LfgPercent","LftPercent")]
loserHistory$victory<-0

names(winnerHistory)<-c("season","team","score","fgp","ftp","victory")
names(loserHistory)<-c("season","team","score","fgp","ftp","victory")
teamHistory <- rbind(winnerHistory,loserHistory)
#create per season, per team stats
averageStats <- aggregate(teamHistory, by=list(teamHistory$season, teamHistory$team), FUN=mean, na.rm=TRUE)



