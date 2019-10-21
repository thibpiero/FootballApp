rm(list=ls())

library(tidyverse)
library(DataExplorer)

dffoot <- read.csv("~/Desktop/RTHIB/FootballApp/Shiny Foot/bundesliga-20132017-all-team-stats/Major_European_Leagues_Stats_Std_2013_17.csv", comment.char="#")

DataExplorer::introduce(dffoot)

plot_bar(dffoot)
plot_histogram(dffoot)

dffoot$Assists = as.integer(dffoot$Assists)

str(dffoot)

Teamstat <- dffoot %>%
  group_by(Team)%>%
  summarise(teamgoals = sum(Goals, na.rm = TRUE), teamassists = sum(Assists, na.rm = TRUE), teamredcards = sum(Red_Cards, na.rm = TRUE),
            teamyellowcards = sum(Yellow_Cards, na.rm = TRUE))

Playerstat <- dffoot%>%
  group_by(Name)%>%
  summarise(playergoals = sum(Goals, na.rm = TRUE), playerassists = sum(Assists, na.rm = TRUE), playerredcards = sum(Red_Cards, na.rm = TRUE),
            playeryellowcards = sum(Yellow_Cards, na.rm = TRUE))


Leaguestat <- dffoot%>%
  group_by(League)%>%
  summarise(leaguegoals = sum(Goals, na.rm = TRUE), leagueassists = sum(Assists, na.rm = TRUE), leagueredcards = sum(Red_Cards, na.rm = TRUE),
            leagueyellowcards = sum(Yellow_Cards, na.rm = TRUE))


Seasonstat <- dffoot%>%
  group_by(Season)%>%
  summarise(seasongoals = sum(Goals, na.rm = TRUE), seasonassists = sum(Assists, na.rm = TRUE), seasonredcards = sum(Red_Cards, na.rm = TRUE),
            seasonyellowcards = sum(Yellow_Cards, na.rm = TRUE))
