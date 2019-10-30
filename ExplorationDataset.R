rm(list=ls())

library(tidyverse)
library(DataExplorer)

dffoot <- read.csv("~/Desktop/RTHIB/FootballApp/Shiny Foot/bundesliga-20132017-all-team-stats/Major_European_Leagues_Stats_Std_2013_17.csv", comment.char="#")

DataExplorer::introduce(dffoot)

plot_bar(dffoot)
plot_histogram(dffoot)

dffoot$Assists = as.integer(dffoot$Assists)

str(dffoot)

sum((dffoot$Name == "Su\xe1rez, Luis")== TRUE)

unique(dffoot$Name)
  
dffoot <- dffoot %>%
  mutate(Name=replace(Name, Name == "Cristiano Ronaldo" ,"Ronaldo, Cristiano"))
#%>%
#  mutate(Name=replace(Name, Name == "Su\xe1rez, Luis" ,"Suarez, Luis")) %>%
#  mutate(Name=replace(Name, Name == "g\xe1ero, Sergio" ,"Aguero, Sergio")) %>%
#  mutate(Name=replace(Name, Name == "Higua\xe1n, Gonzalo" ,"Higuain, Gonzalo")) %>%
#  mutate(Name=replace(Name, Name == "S<e1>nchez, Alexis	" ,"Sanchez, Alexis")) %>%
#  as.data.frame()

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




top20goals <- Playerstat %>%
  arrange(desc(playergoals)) %>%
  head(20)

top20stats <- dffoot[c(1,3,7)] %>%
  filter(Name %in% top20goals$Name)%>%
    group_by(Name, Season)%>%
    summarise_all(funs(sum), na.rm= TRUE)

ggplot(top20stats,aes(x = Season, y = Goals, color = Name, group= Name))+
  geom_point()+
  geom_line()


ggplot(top20stats)+
  geom_point(mapping = aes(x = Name, y = Goals, color = Name))


top20goalsAccrosseason <- top20stats%>%
  spread(key = Season, value = Goals)













