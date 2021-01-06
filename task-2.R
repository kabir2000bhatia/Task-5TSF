#Exploratory data analysis on IPL dataset

install.packages('readr')
install.packages("dplyr")
install.packages("tidyverse")

library(readr)
library(dplyr)
library(ggplot2)

setwd("C:\\Users\\HP\\Downloads\\SPARKS_INTERNSHIP")

matches_data <- read.csv("matches.csv")
deliveries_data <- read.csv("deliveries.csv")

View(matches_data)
View(deliveries_data)

head(matches_data)
summary(matches_data)
summary(deliveries_data)

sum(is.na(matches_data))
sum(is.na(deliveries_data))

#1 Number of matches played per season

No_of_matches_per_season <- select(matches_data, season)
No_of_matches_per_season <- group_by(No_of_matches_per_season, season)
No_of_matches_per_season <- summarize(No_of_matches_per_season, n())
No_of_matches_per_season

# OR

No_of_matches_per_season <- matches_data%>%select(season)%>%group_by(season)%>%summarize(total=n())
No_of_matches_per_season
ggplot(No_of_matches_per_season, aes(x=season, y=total, colour=season))+ geom_point()

write.csv(No_of_matches_per_season,"matchesPerSeason.csv")

#2 Most number of man of the match awards

Most_plyr_of_the_match_winner <- matches_data%>%select(player_of_match)%>%group_by(player_of_match)%>%summarize(total_count=n())%>%arrange(-total_count)
Most_plyr_of_the_match_winner<- Most_plyr_of_the_match_winner[1:10,]
Most_plyr_of_the_match_winner

write.csv(Most_plyr_of_the_match_winner, "MaxPlyrOfTheMatch.csv")

#3 Team with most wins/Most Successful teams

tot_wins_by_team <- matches_data%>%select(winner)%>%group_by(winner)%>%summarize(tot_wins=n())%>%arrange(-tot_wins)
tot_wins_by_team

write.csv(tot_wins_by_team, "MostSuccessfulTeams.csv")

#4 team who won by maximum runs

win_by_max_runs <- matches_data%>%select(winner, win_by_runs)%>%group_by(winner)%>%summarize(total_runs=sum(win_by_runs))%>%arrange(-total_runs)
win_by_max_runs

write.csv(win_by_max_runs, "TeamWinByMaxRuns.csv")

#5 team won by min runs
win_by_min_runs <- matches_data%>%select(winner, win_by_runs)%>%group_by(winner)%>%summarize(total_runs=sum(win_by_runs))%>%arrange(total_runs)
win_by_min_runs

write.csv(win_by_min_runs, "TeamWinByMinRuns.csv")

#6 team who won by max wickets

win_by_max_wickets <- matches_data%>%select(winner, win_by_wickets)%>%group_by(winner)%>%summarize(tot_wickets=sum(win_by_wickets))%>%arrange(-tot_wickets)
win_by_max_wickets

write.csv(win_by_max_wickets, "TeamWinByMaxWickets.csv")

#7 stadium/venue with max no of matches played

max_venue <-matches_data%>%select(venue)%>%group_by(venue)%>%summarize(no_of_matches_played=n())%>%arrange(-no_of_matches_played)
max_venue

write.csv(max_venue, "MaxVenue.csv")

#8 batsman with most runs

most_runs_by_batsman <- deliveries_data%>%select(batsman, batsman_runs)%>%group_by(batsman)%>%summarize(tot_runs=sum(batsman_runs))%>%arrange(-tot_runs)
most_runs_by_batsman

write.csv(most_runs_by_batsman, "MostRunsByPlayer.csv")

#9 Top bowlers with most wickets
 deliveries_data%>%if_else(batsman==player_dismissed, 1, 0)

most_wickets_by_bowler<- deliveries_data%>%select(bowler, player_dismissed)%>%group_by(bowler)
most_wickets_by_bowler

#10 Bowler who conceded max runs

min_runs_conceded <- deliveries_data%>%select(bowler, total_runs)%>%group_by(bowler)%>%summarize(tot_runs=sum(total_runs))%>%arrange(-tot_runs)
min_runs_conceded


#11 Has Toss-winning helped in winning matches?

y=0
n=0
for(i in seq(1,nrow(matches_data)))
{
  if (matches_data$toss_winner[i] == matches_data$winner[i])
    y=y+1
  else 
    n=n+1
}

if (y >= n)
{
  print(paste("Yes, Toss-winning helped in winning matches."))
  print(paste("Matches won by toss_winner are: ", y, "& Total matches: ", nrow(matches_data)))
} else
  
{
  print(paste("No, Toss-winning didn't help in winning matches."))
  print(paste("Matches won by other team are: ", n, "& Total matches: ", nrow(matches_data))) 
}

winning_cnt = c(y,n)
teams = c("toss_win_team_won_match" , "toss_win_team_lost_match ")
df = data.frame(teams,winning_cnt, stringsAsFactors = F)
df

write.csv(df, "tossFactor.csv")

