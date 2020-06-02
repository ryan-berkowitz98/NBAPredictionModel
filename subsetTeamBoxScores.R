#This script is essential to creating a margin of error for 
#NBA predicitons. When building the four factor model, I used each teams'
#season averages, but the model did not consider the variation in a team's
#performance throughout the season. This script scrapes each teams' nightly 
#stats, which will allow us to later calculate the variance in our 
#model vaiables and create a confidence interval for our predictions. 
#I first scraped data for all the home teams, than I moved on to the away 
#teams before combing all the data into a single dataframe

library(tidyverse)

#Load the necessary data
game_df <- read_csv("NBA_2020_Game_Data.csv")
master_list <- readRDS("NBA_2020_Master_Box_Score.rds")

#Declare the dimensions for our matrices 
iterations <- length(master_list);
variables <- 11;

#create an empty matrix for the home teams and declare the variables
homeTeam <- matrix(ncol = variables, nrow = iterations);
colnames(homeTeam) <- c("gameID", "team", "FG", "FGA", "3P", "3PA", "FT", 
                      "FTA", "ORB", "TOV", "DRB")


#for loop that populates the entire home team matrix
for(i in 1:iterations){
  tempFrame = master_list[[i]]$home_basic_boxscore
  homeTeam[i,1] = game_df$game_id[i]
  homeTeam[i,2] = game_df$home_team_name[i]
  homeTeam[i,3] = tempFrame$FG[length(tempFrame$Player)]
  homeTeam[i,4] = tempFrame$FGA[length(tempFrame$Player)]
  homeTeam[i,5] = tempFrame$`3P`[length(tempFrame$Player)]
  homeTeam[i,6] = tempFrame$`3PA`[length(tempFrame$Player)]
  homeTeam[i,7] = tempFrame$FT[length(tempFrame$Player)]
  homeTeam[i,8] = tempFrame$FTA[length(tempFrame$Player)]
  homeTeam[i,9] = tempFrame$ORB[length(tempFrame$Player)]
  homeTeam[i, 10] = tempFrame$TOV[length(tempFrame$Player)]
  homeTeam[i,11] =tempFrame$DRB[length(tempFrame$Player)]

}



#create an empty matrix for the away teams and declare the variables
awayTeam <- matrix(ncol = variables, nrow = iterations);
colnames(awayTeam) <- c("gameID", "team", "FG", "FGA", "3P", "3PA", "FT", 
                        "FTA", "ORB", "TOV", "DRB");



#for loop that populates the entire away team matrix
for(i in 1:iterations){
  tempFrame = master_list[[i]]$visitor_basic_boxscore
  awayTeam[i,1] = game_df$game_id[i]
  awayTeam[i,2] = game_df$visitor_team_name[i]
  awayTeam[i,3] = tempFrame$FG[length(tempFrame$Player)]
  awayTeam[i,4] = tempFrame$FGA[length(tempFrame$Player)]
  awayTeam[i,5] = tempFrame$`3P`[length(tempFrame$Player)]
  awayTeam[i,6] = tempFrame$`3PA`[length(tempFrame$Player)]
  awayTeam[i,7] = tempFrame$FT[length(tempFrame$Player)]
  awayTeam[i,8] = tempFrame$FTA[length(tempFrame$Player)]
  awayTeam[i,9] = tempFrame$ORB[length(tempFrame$Player)]
  awayTeam[i, 10] = tempFrame$TOV[length(tempFrame$Player)]
  awayTeam[i,11] =tempFrame$DRB[length(tempFrame$Player)]
  
}


#Turn both matrices into tibbles, rearrange them both by gameID
#and write csv's for later use to build confidence Intervals
everyGameHomeTeamStats <- as_tibble(homeTeam);
everyGameHomeTeamStats <- arrange(everyGameHomeTeamStats, gameID);
#write_csv(everyGameHomeTeamStats, "homeTeamStatsEveryGame.csv");


everyGameAwayTeamStats <- as_tibble(awayTeam);
everyGameAwayTeamStats <- arrange(everyGameAwayTeamStats, gameID);
#write_csv(everyGameAwayTeamStats, "awayTeamStatsEveryGame.csv");





#If following work sequentially, I next worked on modelSkeleton.R












