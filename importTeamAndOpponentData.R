# This script imports team data from basketball-reference.com. It includes 
# general team statistics about each team, as well as statistics about
# how opponents played against the team. I originially imported the 
# data into excel before reading it into r. 

library(readxl);
library(tidyverse);


teamPerGame <- read_excel("basketballReference (2) (1).xlsx",
                          sheet = 1);

opponentPerGame <- read_excel("basketballReference (2) (1).xlsx",
                              sheet = 2);


# Order both dataframes alphebetically by team so that the row number
# will be the same when referencing a team
teamPerGame <- teamPerGame[order(teamPerGame$Team),];
opponentPerGame <- opponentPerGame[order(opponentPerGame$Team),];


#get rid of any special characters in the data frame
teamPerGame <- teamPerGame %>% 
  mutate(Team = gsub("[[:punct:]]", "", Team))


#Write a csv for later use
#write_csv(teamPerGame, "teamPerGameData.csv");
#write_csv(opponentPerGame, "opponentPerGameData.csv");



#If following work sequentially, I next worked on 
# CalculatingFourFactors.R





