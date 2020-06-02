#This script takes the data that was imported from basketball_reference.com
#and uses the team stats to build each of the variables in the four
#factor model. At the end I create a new data table that only contains 
#the relevant information needed to create the model 

### NBA FUNCTIONS ###
library(tidyverse);


#Import both data sets
teamPerGame <- read_csv("teamPerGameData.csv");
opponentPerGame <- read_csv("opponentPerGameData.csv");




## Calculating Team Field Goal Percentage (EFGP)  ##

#Calculate EFGP
EFGP <- function(){
  ((teamPerGame$FG + 0.5*teamPerGame$`3P`) / 
    (teamPerGame$FGA + teamPerGame$`3PA`)) - 
    ((opponentPerGame$FG + 0.5*opponentPerGame$`3P`) /
    (opponentPerGame$FGA + opponentPerGame$`3PA`))
} 

#Add EFGP to data table
teamPerGame <- teamPerGame %>% 
  mutate(EFGP = EFGP());

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##Calculating Team Net Turnover Percentage (TOVP) ##

# Calculating  possesions per game for both data tables
POSS <- function(){
  teamPerGame$FGA - teamPerGame$ORB + 
    teamPerGame$TOV + 0.4*teamPerGame$FTA
} 


# Add POSS to the data table
teamPerGame <- teamPerGame %>% 
  mutate(POSS = POSS());

#Calculating Net TOVP
NTOVP <- function(){
  (teamPerGame$TOV / POSS()) - 
    (opponentPerGame$TOV / POSS())
} 

#Add NTOVP to the data table
teamPerGame <- teamPerGame %>% 
  mutate(NTOVP = NTOVP());


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##Calculating Team Offensive Rebounding Percentage (ORBP)

#Calculating ORBP
ORBP <- function(){
  (teamPerGame$ORB /(teamPerGame$ORB + opponentPerGame$DRB)) - 
    (opponentPerGame$ORB / (opponentPerGame$ORB + teamPerGame$DRB))
} 

#Add ORBP to the data table
teamPerGame <- teamPerGame %>% 
  mutate(ORBP = ORBP());


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



##Calculating Team Net Free Throw Percentage (NFTP) ##

#Calculating NFTP
NFTP <- function(){
  (teamPerGame$FT/teamPerGame$FGA) - 
    (opponentPerGame$FT/opponentPerGame$FGA);
}

#Add NFTP to the data table
teamPerGame <- teamPerGame %>% 
  mutate(NFTP = NFTP());


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Create a new data table with just the four factor variables
teamStats <- teamPerGame %>% 
  select(Team, EFGP:NFTP);

#Write a csv to use for later use and shiny app
#write_csv(teamStats, "teamStats.csv");



#If following work sequentially, I next worked on importBoxScores.R







