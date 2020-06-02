#This script calculates a confidence interval for each prediction made by 
#the NBA prediction model I have built. I do this by building a four
#factor score for every game that each team played throughout the season.
#This allows the program to account for variation within team play
#and build a confidence interval for each predicted outcome

library(tidyverse);
modelDataNoCI <- read_csv("modelDataNoCI.csv");


homeTeamCI <- read_csv("homeTeamStatsEveryGame.csv");
awayTeamCI <- read_csv("awayTeamStatsEveryGame.csv");


#building a four factor score for every team for every game throughout 
#the season

#first building up the four factors for the home team for every game

#calculate the number of Possessions the home team had in each game
POSS <- function(){
  homeTeamCI$FGA - homeTeamCI$ORB + 
    homeTeamCI$TOV + 0.4*homeTeamCI$FTA
} 


# Add POSS to the home team data table
homeTeamCI <- homeTeamCI %>% 
  mutate(POSS = POSS());


#Calculate EFGP
EFGP <- function(){
  ((homeTeamCI$FG + 0.5*homeTeamCI$`3P`) / 
     (homeTeamCI$FGA + homeTeamCI$`3PA`)) - 
    ((awayTeamCI$FG + 0.5*awayTeamCI$`3P`) /
       (awayTeamCI$FGA + awayTeamCI$`3PA`))
} 

#Add EFGP to home team data table
homeTeamCI <- homeTeamCI %>% 
  mutate(EFGP = EFGP());


#Calculating Net TOVP
NTOVP <- function(){
  (homeTeamCI$TOV / POSS()) - 
    (awayTeamCI$TOV / POSS())
} 

#Add NTOVP to the home team data table
homeTeamCI <- homeTeamCI %>% 
  mutate(NTOVP = NTOVP());


#Calculating ORBP
ORBP <- function(){
  (homeTeamCI$ORB /(homeTeamCI$ORB + awayTeamCI$DRB)) - 
    (awayTeamCI$ORB / (awayTeamCI$ORB + homeTeamCI$DRB))
} 

#Add ORBP to the home team data table
homeTeamCI <- homeTeamCI %>% 
  mutate(ORBP = ORBP());


#Calculating NFTP
NFTP <- function(){
  (homeTeamCI$FT/homeTeamCI$FGA) - 
    (awayTeamCI$FT/awayTeamCI$FGA);
}

#Add NFTP to the home team data table
homeTeamCI <- homeTeamCI %>% 
  mutate(NFTP = NFTP());



#next building up the four factors for the away team for every game

#calculate the number of Possessions the home team had in each game
POSS <- function(){
  awayTeamCI$FGA - awayTeamCI$ORB +
    awayTeamCI$TOV + 0.4*awayTeamCI$FTA
}


# Add POSS to the home team data table
awayTeamCI <- awayTeamCI %>%
  mutate(POSS = POSS());


#Calculate EFGP
EFGP <- function(){
  ((awayTeamCI$FG + 0.5*awayTeamCI$`3P`) /
     (awayTeamCI$FGA + awayTeamCI$`3PA`)) -
    ((homeTeamCI$FG + 0.5*homeTeamCI$`3P`) /
       (homeTeamCI$FGA + homeTeamCI$`3PA`))
}

#Add EFGP to home team data table
awayTeamCI <- awayTeamCI %>%
  mutate(EFGP = EFGP());


#Calculating Net TOVP
NTOVP <- function(){
  (awayTeamCI$TOV / POSS()) -
    (homeTeamCI$TOV / POSS())
}

#Add NTOVP to the home team data table
awayTeamCI <- awayTeamCI %>%
  mutate(NTOVP = NTOVP());


#Calculating ORBP
ORBP <- function(){
  (awayTeamCI$ORB /(awayTeamCI$ORB + homeTeamCI$DRB)) -
    (homeTeamCI$ORB / (homeTeamCI$ORB + awayTeamCI$DRB))
}

#Add ORBP to the home team data table
awayTeamCI <- awayTeamCI %>%
  mutate(ORBP = ORBP());


#Calculating NFTP
NFTP <- function(){
  (awayTeamCI$FT/awayTeamCI$FGA) -
    (homeTeamCI$FT/homeTeamCI$FGA);
}

#Add NFTP to the home team data table
awayTeamCI <- awayTeamCI %>%
  mutate(NFTP = NFTP());




#Combine the home and away team data frames to get a comprehensive list
#of team stats for every game througout the NBA season
allTeamStatsEveryGame <- rbind(homeTeamCI, awayTeamCI);


#Create a four factor score for every team for every game of the season
allTeamStatsEveryGame <- allTeamStatsEveryGame %>% 
  mutate(fourFactorScore = (.9*EFGP) + (.01*NTOVP) + (.05*ORBP) + 
           (.04*NFTP));

#Write a csv for later use
#write_csv(allTeamStatsEveryGame, "allTeamStatsEveryGame.csv");


#Create an empty matrix to put the lower and upper values of the confindence
#Interval in. We will populate this matrix one row at a time then combine
#the columns with the official model 

ciMatrix <- matrix(nrow = length(modelDataNoCI$gameID), ncol = 2);
colnames(ciMatrix) <- c("lowerCI", "upperCI");


#Build a for loop that populates the CI matrix
for(i in 1:nrow(modelDataNoCI)){
  
  #create temporary home and away data frames that subset all of the game
  #date for each team
  tempFrameHome <- filter(allTeamStatsEveryGame, 
                          team == modelDataNoCI$homeTeam[i])
  tempFrameAway <- filter(allTeamStatsEveryGame,
                          team == modelDataNoCI$awayTeam[i])
  
  #calculate the predicted pace for the game
  pace <- (mean(tempFrameHome$POSS) - mean(allTeamStatsEveryGame$POSS)) +
    (mean(tempFrameAway$POSS) - mean(allTeamStatsEveryGame$POSS)) + 
    mean(allTeamStatsEveryGame$POSS)
  
  #calculate the pooled standard deviation for the two teams playing
  #to use in the CI equation
  pooledDeviation <- sqrt(
    (
      (nrow(tempFrameHome)*var(tempFrameHome$fourFactorScore)) + 
        (nrow(tempFrameAway)*var(tempFrameAway$fourFactorScore))
    ) / 
      (nrow(tempFrameHome) + nrow(tempFrameAway) - 2)
  )
  differenceInMeans <- (mean(tempFrameHome$fourFactorScore) - 
                mean(tempFrameAway$fourFactorScore))
  
  #set the 95% CI constant of 1.96 as a constant to use in the CI equation
  z <- 1.96
  
  #calculate the lower limit of the confidence interval
  lowValue <- 2*pace*(differenceInMeans - (z*pooledDeviation))
  lowValue <- lowValue + mean(modelDataNoCI$homeMinusAwayActual)
  lowValue <- round(lowValue, digits = 0)
  
  #calculate the upper limit of the confidence interval
  highValue <- 2*pace*(differenceInMeans + (z*pooledDeviation))
  highValue <- highValue +mean(modelDataNoCI$homeMinusAwayActual)
  highValue <- round(highValue, digits = 0)
  
  #Populate the ciMatrix with the lower and upper limits of the 
  #confidence interval
  ciMatrix[i, 1] <- lowValue
  ciMatrix[i, 2] <- highValue
}


#Add the lower and upper levels of the confidence interval to our model data
#This will be the final form of our data set and I will be using this 
#in the shiny server for the most part
NBAPredicitionModelData <- cbind(modelDataNoCI, ciMatrix);


#Write a csv for later use
#write_csv(NBAPredicitionModelData, "NBAPredictionModelData.csv");




#If following work sequentially, I next worked on modelPerformance.R






