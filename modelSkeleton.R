#This script builds the foundation of the four factor model. It is the
#"skeleton" of the final model. I write all of the functions
#for calculating a four factor score for each team for every game played
#during the 2020 season. However, this script is not used for making 
#actual predictions. Since my model incorporates weighted averages, I
#test the prediction accuracy of several weighting methods in 
#modelWeightTesting.R, where I end up creating predictions for all the 
#NBA games this season.


library(tidyverse);


#import the data sets that we are going to use
fourFactors <- read_csv("teamStats.csv");
gameLog <- read_csv("NBA_2020_Game_Data.csv");
gameLog <- gameLog %>% 
  select(gameID = game_id, awayTeam = visitor_team_name, 
         awayPts = visitor_pts, homeTeam = home_team_name, 
         homePts = home_pts, winner);


#Add a column to the gameLog that for the actual home - away points
gameLog <- gameLog %>% 
  mutate(homeMinusAwayActual = homePts - awayPts);


#add a column to gameLog that categorizes each game by how many
#points the game was one by ("close game", "average game", "blowout game")
gameLog <- gameLog %>% 
  mutate(
    gameCategorization = 
      ifelse(
        abs(homeMinusAwayActual) <= mean(homeMinusAwayActual) + 
                                       0.5*sd(homeMinusAwayActual), 
        "Close Game", ifelse(
          abs(homeMinusAwayActual) >= mean(homeMinusAwayActual) +
                                         1.5*sd(homeMinusAwayActual),
          "Blowout Game", "Average Game"
        )
      )
  )


#add homeColumn to gameLog table
gameLog <- gameLog %>% 
  mutate(
    homeColumn = match(gameLog$homeTeam, fourFactors$Team)
  )

#add awayColumn to gameLog table
gameLog <- gameLog %>% 
  mutate(
    awayColumn = match(gameLog$awayTeam, fourFactors$Team)
  )



#Get the predicted pace
gameLog <- gameLog %>% 
  mutate(
    pace = 
      (fourFactors$POSS[homeColumn] - mean(fourFactors$POSS)) +
      (fourFactors$POSS[awayColumn] - mean(fourFactors$POSS)) + 
      mean(fourFactors$POSS)
  )


#Write a function that gets the predicted spread with the ability to 
#determine the weights for the four factors
spreadCalc <- function(weightEFGP, weightNTOVP, weightORBP, weightNFTP){
  gameLog <- gameLog %>% 
    mutate(
      predictedSpread = 
        (weightEFGP*(fourFactors$EFGP[homeColumn] - fourFactors$EFGP[awayColumn]) + 
           weightNTOVP*(fourFactors$NTOVP[homeColumn] - fourFactors$NTOVP[awayColumn]) +
           weightORBP*(fourFactors$ORBP[homeColumn] - fourFactors$ORBP[awayColumn]) + 
           weightNFTP*(fourFactors$NFTP[homeColumn] - fourFactors$NFTP[awayColumn]))*
        (2*pace) + mean(gameLog$homeMinusAwayActual)
    )
  
  #round the predicted spread to a whole number
  gameLog <- gameLog %>% 
    mutate(predictedSpread = round(predictedSpread, digits=0))
  
  #Calculate how far the prediction was off from the actual score
  gameLog <- gameLog %>% 
    mutate(predictionError = abs(predictedSpread - homeMinusAwayActual))
  
  #Determine whether or not the model predicted the correct winning team
  #value of 1 indicates winning team was correctly predicted
  #value of 0 indicates model did not correctly predict the winning team
  gameLog <-gameLog %>% 
    mutate(
      wasModelCorrect = ifelse(
        (predictedSpread<0 & homeMinusAwayActual<0) |
          (predictedSpread>0 & homeMinusAwayActual>0), 1, 0
      )
    )


  
  #create a new data frame dropping the variabels we do not need
  modelTest <- gameLog %>% 
    select(
      c(gameID:homeMinusAwayActual, predictedSpread:predictionError,
        gameCategorization, wasModelCorrect)
    )
  
  
  #return the condensed data frame
  return(modelTest)
  
}



#If following work sequentially, I next worked on modelWeightTesting.R
  

