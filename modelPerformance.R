#This script will help analyze the performace of the prediction model. 
#To tell whether or not it would even be useful to use the four factors model
#to predict the outcome of games, I will be building a much simpler model
#that simply predicts the team with the better record at the time they face
#each other will be the winner. I will then test whether or not one model
#is significantly better at predicting game outcomes than the other


library(tidyverse);

#load the datasets that will be used in this script
modelData <- read_csv("NBAPredictionModelData.csv");
teamStats <- read_csv("teamStats.csv");

#create a new dataset that has all of the game information from the season
#that will be used to build the simpler model
simpleModel <- modelData %>% 
  select(c(gameID:homeMinusAwayActual));


#create a new data frame that will be populated with a team's wins, 
#losses, and win percentage
winsLosses <- matrix(ncol=4, nrow = nrow(teamStats));
colnames(winsLosses) <- c("team", "wins", "losses", "winPCT");
winsLosses <- as.data.frame(winsLosses);


#Write a function that will populated the winsLosses dataframe
buildWinsLosses <- function(){
  for(i in 1:nrow(winsLosses)){
    #the first column is the team's name
    winsLosses[i,1] = teamStats$Team[i]
    
    #create a temporary data frame that will be populated with all the 
    #games that the team won
    tempFrameWins <- filter(modelData,
                            winner == winsLosses[i,1])
    
    #populate the wins column of the winsLosses dataframe with the length
    #of the temporary wins dataframe
    winsLosses[i,2] = nrow(tempFrameWins)
    
    #create a temporary data frame that is populated with all of the games
    #that the team played in
    tempFrameAllGames <- filter(modelData,
                                awayTeam == winsLosses[i,1] | 
                                  homeTeam == winsLosses[i,1]) ;
    
    #populate the losses column of the winsLosses dataframe
    winsLosses[i,3] = nrow(tempFrameAllGames) - winsLosses[i,2];
    
    #calculate a team's win percentage and populate the last column
    #of the winsLosses dataframe
    winsLosses[i,4] = winsLosses[i,2] / nrow(tempFrameAllGames);
    
  } 
  return(winsLosses)
}

#call the function that populates the winsLosses dataframe
winsLosses <- buildWinsLosses();
 

#create a dataframe that will be populated with the predicted winner of 
#each game using the simple model. After building this dataframe we will
#combine it with rest of the simpleModel data 
simplePredictedWinner <- matrix(nrow=nrow(modelData), ncol=1);
colnames(simplePredictedWinner) <- "predictedWinner";
simplePredictedWinner <- as.data.frame(simplePredictedWinner);


#write a function that will populate the predictedWinner dataframe
buildSimplePredictedWinner <- function(){
  for(i in 1:nrow(modelData)){
    #for each game, get the corresponding column numbers in the 
    #winsLosses dataframe to help determine which team has the higher 
    #win pctg
    homeColumn <- match(modelData$homeTeam[i], winsLosses$team);
    awayColumn <- match(modelData$awayTeam[i], winsLosses$team);
    
    #assign a variable both the home and away teams that captures
    #their win pctg
    homeWinPct <- winsLosses$winPCT[homeColumn]
    awayWinPct <- winsLosses$winPCT[awayColumn]
    
    #set up a dataFrame that is populated with the two teams currently 
    #playing. This will be used in the case that both teams have the same
    #winning pctg. If this is the case, one of the teams will be chosen 
    #at random as the predicted winner
    currentGame <- matrix(nrow=2, ncol=1);
    colnames(currentGame) <- "team";
    currentGame[1,1] = modelData$homeTeam[i];
    currentGame[2,1] = modelData$awayTeam[i];
    currentGame <- as_tibble(currentGame)
   
    #populate the predicted winner matrix. 
    ifelse(homeWinPct == awayWinPct, 
          simplePredictedWinner[i,1] <- sample_n(currentGame, 1),
          ifelse(homeWinPct > awayWinPct, 
                 simplePredictedWinner[i,1] <- modelData$homeTeam[i], 
                 simplePredictedWinner[i,1] <- modelData$awayTeam[i])
          )
  }
  return(simplePredictedWinner)
}

#call the function that populates the predicted winner matrix and assign it
#to a variable
simplePredictedWinner <- buildSimplePredictedWinner();

#add the predicted winner column to the simple model dataframe
simpleModel <- cbind2(simpleModel, simplePredictedWinner); 

#add "wasModelCorrect" variable to the dataframe that takes value of 1
#if the model predicted the correct winner and 0 if it did not
simpleModel <- simpleModel %>% 
  mutate(wasModelCorrect = ifelse(predictedWinner == winner, 1, 0));


#to compare the effectiveness of the four factor model and the simple model
#we want to compare the prediction errors of both models. However, we have 
#yet to make an prediction for the simple model.
#The prediction for the simple model is just going to be the average of the 
#the margin of victory for all games played during the season

#add a variable to the simple model data that sets the predicted spread equal
#to the average margin of victory of all games played during the season. if
#the home team is the predicted winner, the spread is positive, and if the 
#away team is the predicted winner, the spread is negative
simpleModel <- simpleModel %>% 
  mutate(
    predictedSpread = ifelse(homeTeam == predictedWinner, 
                             mean(abs(modelData$homeMinusAwayActual)),
                             -1*mean(abs(modelData$homeMinusAwayActual))) %>% 
      round()
  ); 

#add a variable to the simple model data that calulates the prediction error
#of the simple model
simpleModel <- simpleModel %>% 
  mutate(predictionError = abs(predictedSpread - homeMinusAwayActual));

#write a csv of the simple model to be used in the future
#write_csv(simpleModel, "simpleModelData.csv");


#run a paired t-test that compares the prediction errors of the four factor 
#model to the prediction errors of the simple model
modelTest <- t.test(modelData$predictionError, simpleModel$predictionError,
                    paired = T, alternative = "less");

#the results of the test:
# t = -6.6925, df = 970, p-val = 1.853e^-11
#there is significant evidence to conclude that on average, the four factor
#model is better at making predictions (has a smaller average prediction
#error) than the simple model for NBA games during the 2020 season






#If following work sequentially, I next worked on building a GUI. The code
#for my GUI can be found in ui.R & server.R









