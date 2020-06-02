## Server NBA Prediction Model

library(shiny);
library(PerformanceAnalytics);
library(stringr);


#Code for creating game spread starts on line 39
#Code for first tabbed panel starts on line 89
#Code for second tabbed panel starts on line 193
#Code for third tabbed panel starts on line 360
#Code for fourth tabbed panel starts on line 493



server <- function(input, output){
  
  #Import the necessary data tables
  teamStats <- read_csv("teamStats.csv");
  modelData <- read_csv("NBAPredictionModelData.csv");
  allTeamStatsEveryGame <- read_csv("allTeamStatsEveryGame.csv");
  simpleModel <- read_csv("simpleModelData.csv");
  
  
  #create a reactive function that returns the column of the home team 
  homeColumn <-  reactive({
      number = match(input$homeTeam, teamStats$Team)
      return(number)
  })
  
  #create a reactive function that returns the column of the away team
  awayColumn <-  reactive({
    number = match(input$awayTeam, teamStats$Team)
    return(number)
  })
  
  
  
  #~~~~~~~~DETERMINING THE SPREAD OF THE GAME USING FOUR FACTORS~~~~~~~~~##

  #compute the predicted pace of the game
  paceCalc <- reactive({
    (teamStats$POSS[homeColumn()] - mean(teamStats$POSS)) +
      (teamStats$POSS[awayColumn()] - mean(teamStats$POSS)) + 
      mean(teamStats$POSS);
  })
  
  #round the pace to the nearest hundreth
  pace <- reactive({
    round(paceCalc(), digits = 2)
  })
  
  
  #calculate the predicted spread
  predictedSpread <- reactive({
    round(
      (.9*(teamStats$EFGP[homeColumn()] - teamStats$EFGP[awayColumn()]) +
         .01*(teamStats$NTOVP[homeColumn()] - teamStats$NTOVP[awayColumn()]) +
         .05* (teamStats$ORBP[homeColumn()] - teamStats$ORBP[awayColumn()]) +
         .04*(teamStats$NFTP[homeColumn()] - teamStats$NFTP[awayColumn()]) )*
        (2*pace()) + mean(modelData$homeMinusAwayActual) 
    )
     
  })

  
  #let the user know who the predicted winner is & by how many points
  output$spread <- renderText({
    ifelse(
      predictedSpread() > 0, 
           paste("The", homeTeamName(),
             "are predicted to win by",predictedSpread(), " points."),
           ifelse(
             predictedSpread() < 0,
             paste("The", awayTeamName(), "are predicted to win by",
                   -1*predictedSpread(), "points."),
             paste("The game is predicted to end in a tie.", "")
           )
      )
  })
  
  ##DONE DETERMINING THE SPREAD OF THE GAME##
  
  
  
  
  
  
  ####START OF CODE FOR FIRST TABBED PANEL
  
  
  ##~~~~~~~~~DISPLAY TEAM RANK FOR EACH OF THE FOUR FACTORS~~~~~~~~~~~~~##
  
  
  #write a new dataframe for each of the four factors that is ordered
  #by rank
  
  EFGPRanks <- teamStats %>% 
    arrange(desc(EFGP))
  
  NTOVPRanks <- teamStats %>% 
    arrange(desc(NTOVP))
  
  ORBPRanks <- teamStats %>% 
    arrange(desc(ORBP))
  
  NFTPRanks <- teamStats %>% 
    arrange(desc(NFTP))
  
  
  #Create an empty matrix that will be populated with each teams
  #rank for each of the four factors
  rankMatrix <- matrix(ncol=5, nrow=2);
  colnames(rankMatrix) = c("Team", "EFGP",
                           "NTOVP", "ORBP", "NFTP")
  
  #Write a function that populates the matrix according to team ranks
  populateRankMatrix <- reactive({
    rankMatrix[1,1] = homeTeamName();
    rankMatrix[1,2] = match(homeTeamName(), EFGPRanks$Team);
    rankMatrix[1,3] = match(homeTeamName(), NTOVPRanks$Team);
    rankMatrix[1,4] = match(homeTeamName(), ORBPRanks$Team);
    rankMatrix[1,5] = match(homeTeamName(), NFTPRanks$Team);
    rankMatrix[2,1] = awayTeamName();
    rankMatrix[2,2] = match(awayTeamName(), EFGPRanks$Team);
    rankMatrix[2,3] = match(awayTeamName(), NTOVPRanks$Team);
    rankMatrix[2,4] = match(awayTeamName(), ORBPRanks$Team);
    rankMatrix[2,5] = match(awayTeamName(), NFTPRanks$Team);
    
    rankTable <- as.data.frame(rankMatrix)
    return(rankTable)
  })
  
  #write a reactive function that populates the four factor ranks output
  #to be called in the ui
  output$fourFactorRanks <- renderTable({
    populateRankMatrix()
  })
  
  #write a description of the table that will be displayed above it
  output$tableDescription <- renderText({
    "Team Ranks for each of the Four Factor Variables used in
     determing the spread of the game."
  })
  
  #write an overall description of how the four factor variables were
  #calculated
  output$variableDescription <- renderText({
    c("Each variable is calculated using a combination of a",
      "team's performance on both offense and defense.")
  })
  
  #write a description for EFGP
  output$EFGPDescription <- renderText({
    c("EFGP - 'Effective Field Goal Percentage': An alternative measure",
      "of a \nteam's field goal percentage that adjusts for a team's",
      "ability to make \n3 pointers.")
  })
  
  #write a description for NTOVP
  output$NTOVPDescription <- renderText({
    c("NTOVP - 'Net Turnover Percentage': An estimation of turnovers",
      "commited \nby a team per 100 possesions.")
  })
  
  #write a description for ORBP
  output$ORBPDescription <- renderText({
    c("ORBP - 'Offensive Rebounding Percentage': The percentage", 
      "of contested \nrebounds that a team grabs after its own missed shots.")
  })
  
  #write a description for NFTP
  output$NFTPDescription <- renderText({
    c("NFTP - 'Net Free Throw Percentage': The number of free throws",
      "per \nevery field goal attempt.")
  })
  
  
  ##~~~~ DONE DISPLAYING TEAM RANK FOR THE FOUR FACTORS~~~~~##
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###START OF CODE FOR SECOND TABBED PANEL
  
  
  ##~~CREATE A HISTOGRAM TO DISPLAY ALL POSSIBLE OUTCOMES OF THE GAME~~##
  
  #Create a reactive function that returns the home team name 
  homeTeamName <-  reactive({
    input$homeTeam
  })
  
  #Create a reactive function that returns the away team name 
  awayTeamName <-  reactive({
    input$awayTeam
  })
  
  #Write 2 functions that create temporary data frames that displays the 
  #stats for all the games the home and away team played in throughout
  #the season
  tempFrameHome <- function(){
    tempDFHome <- filter(allTeamStatsEveryGame,
                            team == homeTeamName());
    return(tempDFHome)
  }
  
  tempFrameAway <- function(){
    tempDFAway <- filter(allTeamStatsEveryGame,
                         team == awayTeamName());
    return(tempDFAway)
  }

  
  #Create a function that randomly generates a single game from each team
  #and returns the spread
  getRandomSpread <- function(){
      #call the functions that create temporary home and away data frames
      tempFrameHome <- tempFrameHome();
      tempFrameAway <- tempFrameAway();
      
      #randomly select one game from each the home team and the away team
      randomHomeGame <- sample_n(tempFrameHome, 1);
      randomAwayGame <- sample_n(tempFrameAway, 1);
      
      
      #calculate the Predicted Spread for the game
      RandomSpread <- 2*pace()*(randomHomeGame$fourFactorScore -
                                     randomAwayGame$fourFactorScore) +
        mean(modelData$homeMinusAwayActual);
      
  }
  
  #Create an empty matrix that will be populated with 1000 simulated games
  simulationMatrix <- matrix(nrow=1000, ncol=1)

  #write a formula that populates the entire matrix
  populateCIMatrix <- reactive({
    colnames(simulationMatrix) <-"predictionOfSpread"
    
    #use the predictedSpread() function to populate the first row 
    #of the matrix so the populateCIMatrix() renders as a reactive function
    simulationMatrix[1,1] = predictedSpread()
    
    #run a for loop that populates the rest of the matrix
    for(i in 2:1000){
      simulationMatrix[i,1] = getRandomSpread()
    }
    finalMatrix <- as.data.frame(simulationMatrix)
    return(finalMatrix)
  })
  
  
  ### CONFIDENCE INTERVAL FUNCTIONS ####
  
  #function that returns the lower value of the CI
  calculateLowValueCI <- function(){
    #call the functions that create temporary home and away data frames
    tempFrameHome <- tempFrameHome();
    tempFrameAway <- tempFrameAway();
    
    #calculate the pooled standard deviation for the two teams playing
    #to use in the CI equation
    pooledDeviation <- sqrt((
      (nrow(tempFrameHome)*var(tempFrameHome$fourFactorScore)) + 
        (nrow(tempFrameAway)*var(tempFrameAway$fourFactorScore))
    ) / 
      (nrow(tempFrameHome) + nrow(tempFrameAway) - 2))
    differenceInMeans <- (mean(tempFrameHome$fourFactorScore) - 
                            mean(tempFrameAway$fourFactorScore))
    
    #set the 95% CI constant of 1.96 as a constant to use in the CI equation
    z <- 1.96
    
    #calculate the lower limit of the confidence interval
    lowValue <- 2*pace()*(differenceInMeans - (z*pooledDeviation))
    lowValue <- lowValue + mean(modelData$homeMinusAwayActual)
    lowValue <- round(lowValue, digits = 0)
    
    return(lowValue)
    
  }
  
  #function that returns the lower value of the CI
  calculateHighValueCI <- function(){
    #call the functions that create temporary home and away data frames
    tempFrameHome <- tempFrameHome();
    tempFrameAway <- tempFrameAway();
    
    #calculate the pooled standard deviation for the two teams playing
    #to use in the CI equation
    pooledDeviation <- sqrt((
      (nrow(tempFrameHome)*var(tempFrameHome$fourFactorScore)) + 
        (nrow(tempFrameAway)*var(tempFrameAway$fourFactorScore))
    ) / 
      (nrow(tempFrameHome) + nrow(tempFrameAway) - 2))
    differenceInMeans <- (mean(tempFrameHome$fourFactorScore) - 
                            mean(tempFrameAway$fourFactorScore))
    
    #set the 95% CI constant of 1.96 as a constant to use in the CI equation
    z <- 1.96
    
    #calculate the upper limit of the confidence interval
    highValue <- 2*pace()*(differenceInMeans + (z*pooledDeviation))
    highValue <- highValue +mean(modelData$homeMinusAwayActual)
    highValue <- round(highValue, digits = 0)
    
    return(highValue)
    
  }
  
  
  
  #Create a histogram of all 1000 simulated games
   output$CIPlot <- renderPlot({
     ggplot(populateCIMatrix(), aes(predictionOfSpread)) +
       geom_histogram(color="seagreen", fill="turquoise3",
                      alpha=.3, binwidth = 3) +
       geom_vline(aes(xintercept= predictedSpread(),color="Model Prediction"),
                  linetype = "solid", size=1.2, alpha=.8 )+
       geom_vline(aes(xintercept = calculateLowValueCI(), 
                   color="95% Confidence Interval"), linetype = "dashed",
                  size = 1.01, alpha=.8) +
       geom_vline(aes(xintercept = calculateHighValueCI(),
                   color = "95% Confidence Interval"), linetype = "dashed",
                  size = 1.01, alpha=.8) +
       xlim(-50,50) +
       ggtitle(paste("Distribution of Predicted Spreads", 
                     "for 1000 Simulated Games"), 
               subtitle = paste(awayTeamName(), "@", homeTeamName())) +
       xlab(paste("Predicted Spread \n(Positve Values Indicate Home",
                  "Team is Favored, Negative Values Indicate Away Team",
                  "is Favored)"))+
       ylab("Simulated Game Count") +
       scale_color_manual(name="", 
                          values = c("95% Confidence Interval" = "red",
                                              "Model Prediction" = "yellow"))
     
   })
   
   ##~~~~~~~~~~DONE CREATING SIMULATED GAMES HISTOGRAM~~~~~~~~~~~~~~~##
  
   
  
   
   
   
   
   
   
   ###START OF CODE FOR THIRD TABBED PANEL 
   
   
   ##~~~~~~~~~~~~ CREATE & DISPLAY MODEL PERFORMANCE METRICS ~~~~~~~~~~~~##
   
   
   #Write a description for what is being displayed in the model performance
   #tab
   output$gamesPlayedDescription <- renderText({
     paste("All matchups between The", awayTeamName(), "& The", 
           homeTeamName(), "\n     during the 2020 season.")
   })
   
   # Create a data frame that displays all games played between 2 chosen
   # teams during the 2020 season
   populateGamesPlayed <- function(){
     #return columns that match the chosen home and away teams
     gamesPlayed <- modelData %>% 
       filter((awayTeam == awayTeamName() | awayTeam == homeTeamName()) & 
                (homeTeam == homeTeamName() | homeTeam == awayTeamName()))
     
     #create a variable that displays the date of the game
     gamesPlayed <- gamesPlayed %>% 
       mutate(
         date = paste0(substr(gameID, 5, 6), "/", 
                            substr(gameID, 7, 8), "/",
                            substr(gameID, 1, 4))
              )
     
     #create a variable that displays the score of the game
     gamesPlayed <- gamesPlayed %>% 
       mutate(
         score = paste0(awayPts, "-", homePts)
       )
     
     #only return the relevant columns
     gamesPlayed <- gamesPlayed %>% 
       select(date, awayTeam, homeTeam, score, predictionError)
     return(gamesPlayed)
   }
   
   
   #write a render table function that will populate itself with the games
   #played function
   output$gamesPlayedTable <- renderTable({
     populateGamesPlayed()
   })
   
   
   #Since the spread between the two chosen teams will chnge depending on 
   #which team is the home team and which is the away, display both spreads
   #so the user can gain a better understanding of the prediction errors
   #for the games already played between the two teams
   
   #write a description that tells the user the above information ^
   output$switchedSpreadDescription <- renderText({
     c("Because the model accounts for a homecourt advantage, the",
       "predicted outcome depends on which team is the home team. The two",
       "separate spreads are shown below:")
   })
   
   #once again, display the spread for the chosen home and away teams
   
   #split the team names into a list so length() works and we can capture
   #just the team name and not the city
   homeTeamSplit <- reactive({
     str_split(homeTeamName(), ' ')[[1]];
   })
   
   awayTeamSplit <- reactive({
     str_split(awayTeamName(), ' ')[[1]];
   })
     
   
   output$spreadOfChosenGame <- renderText({
     c(
       "Predicted outcome of a game between The", homeTeamName(), "& \nThe",
       awayTeamName(), "when The", homeTeamName(), "are the home team:",
       ifelse(
         predictedSpread() > 0, 
         paste("\nThe", word(homeTeamName(), length(homeTeamSplit())),
               "win by", as.character(predictedSpread()), "points."), 
         ifelse(predictedSpread() < 0,
                paste("\nThe", word(awayTeamName(), length(awayTeamSplit())), 
                      "win by", as.character(-1*predictedSpread()),
                      "points."),
                paste("The game is predicted to end in a tie.", ""))
       )
     )
   })
   
   
   
   
   #display the spread for a game in which the home and away teams are
   #switched
   
   #create a function that calculates thenew spread for the game
   #with home and away teams swithced
   switchedSpread <- function(){
     predictedSpread() - 2*mean(modelData$homeMinusAwayActual) %>% 
       round()
   }
   
   #display the predicted spread of a game in which the chosen home and 
   #away teams are switched
   output$switchedSpreadOfChosenGame <- renderText({
     c(
       "Predicted outcome of a game between The", homeTeamName(), "& \nThe",
       awayTeamName(), "when The", awayTeamName(), "are the home team:",
       ifelse(
         switchedSpread() > 0, 
         paste("\nThe", word(homeTeamName(), length(homeTeamSplit())), 
               "win by", as.character(switchedSpread()), "points."), 
         ifelse(switchedSpread() < 0,
                paste("\nThe", word(awayTeamName(), length(awayTeamSplit())), 
                      "win by", as.character(-1*switchedSpread()),
                      "points."),
                paste("The game is predicted to end in a tie.", ""))
       )
     )
   })
   
   

   
   
   
   
   
   
   
   
   ###START OF CODE OFR FOURTH TABBED PANEL
   
   
   ## ~~~~~~~~~ CREATE & DISPLAY STATISTICAL ANALYSIS METRICS ~~~~~~~~~~~##
   
   #write a description that explains how the model performance will be 
   #analyzed
   output$analysisIntro <- renderText({
     paste("To analyze the performance of the Four Factor Model, I want",
           "to compare it to a much simpler model and test whether or",
           "not it makes more accurate predictions.")
   })
     
     
     #Write a description of how the simple model was built
     output$simpleModelDescription <- renderText({
       paste("The simple model predicts the team with the higher winnning", 
             "\npercentage will win the game. If the two teams have the same", 
             "winning \npercentage, a winner is chosen at random.",
             "The predicted spread for the \nsimple model is the average ",
             "margin of victory for all games during \nthe 2020 season."
         
       )
     })
           
   
   
   
   #create an empty df that to be populated with performance metrics
   #from both models
   modelPerformance <- matrix(nrow=4, ncol=3);
   colnames(modelPerformance) <- c(" ", "Four Factor Model", "Simple Model");
   modelPerformance <- as.data.frame(modelPerformance);
   
   #Write a function that populates the cells of model performance df
   buildModelPerformance <- function(){
     #determine how many games each model correctly predicted and 
     #enter those values in the df
     modelPerformance[1,1] = "Games Correctly Predicted"
     modelPerformance[1,2] = round(sum(modelData$wasModelCorrect));
     modelPerformance[1,3] = round(sum(simpleModel$wasModelCorrect));
     
     #calculate how many games each model did not correctly predict
     #and enter those values into the df
     modelPerformance[2,1] = "Games Incorrectly Predicted";
     modelPerformance[2,2] = round((nrow(modelData) - 
                                sum(modelData$wasModelCorrect)));
     modelPerformance[2,3] = round((nrow(modelData) - 
                                sum(simpleModel$wasModelCorrect)));
     
     #calculate the average prediction error for each of the models and 
     #enter the values into the df
     modelPerformance[3,1] = "Average Prediction Error";
     modelPerformance[3,2] = mean(modelData$predictionError);
     modelPerformance[3,3] = mean(simpleModel$predictionError);
     
     #calculate the sd of the prediction error for each of the mdoels and
     #enter the values into the df
     modelPerformance[4,1] = "SD of Prediction Errors";
     modelPerformance[4,2] = sd(modelData$predictionError);
     modelPerformance[4,3] = sd(simpleModel$predictionError);
     

     return(modelPerformance)
   }
   
   #write an output function that will call the function to build the model
   #perfomance table
   output$modelPerformanceTable <- renderTable({
     buildModelPerformance()
   })
   
   #run a paired t-test that compares the prediction errors of the four factor 
   #model to the prediction errors of the simple model
   modelTest <- t.test(modelData$predictionError, simpleModel$predictionError,
                       paired = T, alternative = "less");
   
   
   #capture the output from the t test
   capturedOutput <- capture.output(modelTest);
   
   #Create a data table where important information from the t test
   #can be stored
   modelTestTable <- matrix(ncol=2, nrow =5);
   colnames(modelTestTable) <- c(" ", " ");
   modelTestTable <- as.data.frame(modelTestTable);
   
   #Write a function that populates the table with the useful information
   populateTestTable <- function(){
     modelTestTable[1,1] = "Test Type:"
     modelTestTable[1,2] = capturedOutput[2]
     modelTestTable[2,1] = "Mean of the Differences:"
     modelTestTable[2,2] = substr(capturedOutput[11],15, 24)
     modelTestTable[3,1] = "t-value:"
     modelTestTable[3,2] = substr(capturedOutput[5], 5, 11)
     modelTestTable[4,1] = "df:"
     modelTestTable[4,2] = substr(capturedOutput[5], 18, 21)
     modelTestTable[5,1] = "P-value:"
     modelTestTable[5,2] = substr(capturedOutput[5], 34, 42)
     
     return(modelTestTable)
   }
   
   #Write a render table function that captures the populated data table
   output$modelTestResults <- renderTable({
     populateTestTable()
   })
   
   
   #Write a description of the model test table
   output$modelTestDescription <- renderText({
     paste("To determine the statistical significance of the Four Factor",
           "Model's ability to make predictions, I ran a paired t-test",
           "comparing the prediction errors of the two models.")
   })
   
   #Write a concusion about the t-test output
   output$testConclusion <- renderText({
     c("There is significant evidence to conclude at the 95% confidence",
       "\nlevel that on average, the Four Factor Model makes more accurate",
       "\npredictions (has a lower prediction error) than the simple model.")
   })

   
   
}