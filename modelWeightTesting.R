##In general, the stats used for building a four factor model are agreed upon. 
#However, there has been much debate about the optimal weights assigned to 
#each of the four factors. I am using this script to determine the ideal 
#weights for each of thefour factors in the NBA Prediction Model based on 
#previous research. To determine the weights that perform the best, I 
#propose four seperate models and analyze which has the lowest average 
#prediction error using an ANOVA test

library(tidyverse);
source("modelSkeleton.R");



##Using the weights proposed by Dean Oliver in his Original Model
originalFourFactors <- spreadCalc(.4, .25, .2, .15)


#Using the weights proposed by Konstantinos Kotzias at statathlon.com
statAthalonweights <- spreadCalc(.43, .39, .1, .8)

#Weighting the model heavily towards shooting given its prevelance in 
#today's NBA
weightedTowardsShooting <- spreadCalc(.6, .15, .15, .1)

#Given that the weightedTowardsShooting method seems to make more accurate
#predictions, try weighting even more towards shooting
superShooting <- spreadCalc(.9,.01,.05,.04)



##Run an Anova Test to determine if one weighting method is statistically
#better than the others ##

#First add a column to each of the data frames that tells us which 
#weighting method was used to build the model
originalFourFactors <- originalFourFactors %>% 
  mutate(weightingMethod = "Orignial Four Factors");

statAthalonweights <- statAthalonweights %>% 
  mutate(weightingMethod = "Stat Athalon");

weightedTowardsShooting <- weightedTowardsShooting %>% 
  mutate(weightingMethod = "Weighted Towards Shooting");

superShooting <- superShooting %>% 
  mutate(weightingMethod = "Weighted Extremely Towards Shooting");

#Combine all three data frames into a single data frame so we can fit 
#an anova model 
anovaTestingFrame <- rbind(originalFourFactors, statAthalonweights, 
                            weightedTowardsShooting, superShooting)

#Convert the weighting method variable into a factor to use in the
#anova test
anovaTestingFrame <- anovaTestingFrame %>% 
  mutate(weightingMethod = as.factor(weightingMethod))

  
  
#Fit an anova model to the data using weighting method as the explanatory
#variable and the prediction error as the response variable
anovaTestModel <- aov(predictionError~weightingMethod, 
                      data = anovaTestingFrame)


#Since we the anova test returned a significant p-value, run a 
#post hoc analysis to determine which groups are different from one 
#another
posthoc <- TukeyHSD(anovaTestModel);


#The analysis shows that the original four factor weighted model,
#the model weighted towards shooting, and the model weighted extremely
#towards shooting have significantly lower prediction errors than the 
#model using the stat athalon weights. 
#There was not a significant difference in the prediction errors between
#the model using the orinigal four factor weights, the model weighted
#towards shooting, or the model weighted extremely towards shooting.
#Since the model weighted extremely towards shooting made the most accurate 
#predictions on average, I will be using those weights for the all of my 
#remaining analysis 


modelDataNoCI <- superShooting;
modelDataNoCI <- modelDataNoCI %>% 
  select(-weightingMethod);

#write_csv(modelDataNoCI, "modelDataNoCI.csv");




#If following work sequentially, I next worked on 
#calculatingConfidenceIntervals.R








