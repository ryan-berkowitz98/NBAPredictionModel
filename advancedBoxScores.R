# This is not my originial work, I have copied code from user: kjytay
# On GitHub to import data from basketball-reference.com, I have made some
# Minor changes, but for the most part this is his/her work

# This script scrapes the box scores for all NBA games in the 
# 2017-18 season. Results are saved into a list called `master`. 
# The key for a game is its game_id (which we can get from the 
# output of 2018-12-11_nba_game_data.R). 
# master$game_id is itself a list of 5 items:
#   1. visitor basic box score
#   2. visitor advanced box score
#   3. home basic box score
#   4. home advanced box score
#   5. score profile against time (like data frame in 
#      nba_play_by_play_single_game.R)
#
# To use this script for other seasons, run 2018-12-11_nba_game_data.R 
# for a different year, then use that output file as the input 
# for this script.



library(rvest)
library(lubridate)
library(tidyverse)

inputfile <- read_csv("NBA_2020_Game_Data.csv")
outputfile <- "../data/NBA-2020_box_score.rds"

# remove the +'s from play-by-play score
parseScore <- function(x) {
  if (startsWith(x, "+")) {
    return(str_sub(x, 3, str_length(x)))
  } else if (endsWith(x, "+")) {
    return(str_sub(x, 1, str_length(x) - 1))
  } else {
    return(x)
  }
}

# helper function to get raw HTML box scores in better shape
parseBoxScore <- function(xx) {
  names(xx) <- c("Player", xx[1,][-1])  # get correct col names
  xx[xx == "Did Not Play"] <- NA
  
  # new col to say who started and who was reserve
  xx$Role <- "Reserve"
  xx$Role[1:6] <- "Starter"
  xx$Role[nrow(xx)] <- NA
  
  # remove old column headings, coerce statistics to numeric type
  xx <- xx[c(-1, -7), ]
  for (j in 3:(ncol(xx)-1)) {
    xx[, j] <- as.numeric(xx[, j])
  }
  xx
}

##############################
# SCRIPT STARTS HERE
##############################
game_df <- as.tibble(inputfile)

master <- list()

for (current_id in game_df$game_id) {
  print(current_id)
  
  ##########
  # get box scores
  ##########
  url <- paste0("https://www.basketball-reference.com/boxscores/",
                current_id, ".html")
  webpage <- read_html(url)
  
  tables <- webpage %>% html_nodes("table") %>%
    html_table()
  names(tables) <- c("visitor_basic_boxscore", "two",
                     "three", "four", 
                     "five", "six", "seven", "eight", "nine",
                     "ten", "eleven", "home_basic_boxscore", "thirteen",
                     "fourteen", "fifteen", "sixteen")
  tables <- lapply(tables, parseBoxScore)
  

  master[[current_id]] <- tables
}


#Save the list of box scores for later use
#saveRDS(master, "NBA_2020_Master_Box_Score.rds")






#If following work sequentially, I next worked on subsetTeamBoxScores.R







