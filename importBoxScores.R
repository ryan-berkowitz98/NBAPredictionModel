# This is not my originial work, I have copied code from user: kjytay
# On GitHub to import data from basketball-reference.com, I have made some
# Minor changes, but for the most part this is his/her work


# This script scrapes top-level NBA schedule and results 
# from basketball-reference.com. User can set year and list of months 
# to determine the window of games to scrape. At the end of the script, 
# I reconstruct the conference standings based on W-L percentage.

library(rvest)
library(lubridate)

########
# PARAMETERS
########
year <- "2020"
monthList <- c("october", "november", "december", "january", "february", 
               "march", "april", "may", "june")
playoff_startDate <- ymd("2020-04-14")
outputfile <- "NBA-2020_game_data.rds"

########
# SCRIPT FOR SCRAPING DATA STARTS HERE
########
df <- data.frame()
for (month in monthList) {
  # get webpage
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                "_games-", month, ".html")
  webpage <- read_html(url)
  
  # get column names
  col_names <- webpage %>% 
    html_nodes("table#schedule > thead > tr > th") %>% 
    html_attr("data-stat")    
  col_names <- c("game_id", col_names)
  
  # extract dates column
  # note that in april, there is a break in the table which just says 
  # "Playoffs". this messes with the data merging later, so we get rid of it
  dates <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>% 
    html_text()
  dates <- dates[dates != "Playoffs"]
  
  # extract game id
  # we need to remove the NA that is due to the "Playoffs" row in april
  game_id <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>%
    html_attr("csk")
  game_id <- game_id[!is.na(game_id)]
  
  # extract all columns (except date)
  data <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > td") %>% 
    html_text() %>%
    matrix(ncol = length(col_names) - 2, byrow = TRUE)
  
  # combine game IDs, dates and columns in dataframe for this month, add col names
  month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
  names(month_df) <- col_names
  
  # add to overall dataframe
  df <- rbind(df, month_df)
}

# change columns to the correct types
df$visitor_pts <- as.numeric(df$visitor_pts)
df$home_pts    <- as.numeric(df$home_pts)
df$attendance  <- as.numeric(gsub(",", "", df$attendance))
df$date_game   <- mdy(df$date_game)

# add column to indicate if regular season or playoff
df$game_type <- with(df, ifelse(date_game >= playoff_startDate, 
                                "Playoff", "Regular"))

# drop boxscore column
df$box_score_text <- NULL

#remove rows of games that were not played due to COVID-19
df <- na.omit(df)

# save to file
#write_csv(df, "NBA_2020_Game_Data.csv")

########
# SCRIPT FOR RANKING TABLE STARTS HERE
########

# get winner and loser of each game
df$winner <- with(df, ifelse(visitor_pts > home_pts, 
                             visitor_team_name, home_team_name))
df$loser <- with(df, ifelse(visitor_pts < home_pts, 
                            visitor_team_name, home_team_name))

# build up standings table for regular season
regular_df <- subset(df, game_type == "Regular")
teams <- sort(unique(regular_df$visitor_team_name))
standings <- data.frame(team = teams, stringsAsFactors = FALSE)




#If following work sequentially, I next worked on advancedBoxScores.R






