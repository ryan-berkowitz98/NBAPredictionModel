## ui NBA Prediction Model
library(shiny);
library(tidyverse);
library(shinythemes);
library(PerformanceAnalytics);

#Import data tables
teamStats <- read_csv("teamStats.csv");
modelData <- read_csv("NBAPredictionModelData.csv");
allTeamStatsEveryGame <- read_csv("allTeamStatsEveryGame.csv");



ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  
  #Application Title
  titlePanel("NBA Prediction Model"),
  
  sidebarLayout(
    
    #create layout for sidebar panel
    sidebarPanel(
      #Input Functions
      selectInput(inputId = "homeTeam", label = "Home Team", 
                  choices = teamStats$Team),
      selectInput(inputId = "awayTeam", label = "Away Team", 
                  choices = teamStats$Team, selected = teamStats$Team[2]),
      textOutput("spread")
    ),
    
    
    #create a tabbed main panel
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  #First tabbed pannel
                  tabPanel(
                    "Four Factor Comparison", 
                    verbatimTextOutput("tableDescription"),
                    tableOutput("fourFactorRanks"),
                    textOutput("variableDescription"),
                    verbatimTextOutput("EFGPDescription"),
                    verbatimTextOutput("NTOVPDescription"),
                    verbatimTextOutput("ORBPDescription"),
                    verbatimTextOutput("NFTPDescription")
                  ),
                  
                  #Second tabbed panel
                  tabPanel(
                    "Simulated Games",
                    plotOutput("CIPlot")
                  ),
                  
                  #third tabbed panel
                  tabPanel(
                    "Model Performance",
                    verbatimTextOutput("gamesPlayedDescription"),
                    tableOutput("gamesPlayedTable"),
                    textOutput("switchedSpreadDescription"),
                    br(),
                    verbatimTextOutput("spreadOfChosenGame"),
                    verbatimTextOutput("switchedSpreadOfChosenGame")
                  ),
                  
                  #fourth tabbed panel
                  tabPanel(
                    "Statistical Analysis", 
                    hr(),
                    textOutput("analysisIntro"),
                    br(),
                    tableOutput("modelPerformanceTable"),
                    verbatimTextOutput("simpleModelDescription"),
                    br(),
                    textOutput("modelTestDescription"),
                    tableOutput("modelTestResults"),
                    verbatimTextOutput("testConclusion")
                  )
  
      )
      
    )
    
  )

)
