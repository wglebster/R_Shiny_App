#global.R
library(shiny)
library(tidyverse)
library(CodeClanData)

#set input variables
min_slider <- min(game_sales$year_of_release)
max_slider <- max(game_sales$year_of_release)
genre_choice <- c(unique(game_sales$genre))
platform_choice <- c(unique(game_sales$platform))
rating_choice <- c(unique(game_sales$rating))
stats_choice <- c("Top 10 critic rated games", "Top 10 user rated games", 
                  "Top 10 games sales", 
                  "All Top 10s", "Platform with most games", 
                  "Studio with most releases",
                  "The year with most game releases")
