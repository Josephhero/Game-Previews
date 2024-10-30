library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(ggplot2)
library(ggimage)
library(gt)
library(gtExtras)
library(cowplot)
library(ggforce)
library(EBImage)

# Data-----

create_card_function <- function(year = get_current_season(), 
                                 week = get_current_week()
){

  this_week <- week
  
  pbp <- load_pbp(seasons = year)
  
  sched <- load_schedules(seasons = year)
  
  home_teams <- sched |> 
    filter(week == this_week) |> 
    pull(home_team)
  
  
  source("Game Background.R")
  source("Game Overview.R")
  source("Game Stats.R")
  source("Assemble Card.R")
  
  for(i in home_teams){
    background_function(f_sched = sched, f_year = year, f_week = week, f_team = i)
    overview_function(f_sched = sched, f_year = year, f_week = week, f_team = i)
    stats_function(f_pbp = pbp, f_sched = sched, f_year = year, f_week = week, f_team = i)
    card_function(f_sched = sched, f_year = year, f_week = week, f_team = i)
  }
}

create_card_function()


