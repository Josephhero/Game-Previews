library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(nflfastR)
library(nflreadr)
library(nflplotR)
library(shadowtext)
library(ggplot2)
library(ggimage)
library(gt)
library(gtExtras)
library(webshot2)
library(cowplot)
library(ggforce)
library(EBImage)
library(magick)
library(sysfonts)
library(showtext)

# Data-----

create_card_function <- function(year = get_current_season(), 
                                 week = get_current_week()
){
  weeks <- c(6:10)

  for(i in weeks){
    week <- i
  this_week <- week
  
  pbp <- load_pbp(seasons = year) |> 
    filter(week <= (this_week - 1))
  
  sched <- load_schedules(seasons = year)
  
  home_teams <- sched |> 
    filter(week == this_week) |> 
    pull(home_team)
  
  source("R/Game Background.R")
  source("R/Game Overview.R")
  source("R/Game Stats.R")
  source("R/Assemble Card.R")
  
  for(i in home_teams){
    background_function(f_sched = sched, f_year = year, f_week = week, f_team = i)
    overview_function(f_sched = sched, f_year = year, f_week = week, f_team = i)
    stats_function(f_pbp = pbp, f_sched = sched, f_year = year, f_week = week, f_team = i)
    card_function(f_sched = sched, f_year = year, f_week = week, f_team = i)
  }
}

create_card_function()
