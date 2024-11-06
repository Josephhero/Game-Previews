# library(tidyverse)
# library(nflverse)
# library(gt)
# library(gtExtras)

# Data-----

overview_function <- function(f_sched = sched, 
                              f_year = get_current_season(), 
                              f_week = get_current_week(use_date = TRUE), 
                              f_team = "KC"){
  
  game_selected <- filter(f_sched, week == f_week, home_team == f_team | away_team == f_team)
  
  game_id_selected <- game_selected$game_id[1]
  
  game_data <- f_sched |> 
    mutate(calc_home = if_else(home_moneyline < away_moneyline, abs(home_moneyline), 100)) |> 
    mutate(calc_away = if_else(away_moneyline < home_moneyline, abs(away_moneyline), 100)) |> 
    mutate(vig_home = calc_home / (abs(home_moneyline) + 100)) |> 
    mutate(vig_away = calc_away / (abs(away_moneyline) + 100)) |> 
    mutate(home_wp = vig_home / (vig_home + vig_away)) |> 
    mutate(away_wp = vig_away / (vig_home + vig_away)) |> 
    select(game_id, home_team, away_team, home_wp, away_wp, home_rest, away_rest) |> 
    clean_homeaway() |> 
    arrange(desc(location)) |> 
    select(game_id, team, location, wp = team_wp, rest = team_rest)
  
  espn_data_gh_url <- "https://github.com/Josephhero/ESPN-Game-Data/raw/refs/heads/main/Data/"
  
  espn_data1 <- read_csv(paste0(espn_data_gh_url, f_year, "_espn_game_data.csv"))
  
  espn_data2 <- espn_data1 |> 
    select(season, game_id, week, home_away, team_abbr, team_record, team_score, 
           turnovers, redzone_att, redzone_conv, penalties) |> 
    mutate(across(c(turnovers, penalties), ~replace_na(.x, 0)))
  
  espn_home1 <- espn_data2 |> 
    filter(home_away == "home") |> 
    select(-home_away)
  
  espn_home <- espn_home1 |> 
    rename_with(~ paste0("home_", .), .cols = setdiff(names(espn_home1), c("season", "game_id", "week")))
  
  espn_away1 <- espn_data2 |> 
    filter(home_away == "away") |> 
    select(-home_away)
  
  espn_away <- espn_away1 |> 
    rename_with(~ paste0("away_", .), .cols = setdiff(names(espn_home1), c("season", "game_id", "week")))
  
  
  espn_data3 <- left_join(espn_home, espn_away, by = c("season", "game_id", "week")) |> 
    clean_homeaway() 
  
  espn_data <- espn_data3 |> 
    summarize(games = n(), 
              record = last(team_record), 
              team_points = sum(team_score, na.rm = TRUE), 
              opp_points = sum(opponent_score, na.rm = TRUE), 
              turnovers_lost = sum(team_turnovers, na.rm = TRUE), 
              turnovers_gained = sum(opponent_turnovers, na.rm = TRUE), 
              redzone_attempts = sum(team_redzone_att, na.rm = TRUE), 
              redzone_conversions = sum(team_redzone_conv, na.rm = TRUE), 
              penalties = sum(team_penalties, na.rm = TRUE), 
              .by = team_abbr) |> 
    mutate(ppg = team_points / games) |> 
    mutate(point_diff = team_points - opp_points) |> 
    mutate(net_turnovers = turnovers_gained - turnovers_lost) |> 
    mutate(redzone_td_rate = redzone_conversions / redzone_attempts) |> 
    mutate(penalties_per_game = penalties / games) 
  
  espn_data_table <- game_data |> 
    filter(game_id == game_id_selected) |> 
    left_join(espn_data, by = c("team" = "team_abbr")) |> 
    mutate(wp = wp * 100) |>
    mutate(ppg = ppg) |> 
    mutate(redzone_td_rate = redzone_td_rate * 100) |> 
    mutate(location = case_when(
      game_selected$location[1] == "Neutral" ~ "Neutral", 
      location == "home" ~ "Home", 
      location == "away" ~ "Away", 
      location == "neutral" ~ "Neutral", 
      TRUE ~ location
    )) |> 
    select(team, record, location, rest, wp, ppg, point_diff, net_turnovers, redzone_td_rate, penalties_per_game)
  
  # Table-----
  hulk_pal <- c(
    "#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
    "#d9f0d3", "#7fbf7b", "#1b7837"
  )
  
  (
    overview_tab <- 
      gt(espn_data_table) |> 
      gt_theme_538() |>
      sub_missing(missing_text = "") |>
      gt_nfl_logos("team") |> 
      cols_label(team = "", 
                 record = "W-L", 
                 location = "loc", 
                 rest = html("Rest<br>Days"), 
                 wp = html("win<br>probability"),
                 ppg = html("Points<br>Per Game"), 
                 point_diff = html("point<br>differential"), 
                 net_turnovers = html("net<br>turnovers"), 
                 redzone_td_rate = html("Red Zone<br>TD Rate"), 
                 penalties_per_game = html("penalties<br>per game")
      ) |> 
      gtExtras::gt_color_box(columns = wp, 
                             domain = range(
                               (min(game_data$wp, na.rm = TRUE) * 100) - 0.1, 
                               (max(game_data$wp, na.rm = TRUE) * 100) + 0.1), 
                             accuracy = 0.1, 
                             suffix = "%"
      ) |> 
      gtExtras::gt_color_box(columns = ppg, 
                             domain = range(
                               (min(espn_data$ppg, na.rm = TRUE) - 0.1), 
                               (max(espn_data$ppg, na.rm = TRUE) + 0.1)), 
                             accuracy = 0.1
      ) |> 
      gtExtras::gt_color_box(columns = point_diff, 
                             domain = range(
                               min(espn_data$point_diff, na.rm = TRUE), 
                               max(espn_data$point_diff, na.rm = TRUE))
      ) |> 
      gtExtras::gt_color_box(columns = net_turnovers, 
                             domain = range(
                               min(espn_data$net_turnovers, na.rm = TRUE), 
                               max(espn_data$net_turnovers, na.rm = TRUE))
      ) |> 
      gtExtras::gt_color_box(columns = redzone_td_rate, 
                             domain = range(
                               (min(espn_data$redzone_td_rate, na.rm = TRUE) * 100) - 0.1, 
                               (max(espn_data$redzone_td_rate, na.rm = TRUE) * 100) + 0.1), 
                             accuracy = 0.1, 
                             suffix = "%"
      ) |> 
      gtExtras::gt_color_box(columns = penalties_per_game, 
                             domain = range(
                               (min(espn_data$penalties_per_game, na.rm = TRUE) - 0.1), 
                               (max(espn_data$penalties_per_game, na.rm = TRUE)) + 0.1), 
                             accuracy = 0.1, 
                             palette = rev(hulk_pal)
      ) |> 
      fmt_number(columns = penalties_per_game, decimals = 1) |> 
      # Column Label format
      tab_style(
        style = cell_text(align = "center", 
                          weight = "bold", 
                          font = google_font("Barlow Condensed"), 
                          size = px(18)
        ),
        locations = cells_column_labels(everything())
      ) |> 
      tab_style(
        style = cell_text(weight = "bold",
                          font = google_font("Barlow Condensed"),
                          size = px(18)),
        locations = cells_body(everything())
      ) |>
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = "record")
      ) 
  )
  
  gtsave(overview_tab, 
         path = "./images", 
         filename = paste0("Overview.png"), 
         expand = c(5, 10, 5, 10)
  )
  
}

