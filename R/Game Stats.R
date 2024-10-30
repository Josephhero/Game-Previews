# library(tidyverse)
# library(nflverse)
# library(gt)
# library(gtExtras)

# Data-----

f_year <- get_current_season()
f_week <- get_current_week()
f_team <- "KC"

# pbp <- load_pbp(seasons = f_year)

stats_function <- function(f_pbp = pbp, 
                           f_sched = sched, 
                           f_year = get_current_season(), 
                           f_week = get_current_week(use_date = TRUE), 
                           f_team = "KC"){
  
  game_data <- f_sched |>
    filter(week == f_week, home_team == f_team | away_team == f_team)
 
  
  pbp <- f_pbp
  scr <- calculate_series_conversion_rates(pbp = pbp)
  
  off_scr <- scr |> 
    arrange(-off_scr) |>
    mutate(type = "series_conversion_rate") |> 
    select(team_abbr = team, type, off_stat = off_scr)
  
  def_scr <- scr |> 
    arrange(def_scr) |> 
    mutate(type = "series_conversion_rate") |> 
    select(team_abbr = team, type, def_stat = def_scr)
  
  off_stats1 <- pbp |> 
    filter(pass == 1 | rush == 1, !is.na(epa), !is.na(down)) |> 
    mutate(explosive_play = case_when(
      pass == 1 & yards_gained >= 20 ~ 1, 
      rush == 1 & yards_gained >= 10 ~ 1, 
      TRUE ~ 0
    )) |> 
    summarize(plays = n(), 
              pass_plays = sum(pass, na.rm = TRUE), 
              rush_plays = sum(rush, na.rm = TRUE), 
              mean_epa = mean(epa, na.rm = TRUE), 
              pass_epa = mean(epa[pass == 1], na.rm = TRUE), 
              rush_epa = mean(epa[rush == 1], na.rm = TRUE), 
              success_rate = sum(success, na.rm = TRUE) / plays, 
              pass_success_rate = sum(success[pass == 1], na.rm = TRUE) / pass_plays, 
              rush_success_rate = sum(success[rush == 1], na.rm = TRUE) / rush_plays,
              pass_rate = sum(pass, na.rm = TRUE) / plays, 
              proe = mean(xpass, na.rm = TRUE) - mean(pass, na.rm = TRUE),  
              early_pass = (sum(pass[down < 3], na.rm = TRUE)), 
              late_pass = (sum(pass[down > 2], na.rm = TRUE)), 
              early_rush = (sum(rush[down < 3], na.rm = TRUE)), 
              late_rush = (sum(rush[down > 2], na.rm = TRUE)), 
              late_success_rate = (sum(success[down > 2], na.rm = TRUE) / (late_pass + late_rush)), 
              explosive_play_rate = sum(explosive_play, na.rm = TRUE) / (late_pass + late_rush), 
              .by = posteam
    ) |>
    select(team_abbr = posteam, everything(), 
           -c(pass_plays, rush_plays, early_pass, late_pass, early_rush, late_rush))
  
  off_stats_pivot <- off_stats1 |> 
    pivot_longer(cols = -c(team_abbr, plays),  # Pivot all columns except team and plays
                 names_to = "type",       # New column for the type (column names)
                 values_to = "off_stat") |> 
    select(-plays)
  
  off_stats <- bind_rows(off_stats_pivot, off_scr) |> 
    arrange(team_abbr)
  
  
  def_stats1 <- pbp |> 
    filter(pass == 1 | rush == 1, !is.na(epa), !is.na(down)) |> 
    mutate(explosive_play = case_when(
      pass == 1 & yards_gained >= 20 ~ 1, 
      rush == 1 & yards_gained >= 10 ~ 1, 
      TRUE ~ 0
    )) |> 
    summarize(plays = n(), 
              pass_plays = sum(pass, na.rm = TRUE), 
              rush_plays = sum(rush, na.rm = TRUE), 
              mean_epa = mean(epa, na.rm = TRUE), 
              pass_epa = mean(epa[pass == 1], na.rm = TRUE), 
              rush_epa = mean(epa[rush == 1], na.rm = TRUE), 
              success_rate = sum(success, na.rm = TRUE) / plays, 
              pass_success_rate = sum(success[pass == 1], na.rm = TRUE) / pass_plays, 
              rush_success_rate = sum(success[rush == 1], na.rm = TRUE) / rush_plays,
              pass_rate = sum(pass, na.rm = TRUE) / plays, 
              proe = mean(xpass, na.rm = TRUE) - mean(pass, na.rm = TRUE),  
              early_pass = (sum(pass[down < 3], na.rm = TRUE)), 
              late_pass = (sum(pass[down > 2], na.rm = TRUE)), 
              early_rush = (sum(rush[down < 3], na.rm = TRUE)), 
              late_rush = (sum(rush[down > 2], na.rm = TRUE)), 
              late_success_rate = (sum(success[down > 2], na.rm = TRUE) / (late_pass + late_rush)), 
              explosive_play_rate = sum(explosive_play, na.rm = TRUE) / (late_pass + late_rush), 
              .by = defteam
    ) |>
    select(team_abbr = defteam, everything(), 
           -c(pass_plays, rush_plays, early_pass, late_pass, early_rush, late_rush))
  
  def_stats_pivot <- def_stats1 |> 
    pivot_longer(cols = -c(team_abbr, plays),  # Pivot all columns except team and plays
                 names_to = "type",       # New column for the type (column names)
                 values_to = "def_stat") |> 
    select(-plays)
  
  def_to <- load_player_stats(stat_type = "defense") |> 
    mutate(turnover = def_interceptions + def_fumble_recovery_opp) |> 
    summarize(turnovers = sum(turnover, na.rm = TRUE), 
              .by = team)
  
  def_stats <- bind_rows(def_stats_pivot, def_scr) |> 
    arrange(team_abbr)
  
  stats <- left_join(off_stats, def_stats, 
                     by = c("team_abbr", "type")) |> 
    mutate(off_rank = dense_rank(desc(off_stat)), .by = type) |> 
    mutate(def_rank = dense_rank(def_stat), .by = type) |> 
    mutate(off_stat_text = case_when(
      str_ends(type, "_epa") ~ paste0(round(off_stat, digits = 3), " (", off_rank, ")"), 
      str_ends(type, "_success") ~ paste0(round(off_stat * 100, digits = 1), "% (", off_rank, ")"),  
      str_ends(type, "_rate") ~ paste0(round(off_stat * 100, digits = 1), "% (", off_rank, ")"), 
      str_ends(type, "proe") ~ paste0(round(off_stat * 100, digits = 1), "% (", off_rank, ")"),
      TRUE ~ paste0(off_stat, " (", off_rank, ")")
    )) |> 
    mutate(def_stat_text = case_when(
      str_ends(type, "_epa") ~ paste0(round(def_stat, digits = 3), " (", def_rank, ")"), 
      str_ends(type, "_success") ~ paste0(round(def_stat * 100, digits = 1), "% (", def_rank, ")"),  
      str_ends(type, "_rate") ~ paste0(round(def_stat * 100, digits = 1), "% (", def_rank, ")"), 
      str_ends(type, "proe") ~ paste0(round(def_stat * 100, digits = 1), "% (", def_rank, ")"),
      TRUE ~ paste0(def_stat, " (", def_rank, ")")
    ))
  
  sched <- load_schedules(seasons = f_year)
  
  game_data <- sched |> 
    filter(week == f_week, home_team == f_team | away_team == f_team) |> 
    left_join(select(load_teams(), team_abbr, home_team_color = team_color), 
              by = c("home_team" = "team_abbr")) |> 
    left_join(select(load_teams(), team_abbr, away_team_color = team_color), 
              by = c("away_team" = "team_abbr"))
  
  home_team <- game_data$home_team[1]
  away_team <- game_data$away_team[1]
  home_team_color <- game_data$home_team_color[1]
  away_team_color <- game_data$away_team_color[1]
  
  
  home_stats <- stats |> 
    filter(team_abbr == home_team) |> 
    select(home_team = team_abbr, home_off = off_stat_text, 
           home_def = def_stat_text, type, home_off_rank = off_rank, 
           home_def_rank = def_rank)
  
  away_stats <- stats |> 
    filter(team_abbr == away_team) |> 
    select(type, away_def = def_stat_text, away_off = off_stat_text, 
           away_team = team_abbr, away_off_rank = off_rank, away_def_rank = def_rank)
  
  stat_type_list <- c(
    "mean_epa", "pass_epa", "rush_epa", "success_rate", "pass_success_rate", 
    "rush_success_rate", "late_success_rate", "pass_rate", "proe", 
    "explosive_play_rate", "series_conversion_rate"
  )
  
  all_stats <- left_join(away_stats, home_stats, by = "type") |> 
    mutate(type_text = case_when(
      type == "mean_epa" ~ "Expected Points Added (EPA)", 
      type == "pass_epa" ~ "Passing EPA", 
      type == "rush_epa" ~ "Rushing EPA", 
      type == "pass_rate" ~ "Pass Rate", 
      type == "proe" ~ "Pass Rate Over Expected", 
      type == "success_rate" ~ "Overall Success Rate (SR)", 
      type == "pass_success_rate" ~ "Passing SR", 
      type == "rush_success_rate" ~ "Rushing SR", 
      type == "late_success_rate" ~ "Late Down Success Rate", 
      type == "explosive_play_rate" ~ "Explosive Play Rate", 
      type == "series_conversion_rate" ~ "Series Conversion Rate",
      TRUE ~ type
    ), .after = type) |> 
    mutate(sort_order = match(type, stat_type_list), .after = type_text) |> 
    replace_na(list(sort_order = 99)) |> 
    select(away_off_rank, away_def_rank, away_off, away_def, type_text, 
           sort_order, home_def, home_off, home_off_rank, home_def_rank) |> 
    arrange(sort_order)
  
  stats_preview <- all_stats |> 
    filter(sort_order <= 11)
  
  hulk_pal <- c(
    "#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7",
    "#d9f0d3", "#7fbf7b", "#1b7837"
  )
  
  run_date <- format(Sys.time(), "%a, %b %d %I:%M %p %Z")
  
  (stats_tab <- 
      gt(all_stats) |>  
      gt_theme_538() |>
      cols_label(home_off = "off", 
                 home_def = "def", 
                 type_text = "", 
                 away_def = "def", 
                 away_off = "off"
      ) |> 
      cols_hide(c(sort_order, ends_with("_rank"))) |> 
      sub_missing(missing_text = "") |> 
      tab_spanner(columns = starts_with("home_"), 
                  label = home_team) |> 
      tab_spanner(columns = starts_with("away_"), 
                  label = away_team) |> 
      tab_spanner(columns = type_text, 
                  label = "@", 
                  id = "at_spanner") |>
      tab_style(
        style = cell_text(align = "center", 
                          weight = "bold", 
                          size = px(18)),
        locations = cells_column_spanners(
          spanners = "at_spanner"
        )
      ) |>
      gt_nfl_wordmarks(locations = cells_column_spanners(), 
                       height = 30) |>
      tab_options(
        column_labels.border.bottom.width = px(0),
        column_labels.border.bottom.color = "transparent",
        column_labels.padding = px(0)
      ) |>
      tab_style(
        style = list(
          cell_text(align = "center", 
                    weight = "bold", 
                    font = google_font("Barlow Condensed"), 
                    size = px(18)),
          cell_borders(
            sides = c("bottom"),
            color = "black",
            weight = px(2)
          )
        ),
        locations = cells_column_labels(everything())
      ) |> 
      tab_style(
        style = list(
          cell_borders(
            sides = c("bottom"),
            color = "white",
            weight = px(2)
          )
        ),
        locations = cells_column_labels(
          columns = c("type_text"))
      ) |>
      tab_style(
        style = cell_text(align = "center", 
                          font = google_font("Barlow Condensed"),
                          size = px(18)),
        locations = cells_body(
          columns = type_text, 
        )
      ) |>
      tab_style(
        style = cell_text(align = "center", 
                          font = google_font("Barlow Condensed"),
                          size = px(18)),
        locations = cells_body(everything())
      ) |>
      data_color(columns = away_off_rank, 
                 target_columns = away_off, 
                 palette = rev(hulk_pal), 
                 domain = 1:32) |> 
      data_color(columns = away_def_rank, 
                 target_columns = away_def, 
                 palette = rev(hulk_pal), 
                 domain = 1:32) |> 
      data_color(columns = home_off_rank, 
                 target_columns = home_off, 
                 palette = rev(hulk_pal), 
                 domain = 1:32) |> 
      data_color(columns = home_def_rank, 
                 target_columns = home_def, 
                 palette = rev(hulk_pal), 
                 domain = 1:32) |> 
      # Add border lines around column
      tab_style(
        style = list(
          cell_borders(
            side = c("left", "right"),
            color = "black",
            weight = px(2)
          )
        ),
        locations = cells_body(
          columns = c(type_text)
        )
      ) |> 
      # Add Footnote and top and bottom borders around footnote
      tab_footnote(
        footnote = paste0("Rate at which a series starting on 1st down", 
                          " earns a new 1st down or TD on that series."),
        locations = cells_body(
          columns = c(type_text),
          rows = type_text == "Series Conversion Rate"
        )
      ) |>
      tab_footnote(
        footnote = md("**Note:** NFL rank shown in parentheses (x).")
      ) |> 
      tab_style(
        style = list(
          cell_text(style = "italic", 
                    size = px(12))
        ),
        locations = cells_footnotes()
      ) |>
      tab_style(
        style = list(
          cell_borders(
            side = c("top"),
            color = "gray80",
            weight = px(1)
          )
        ),
        locations = cells_source_notes()
      ) |>
      tab_options(footnotes.padding = px(0)
      ) |> 
      tab_source_note(
        source_note = md(
          sprintf(
            "<div style=\"width: 100%%; display: table;\">
               <div style=\"display: table-row\">
               <div style=\"width: 40%%; display: table-cell;\">
                 <img src=\"https://github.com/Josephhero/Jefe-Logo/raw/main/Jefe%%20Logo%%20Skyline.png\" style=\"height:35px;\">
               </div>
               <div style=\"display: table-cell;vertical-align: middle;text-align: right; font-size: 0.9em;\">
                 Data: nflverse.com & ESPN via {espnscrapeR}<br>Updated: %s
               </div>
             </div>
           </div>", 
            run_date  # Insert the formatted date and time here
          )
        )
      )
  )
  
  gtsave(stats_tab, 
         path = "./images", 
         filename = paste0("Stats.png"), 
         expand = c(5, 10, 0, 10))
  
}

