# library(tidyverse)
# library(nflverse)
# library(ggplot2)
# library(shadowtext)


# f_year <- get_current_season()
# f_week <- get_current_week()
# f_team <- "KC"

# sched <- load_schedules(seasons = f_year)

background_function <- function(f_sched = sched, 
                                f_year = get_current_season(), 
                                f_week = get_current_week(use_date = TRUE), 
                                f_team = "KC"){
  game_data <- f_sched |> 
    filter(week == f_week, home_team == f_team | away_team == f_team) |> 
    left_join(select(load_teams(), team_abbr, home_team_color = team_color), 
              by = c("home_team" = "team_abbr")) |> 
    left_join(select(load_teams(), team_abbr, away_team_color = team_color), 
              by = c("away_team" = "team_abbr")) |> 
    mutate(gametime_format = str_squish(
      sub("^0", "", format(as.POSIXct(paste0(gameday, " ", gametime)), "%a, %b %e %l:%M %p"), '%r')
      ), .after = gametime) |> 
    left_join(select(
      read_csv("https://raw.githubusercontent.com/Josephhero/NFL-Stadiums/refs/heads/main/nfl_stadiums_2024.csv"), 
      team_abbr, stadium_id, city, state_abbr, country), 
      by = c("home_team" = "team_abbr", "stadium_id")) |> 
    mutate(gametime_label = case_when(
      location == "Neutral" & country == "America" ~ paste0(gametime_format, " EST in ", city, ", ", state_abbr), 
      location == "Neutral" & country != "America" ~ paste0(gametime_format, " EST in ", city, ", ", country), 
      TRUE ~ paste0(gametime_format, " EST")
    ))
  
  gh_url <- "https://raw.githubusercontent.com/Josephhero/NFL-Helmets/main/Helmet%20Images/"
  home_team <- game_data$home_team[1]
  away_team <- game_data$away_team[1]
  home_team_color <- game_data$home_team_color[1]
  away_team_color <- game_data$away_team_color[1]
  home_helmet_url <- paste0(gh_url, home_team, "_left.png")
  away_helmet_url <- paste0(gh_url, away_team, "_right.png")
  
  # Code for the background image
  # Add Google Font (replace "Roboto" with your desired font)
  sysfonts::font_add_google(name = "Barlow Condensed", family = "barlow_condensed")
  # sysfonts::font_add_google(name = "Barlow", family = "barlow_condensed")
  
  # Enable showtext to render the text properly
  showtext::showtext_auto()
  # Create a sample dataset
  df <- data.frame(
    x = 0:10,
    y = 0:10
  )
  
  # Create the plot
  (
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_polygon(data = data.frame(
        x = c(0, 4, 6, 0),
        y = c(0, 0, 10, 10)
      ), aes(x = x, y = y), 
      fill = away_team_color, color = "black") +  
      geom_polygon(data = data.frame(
        x = c(10, 4, 6, 10),
        y = c(0, 0, 10, 10)
      ), aes(x = x, y = y), 
      fill = home_team_color, color = "black") + 
      geom_nfl_logos(data = data.frame(
        x = 2.6, 
        y = 4.5), 
        aes(team_abbr = away_team, x = x, y = y), 
        height = 0.35, 
        alpha = 0.3,
        # color = "b/w"
      ) + 
      geom_nfl_logos(data = data.frame(
        x = 7.75, 
        y = 4.5), 
        aes(team_abbr = home_team, x = x, y = y), 
        height = 0.35, 
        alpha = 0.3, 
        # color = "b/w"
      ) + 
      ggimage::geom_image(data = data.frame(
        x = 2.65,
        y = 8.5,
        image = away_helmet_url),
        aes(x = x, y = y, image = image), 
        size = 0.16) +
      ggimage::geom_image(data = data.frame(
        x = 7.35,
        y = 8.5,
        image = home_helmet_url),
        aes(x = x, y = y, image = image), 
        size = 0.16) +
      shadowtext::geom_shadowtext(data = data.frame(x = 5, y = 8.3), 
                                  aes(x = x, y = y, label = "@"), 
                                  color = "white", 
                                  bg.color = "black",
                                  size = 50, 
                                  # family = "barlow_condensed"
      ) + 
      geom_label(data = 
                   data.frame(
                     x = 5, 
                     y = 9.6), 
                 aes(label = game_data$gametime_label), 
                 size = 35, 
                 color = "#daa520", 
                 fill = "gray10",
                 fontface = "bold", 
                 family = "barlow_condensed",
                 label.size = 1.6, 
                 label.padding = unit(0.08, "lines"), 
                 label.r = unit(0.05, "lines")
      ) + 
      geom_label(data = 
                   data.frame(
                     x = 5, 
                     y = 8.95), 
                 aes(label = paste0("Week ", f_week)), 
                 size = 35, 
                 color = "#daa520", 
                 fill = "gray10",
                 fontface = "bold", 
                 family = "barlow_condensed",
                 label.size = 1.6, 
                 label.padding = unit(0.08, "lines"), 
                 label.r = unit(0.05, "lines")
      ) + 
      theme_void()
  )
  
  ggsave(p, 
         filename = paste0("./images/Background.png"), 
         height = 12, 
         width = 10)
  
}
