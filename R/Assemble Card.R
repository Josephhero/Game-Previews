# library(tidyverse)
# library(cowplot)
# library(ggforce)

# Data-----

card_function <- function(f_sched = sched, 
                          f_year = get_current_season(), 
                          f_week = get_current_week(use_date = TRUE), 
                          f_team = "KC"){
  
  game_data <- f_sched |> 
    filter(week == f_week, home_team == f_team | away_team == f_team)
  
  home_team <- game_data$home_team[1]
  away_team <- game_data$away_team[1]
  
  path <- "images/"
  base_name <- paste0(f_year, " Week ", f_week, " ", away_team, " at ", home_team)
  
  background_img <- EBImage::readImage(paste0(path, "Background.png"))
  overview_img <- EBImage::readImage(paste0(path, "Overview.png"))
  stats_img <- EBImage::readImage(paste0(path, "Stats.png"))
  
  game_card1 <- 
    #Create blank canvas
    cowplot::ggdraw(xlim = c(0, 10), ylim = c(0, 12)) + 
    #Add background plot
    cowplot::draw_image(background_img, width = 10, height = 12) +
    #Add other plots
    cowplot::draw_image(overview_img, 
                        x = 5, 
                        y = 8,  
                        height = 1.9, 
                        width = 8, 
                        hjust = 0.5, 
                        vjust = 0.5,) + 
    cowplot::draw_image(stats_img, 
                        x = 5, 
                        y = 3.89,  
                        height = 6.57,
                        width = 6.37, 
                        hjust = 0.5, 
                        vjust = 0.5,) + 
    theme_void()
  
  game_card <- game_card1 +  
    ggforce::geom_shape(
      data = data.frame(
        x = c(0.97, 9.03, 9.03, 0.97),
        y = c(8.85, 8.85, 7.19, 7.19)), 
      mapping = aes(
        x = x, y = y
      ),
      # Use relative npc unit (values between 0 and 1)
      # This ensures that radius is not too large for your canvas
      radius = unit(0.3, 'cm'),
      color = "#daa520", 
      fill = "transparent",
      linewidth = 3
    ) + 
    ggforce::geom_shape(
      data = data.frame(
        x = c(1.8, 8.2, 8.2, 1.8),
        y = c(6.76, 6.76, 1.02, 1.02)), 
      mapping = aes(
        x = x, y = y
      ),
      # Use relative npc unit (values between 0 and 1)
      # This ensures that radius is not too large for your canvas
      radius = unit(0.3, 'cm'),
      color = "#daa520", 
      fill = "transparent",
      linewidth = 3
    ) + 
    geom_label() + 
    theme_void()
  
  ggsave(plot = game_card, 
         filename = paste0(path, base_name, ".png"), 
         height = 12, 
         width = 10
  )
  
  img_path <- paste0(path, base_name, ".png")
  
  # Load the image
  img <- image_read(img_path)
  
  flattened_img <- magick::image_flatten(img)
  trimmed_img <- magick::image_trim(flattened_img, fuzz = 1)
  
  image_write(trimmed_img, img_path)
  
}
