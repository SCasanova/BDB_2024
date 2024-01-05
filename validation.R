library(tidyverse)
library(arrow)
library(data.table)

pc <- read_parquet('pitch_control/pc-week9-3.parquet')

tackler_info <- tackles_preprocess(read_parquet('clean_data/week9.parquet')) %>% 
  select(game = gameId, play = playId, frame = frameId, tackler = nflId, mu_x, mu_y, rad_dir, scal_x, scal_y, radio, min_dist)

full_info <- 
  left_join(pc, tackler_info, by = c('game', 'play', 'frame'))

players <- read_parquet('clean_data/week9.parquet')

value <- read_parquet('value_map/value-week1-2.parquet')



# Field -------------------------------------------------------------------

plot_field <- function(field_color="#ffffff", line_color = "#212529", number_color = "#adb5bd") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      legend.title.align = 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 10,
      colour = number_color,
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 10,
      colour = number_color, 
    )
  return(field)
}


# Function that asigns colors to the teams
fetch_team_colors <- function(team_colors_=NULL, h_team_, a_team_, diverge_=FALSE) {
  colors_url <- "https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"
  
  if (is.null(team_colors_)) {
    team_colors_ <- suppressMessages(readr::read_tsv(colors_url))
  }
  
  h_team_color1 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color1)
  h_team_color2 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color2)
  a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color1)
  a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color2)
  
  if (diverge_ == TRUE) {
    h_team_color1_family <- team_colors_ %>% filter(teams == h_team_) %>% select(color1_family) %>% pull()
    a_team_color1_family <- team_colors_ %>% filter(teams == a_team_) %>% select(color1_family) %>% pull()
    
    if (h_team_color1_family == a_team_color1_family) {
      a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% select(color2) %>% pull()
      a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% select(color1) %>% pull()
    }
  }
  
  df_colors <- tibble(
    home_1 = h_team_color1, home_2 = h_team_color2, away_1 = a_team_color1, away_2 = a_team_color2
  )
  return(df_colors)
}



# Testing -----------------------------------------------------------------


library(gganimate)

play_pc <- pc %>% 
  filter(game == '2022110609' & play == 271 & frame == 25)

players_frame <- players %>%  
  filter(gameId == '2022110609' & playId == 271 & frameId == 25) %>% 
  rename(frame = frameId)


plot_field()+
  # geom_contour(data = value_frame, aes(x,y,z=value), breaks = seq(0.2,1,0.2))+
  geom_raster(data = play_pc, aes(x,y, fill = PC), interpolate = F, alpha = 0.6)+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('red', alpha('white', 0), '#4f9943'))+
  geom_segment(data = players_frame, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))+
  geom_point(data = players_frame, aes(x,y, color = club), size = 9)+
  geom_text(
    data = players_frame,
    mapping = aes(x = x, y = y, label = jerseyNumber),
    size = 4.5
  )

play_tackler <- tackler_info %>% 
  filter(game == '2022110609' & play == 271) %>% 
  mutate(eigen_v1_1 = cos(rad_dir)*scal_y, 
         eigen_v1_2 = -sin(rad_dir)*scal_y,
         eigen_v2_1 = -sin(rad_dir)*scal_x, 
         eigen_v2_2 = -cos(rad_dir)*scal_x,
         f1_1 = mu_x-sqrt((2*max(scal_x, scal_y))^2 -  (2*min(scal_x, scal_y))^2)*sin(rad_dir),
         f1_2 = mu_y-sqrt((2*max(scal_x, scal_y))^2 -  (2*min(scal_x, scal_y))^2)*cos(rad_dir),
         f2_1 = mu_x+sqrt((2*max(scal_x, scal_y))^2 -  (2*min(scal_x, scal_y))^2)*sin(rad_dir),
         f2_2 = mu_y+sqrt((2*max(scal_x, scal_y))^2 -  (2*min(scal_x, scal_y))^2)*cos(rad_dir)
         ) 

value_frame <- value %>% 
  filter(game == '2022110609' & play == 271& frame == 25)


pc_area <- play_pc %>% 
  left_join(play_tackler, by = c('game', 'play', 'frame')) %>% 
  filter(sqrt((x-f1_1)^2 + (y-f1_2)^2) + sqrt((x-f2_1)^2 + (y-f2_2)^2) <= 4*max(scal_x, scal_y))

pc_area %>% 
  filter(frame == 27) %>% 
  select(x,y,PC) %>% 
  inner_join(value_frame, by = c('x','y')) %>% 
  mutate(tackle_prob = PC*value) %>% 
  summarise(prob = sum(tackle_prob)) 
  

plot_field()+
  geom_contour(data = value_frame, aes(x,y,z=value), breaks = seq(0.2,1,0.2))+
  geom_raster(data = play_pc %>% filter(frame == 27), aes(x,y, fill = PC), interpolate = F, alpha = 0.6)+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('red', alpha('white', 0), '#4f9943'))+
  scale_x_continuous(limits = c(0,120))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  geom_segment(data = players_frame, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))+
  geom_point(data = players_frame, aes(x,y, color = club), size = 9)+
  geom_text(
    data = players_frame,
    mapping = aes(x = x, y = y, label = jerseyNumber),
    size = 4.5
  )+
  geom_point(data = pc_area %>% filter(frame == 27), aes(x,y))

library(ggforce)

ggplot(data = play_tackler %>% filter(frame==27))+
  geom_ellipse(aes(x0 = mu_x, y0 = mu_y, a = 2*scal_y, b =2*scal_x, angle = -rad_dir))+
  coord_fixed()+
  geom_segment(data = play_tackler %>% filter(frame==6), aes(x = mu_x, y = mu_y, xend = mu_x+2*eigen_v1_1, yend = mu_y+2*eigen_v1_2),
               arrow = arrow(length = unit(2, "mm")))+
    geom_segment(data = play_tackler %>% filter(frame==6), aes(x = mu_x, y = mu_y, xend = mu_x+2*eigen_v2_1, yend = mu_y+2*eigen_v2_2),
                 arrow = arrow(length = unit(2, "mm")))+
  geom_point(aes(f1_1, f1_2))+
  geom_point(aes(f2_1, f2_2))






