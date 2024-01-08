# Packages
library(tidyverse)
library(gganimate)
library(mvtnorm)

# plot image settings
options(repr.plot.width=20, repr.plot.height = 10)


# Data sets
# week <- read_csv('data/tracking_week_1.csv')
week <- read_parquet('clean_data/week1.parquet')


tackles <- read_csv('data/tackles.csv')
plays <- read_csv('data/plays.csv')
games <- read_csv('data/games.csv')

# Union between tracking data and tackles
week <- left_join(week, tackles, c('gameId', 'playId', 'nflId'))

# We choose the play we want to plot
game_id <- 2022091101
play_id <- 641

# Filter the game of interest
game_ <- filter(games, gameId == game_id)
# Filter the play of interest
play_ <- filter(plays, gameId== game_id, playId == play_id)
# Filter the frames of the game and play
df_track <- filter(week, gameId == game_id, playId == play_id)
# We save the direction of the play
play_direction_ <-  df_track %>% head(1) %>% 
  dplyr::pull(playDirection)
# We select the columns of interest
df_track <- df_track %>%
  filter((club == 'CLE' & jerseyNumber %in% c(27, 55, 75, 7,18, 85, 11)) | 
           (club == 'CAR' & jerseyNumber %in% c(21,24,25, 98,53, 26))) %>% 
  dplyr::select(x, y, vector_x, vector_y, s, dir, event, displayName, jerseyNumber, frameId, club, tackle, assist, pff_missedTackle)
# We create the vectors of movement of each player
df_track <- df_track %>%
  dplyr::mutate(
    # dir_rad = dir * pi / 180,
    # v_x = sin(dir_rad) * s,
    # v_y = cos(dir_rad) * s,
    # v_theta = atan(v_y / v_x),
    # v_theta = ifelse(is.nan(v_theta), 0, v_theta)
  ) %>%
  dplyr::select(frameId, event, team = club, jerseyNumber, displayName, x, y, s, v_x = vector_x, v_y = vector_y, tackle, assist, pff_missedTackle)


# Function that plot the field
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


# Determine the location of the line of scrimmage and the first down
if (play_direction_ == "left") {
  line_of_scrimmage = play_$yardlineNumber + 10
  to_go_line = line_of_scrimmage - play_$yardsToGo
} else {
  line_of_scrimmage = 110 - play_$yardlineNumber
  to_go_line = line_of_scrimmage + play_$yardsToGo
}

df_colors <- fetch_team_colors(h_team_ = game_$homeTeamAbbr, a_team_ = game_$visitorTeamAbbr, diverge_ = T)

# Plot by frame
play_frames <- plot_field() + 
  # line of scrimmage
  annotate(
    "segment",
    x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
    colour = "#0d41e1", size = 1.5
  ) +
  # 1st down marker
  annotate(
    "segment",
    x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
    colour = "#f9c80e", size = 1.5
  ) +
  # away team velocities
  geom_segment(
    data = df_track %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y, xend = v_x, yend = v_y),
    colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) + 
  # home team velocities
  geom_segment(
    data = df_track %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y, xend = v_x, yend = v_y),
    colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) +
  # away team locs and jersey numbers
  geom_point(
    data = df_track %>% dplyr::filter(team == game_$visitorTeamAbbr & jerseyNumber != 27),
    mapping = aes(x = x, y = y),
    fill = "#f8f9fa", colour = df_colors$away_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track %>% dplyr::filter(team == game_$visitorTeamAbbr),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$away_1, size = 4.5
  ) +
  # home team locs and jersey numbers
  geom_point(
    data = df_track %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y),
    fill = '#40b3ed', colour = '#000000',
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track %>% dplyr::filter(team == game_$homeTeamAbbr),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$home_2, size = 4.5, 
  ) +
  # ball
  geom_point(
    data = df_track %>% dplyr::filter(jerseyNumber == 27 & team == 'CLE'),
    mapping = aes(x = x, y = y),
    fill = "#e8c83f", colour = df_colors$away_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = df_track %>% dplyr::filter(jerseyNumber == 27 & team == 'CLE'),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$home_2, size = 4.5, 
  ) +
  # geom_point(
  #   data = df_track %>% dplyr::filter(team == "football"),
  #   mapping = aes(x = x, y = y),
  #   fill = "#935e38", colour = "#d9d9d9",
  #   shape = 21, alpha = 1, size = 4, stroke = 1
  # ) +
  # title 
  labs(title = play_$playDescription) +
  geom_text(data = tack_quality, 
            mapping = aes(
              x = 100, 
              y = 57, 
              label = paste0('Tackle Probability: ', round(prob(max_sum),3)*100,'%')),
            size = 7)+
  # animation stuff
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

# Number of frames in the play
play_length <- length(unique(df_track$frameId))
# Animation of the play
a_gif <- animate(
  play_frames,
  fps = 10, 
  nframes = 67,
  width = 800,
  end_pause = 0
)

anim_save('figures/play_example_reduced.gif', animation = last_animation())
