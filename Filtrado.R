library(tidyverse)
library(nflverse)


week1 <- read_csv('data/tracking_week_1.csv')
plays <- read_csv('data/plays.csv')

plays_pbp <- load_pbp(2022) %>% 
  select(old_game_id, play_id, pass, rush, sack, qb_scramble, play_type) %>% 
  filter((pass == 1 | (rush == 1 & qb_scramble == 0)) & sack == 0) %>% 
  mutate(game_id = as.numeric(old_game_id))

ball_loc <- week1 %>% 
  filter(club == 'football') %>% 
  group_by(gameId, playId, frameId) %>% 
  select(x_ball = x, y_ball = y) %>% 
  ungroup()

data_ball <- week1 %>% 
  # Datos de modificacion para estandarizar
  mutate(ToLeft = playDirection == "left",
         x = ifelse(ToLeft, 120-x, x) - 10, 
         y = ifelse(ToLeft, 160/3-y, y),
         rad_dir = dir*pi/180,
         vector_x = -s*sin(rad_dir)+x,
         vector_y = -s*cos(rad_dir)+y) %>% 
  left_join(ball_loc, by = c('gameId', 'playId', 'frameId')) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(distance_to_ball = sqrt((x-x_ball)^2 + (y-y_ball)^2)) %>% 
  ungroup() %>% 
  # Agregar datos por jugada (no tener sacks ni scrambles)
  inner_join(plays_pbp, by = c('gameId' = 'game_id', 'playId' = 'play_id')) %>% 
  # Manipulacion de variables para PC
  mutate(radio = radius(distance_to_ball),
         speed_ratio = s^2/(13.6)^2,
         scal_x = (radio+(radio*speed_ratio))/2,
         scal_y = (radio-(radio*speed_ratio))/2,
         mu_x = -s*sin((dir*pi/180))*0.5+x,
         mu_y = -s*cos((dir*pi/180))*0.5+y,
         # Crear etiqueta de inicio de filtro
         initial = ifelse(
           event == 'pass_arrived' | event == 'handoff' | event == 'run' | event == 'lateral' | event == 'pass_outcome_caught', TRUE, FALSE
         )) %>% 
  # head(100) %>% 
  group_by(gameId, playId, nflId) %>% 
  # Por cada jugada y jugador, mantener los frames mayores al initial y 
  # menores a la tacleada -1
  filter(any(initial)) %>% 
  filter(row_number() >= min(which(initial))) %>% 
  ungroup()


