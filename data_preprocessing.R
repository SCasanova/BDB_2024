
library(tidyverse)
library(nflverse)

# We read de data of plays and tackles
plays <- read_csv('data/plays.csv')
tackles <- read_csv('data/tackles.csv')
week <- read_csv('data/tracking_week_1.csv')

# We load the data of nflverse
plays_pbp <- load_pbp(2022) %>% 
  select(old_game_id, play_id, pass, rush, sack, qb_scramble, play_type) %>% 
  filter((pass == 1 | (rush == 1 & qb_scramble == 0)) & sack == 0) %>% 
  mutate(game_id = as.numeric(old_game_id))

# Function that determines the location of the ball carrier in the tracking data
location_carrier <- function(week){
  ball_carrier <- plays %>% 
    select(gameId, playId, ballCarrierId) %>% 
    rename(nflId = ballCarrierId) %>% 
    inner_join(week, c('gameId', 'playId', 'nflId')) %>% 
    select(gameId, playId, frameId, x_car = x, y_car = y)
  ball_carrier
}


tackles_preprocess <- function(week){
  week <- left_join(week, tackles, c('gameId', 'playId', 'nflId'))
  ball_carrier <- location_carrier(week)
  
  data_tackles <- week %>%
    # Agregar datos por jugada (no tener sacks ni scrambles)
    inner_join(plays_pbp, by = c('gameId' = 'game_id', 'playId' = 'play_id'))%>%
    # Crear etiqueta de inicio de filtro
    mutate(initial = ifelse(
      event == 'handoff'| event == 'run' | event == 'lateral' | event == 'pass_arrived' |
      event == 'pass_outcome_caught', TRUE, FALSE
    )) %>% 
    group_by(gameId, playId, nflId) %>% 
    # Por cada jugada y jugador, mantener los frames mayores al initial y 
    # menores a la tacleada
    filter(any(initial)) %>% 
    filter(row_number() >= min(which(initial))) %>% 
    ungroup() %>%
    group_by(gameId, playId) %>% 
    # Por cada jugada y jugador nos quedamos con los frames en donde hubo una tacleada
    filter(any(event == "tackle")) %>% 
    ungroup() %>% 
    # Calculamos la distancia al corredor
    left_join(ball_carrier, by = c('gameId', 'playId', 'frameId')) %>% 
    group_by(gameId, playId, frameId) %>% 
    mutate(distance_to_carrier = sqrt((x-x_car)^2 + (y-y_car)^2)) %>% 
    ungroup() %>%
    group_by(gameId, playId, nflId) %>% 
    mutate(min_dist = ifelse(distance_to_carrier == min(distance_to_carrier), 1, 0)) %>% 
    ungroup() %>% 
    mutate(action =
             case_when(
               event == "tackle" & tackle == 1 ~ 1,
               event == "tackle" & assist == 1 ~ 1,
               pff_missedTackle == 1 & min_dist == 1 ~ 0,
               T ~ NA
             ))
  data_tackles
}

# Execution of the pipeline
data_tackles <- tackles_preprocess(week)
# Play example
play_ex <- filter(data_tackles, gameId == 2022090800, playId == 1102)
