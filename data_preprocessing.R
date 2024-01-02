
library(tidyverse)
library(nflverse)
library(stringr)
library(clock)

# We read de data of plays and tackles
plays <- read_csv('data/plays.csv')
tackles <- read_csv('data/tackles.csv')
#week <- read_csv('data/weeks/tracking_week_1.csv')
players <- read_csv('data/players.csv')

# Preprocess of player data
# Split height column into foot and inch
players[c('foot', 'inch')] <- str_split_fixed(players$height, '-', 2)

players <- players %>% 
  # Convert the height in yards
  mutate(height = as.numeric(foot)/3+as.numeric(inch)/36) %>% 
  # Select columns of interest
  select(nflId, height, weight, position, displayName)

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
# and data from the situation of the play
location_carrier <- function(week){
  ball_carrier <- plays %>% 
    select(gameId, playId, ballCarrierId, quarter, down, yardsToGo, gameClock, 
           yardlineNumber, absoluteYardlineNumber) %>% 
    rename(nflId = ballCarrierId) %>% 
    inner_join(week, c('gameId', 'playId', 'nflId')) %>% 
    left_join(players, by = "nflId") %>% 
    select(gameId, playId, frameId, x_car = x, y_car = y, s_car = s, a_car = a, 
           o_car = o, dir_car = dir, height_car = height, weight_car = weight, 
           position_car = position, name_car = displayName.x, quarter, down, 
           yardsToGo, gameClock, yardlineNumber, absoluteYardlineNumber)
  ball_carrier
}

# Function that determines the cumulative distance of a player in a play for an
# specific frame
covered_distance_play <- function(game_id, play_id, nfl_id, frame_id){
  dist_play <- week %>% filter(gameId == game_id, playId == play_id, nflId == nfl_id, frameId <= frame_id) %>% 
    group_by(gameId, playId, nflId) %>% 
    summarise(traveled_play = sum(dis))
  dist_play$traveled_play
}

# Function that determines the cumulative distance of a player in a game for an
# specific frame
covered_distance_game <- function(game_id, play_id, nfl_id, frame_id){
  dist_game <- week %>% filter(gameId == game_id, nflId == nfl_id) %>% 
    filter(as.numeric(play_frame_id) <= as.numeric(paste0(play_id,str_pad(frame_id, 3, pad = "0")))) %>% 
    group_by(gameId, nflId) %>% 
    summarise(traveled_game = sum(dis))
  dist_game$traveled_game
}

tackles_preprocess <- function(week){
  ball_carrier <- location_carrier(week)
  
  data_tackles <- week %>%
    # Agregar datos por jugada (no tener sacks ni scrambles)
    inner_join(plays_pbp, by = c('gameId' = 'game_id', 'playId' = 'play_id')) %>%
    # Crear etiqueta de inicio de filtro
    mutate(initial = ifelse(
      event == 'handoff'| event == 'run' | event == 'lateral'|
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
    filter(any(event %in% c("tackle", "touchdown"))) %>% 
    ungroup() %>% 
    # Filtramos a los jugadores involucrados en el tacleo
    filter(!is.na(tackle)) %>% 
    # Agregamos información del peso y la altura de los tacleadores
    left_join(select(players, nflId, height, weight, position), by = "nflId") %>% 
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
               tackle == 1 & min_dist == 1 ~ 1,
               assist == 1 & min_dist == 1 ~ 1,
               pff_missedTackle == 1 & min_dist == 1 ~ 0,
               T ~ NA
             )) %>% 
    mutate(dist_t = ifelse(playDirection == "left", absoluteYardlineNumber - 10, 
                           110 - absoluteYardlineNumber)) %>%
    # Split gameClock column into minute, second and milisecond
    separate(gameClock, c('minute', 'second', 'milisecond'), ":") %>%
    # Calculate the time remaining of the game
    mutate(time_remaming = 
             case_when(
               quarter == 1 ~ 45 + (as.numeric(minute)+as.numeric(second)/60),
               quarter == 2 ~ 30 + (as.numeric(minute)+as.numeric(second)/60),
               quarter == 3 ~ 15 + (as.numeric(minute)+as.numeric(second)/60),
               T ~ as.numeric(minute)+as.numeric(second)/60
             )) 
    # # Select the initial event and the moment  of the tackle contact
    # filter(initial | !is.na(action)) %>% 
    # mutate(dist_h = abs(x-x_car), dist_v = abs(y-y_car), dif_w = weight-weight_car)
  
  # We filter the frames of a initial event
  initial_frames <- data_tackles %>% 
    filter(initial == TRUE) %>% 
    # We sort the frame from the last recorded 
    group_by(gameId, playId, nflId) %>% 
    arrange(desc(frameId)) %>% 
    ungroup() %>% 
    # We select the columns of interest
    select(gameId, playId, nflId, frameId, s_in = s, a_in = a, o_in = o, dir_in = dir,
           distance_to_carrier_i = distance_to_carrier, s_car_in = s_car,
           a_car_in = a_car, dist_h_in = dist_h, dist_v_in = dist_v, 
           dif_w, quarter, down, yardsToGo, time_remaming,
           dist_t, tackle_in = action) %>% 
  # We eliminate duplicated records 
    distinct(gameId, playId, nflId, .keep_all = TRUE)
    
  # We filter the frames of contact events
  contact_frames <- data_tackles %>% 
    filter(initial == FALSE | is.na(initial) ) %>% 
    select(gameId, playId, nflId, frameId, s, a, o, dir, distance_to_carrier, s_car, a_car,
           dist_h, dist_v, action) %>% 
    rename(tackle = action) %>% 
    # We eliminate duplicated records 
    distinct(gameId, playId, nflId, .keep_all = TRUE)
  
  # We agregate the distance cumulative distance in the play for the tackler
  contact_frames$distance_play <- contact_frames %>% 
    select(game_id = gameId, play_id = playId, nfl_id = nflId, frame_id = frameId) %>% 
    pmap_dbl(covered_distance_play) 
  
  # We agregate the distance cumulative distance in the game for the tackler
  contact_frames$distance_game <- contact_frames %>% 
    select(game_id = gameId, play_id = playId, nfl_id = nflId, frame_id = frameId) %>% 
    pmap_dbl(covered_distance_game) 
  
  data_tackles <- left_join(initial_frames, contact_frames, by = c('gameId', 'playId', 'nflId')) %>% 
    mutate(s = ifelse(is.na(s), s_in, s), 
           a = ifelse(is.na(a), a_in, a),
           o = ifelse(is.na(o), o_in, o),
           dir = ifelse(is.na(dir), dir_in, dir),
           distance_to_carrier = ifelse(is.na(distance_to_carrier), 
                                        distance_to_carrier_i, distance_to_carrier),
           s_car = ifelse(is.na(s_car), s_car_in, s_car),
           a_car = ifelse(is.na(a_car), a_car_in, a_car),
           dist_h = ifelse(is.na(dist_h), dist_h_in, dist_h),
           dist_v = ifelse(is.na(dist_v), dist_v_in, dist_v),
           tackle = ifelse(is.na(tackle), tackle_in, tackle)) %>%
    subset(select = -c(frameId.x, frameId.y))
  
  # Plays with a tackle right after the catch
  initial_frames_distance <- initial_frames %>% filter(!is.na(tackle_in)) %>%
    select(gameId, playId, nflId, frameId)
  # We aggregate the distance cumulative distance in the play for the tackler 
  initial_frames_distance$distance_play_i <- initial_frames_distance %>%  
    select(game_id = gameId, play_id = playId, nfl_id = nflId, frame_id = frameId) %>% 
    pmap_dbl(covered_distance_play)
  # We aggregate the distance cumulative distance in the game for the tackler
  initial_frames_distance$distance_game_i <- initial_frames_distance %>% 
    select(game_id = gameId, play_id = playId, nfl_id = nflId, frame_id = frameId) %>% 
    pmap_dbl(covered_distance_game)
  # We modify the columns distance_game and distance_play
  data_tackles <- left_join(data_tackles, initial_frames_distance, by = c('gameId', 'playId', 'nflId')) %>% 
    mutate(distance_game = ifelse(is.na(distance_game), distance_game_i, distance_game),
           distance_play = ifelse(is.na(distance_play), distance_play_i, distance_play)) %>% 
    subset(select = -c(tackle_in, frameId, distance_play_i, distance_game_i))
  
  data_tackles 
}

file_names <- list.files("data/", full.names = TRUE)[5:13]

cont <- 1
for(f in file_names){
  week <- read.csv(f)
  # Include variables of interest and for manipulation of the data
  week <- left_join(week, tackles, c('gameId', 'playId', 'nflId')) %>% 
    mutate(play_frame_id = as.numeric(paste0(playId,str_pad(frameId, 3, pad = "0"))))
  # Execution of the pipeline
  data_tackles <- tackles_preprocess(week)
  write.csv(data_tackles, paste0("datosProcesados/week_", cont, ".csv"), row.names = FALSE)
  cont <- cont + 1
}


# Play example week 1
# Play with a fail tackle and a tackle
# play_ex <- filter(data_tackles, gameId == 2022090800, playId == 1102)
# Play with a duplicate contact frame for a player
# play_ex <- filter(data_tackles, gameId == 2022091100, playId == 2491)
# Play with a tackle right after the catch
#play_ex <-  filter(prueba, gameId == 2022091100, playId == 1769)
# # Play with more than one initial event for a player
# play_ex <- filter(data_tackles, gameId == 2022091102, playId == 1493)

# Game example
# game_ex <- filter(week, gameId == 2022090800, nflId == 42816)

# Revisamos la distribución de la distancia entre el tacleador y el corredor
# al momento de la tacleada

# data_tackles %>% select(distance_to_carrier, action) %>% filter(action == 1) %>% 
#   ggplot(aes(sample = distance_to_carrier))+
#   geom_qq(distribution = stats::qunif) + xlab("f") + ylab("yardas")

