library(tidyverse)
library(nflverse)


# Radius function that stabilizes at 10 when distance >= 17
radius <- function(distance){ 
  purrr::map_dbl(
    distance,
    function(x){
      if(x <= 17){
        exp(x*0.1144653)+3
      } else{
        10
      }
    }
  )
}




data_process <- function(week_data){
  
  plays <- read_csv('data/plays.csv')
  
  plays_pbp <- load_pbp(2022) %>% 
    select(old_game_id, play_id, pass, rush, sack, qb_scramble, play_type) %>% 
    filter((pass == 1 | (rush == 1 & qb_scramble == 0)) & sack == 0) %>% 
    mutate(game_id = as.numeric(old_game_id))
  
  ball_loc <- week_data %>% 
    mutate(ToLeft = playDirection == "left",
           x = ifelse(ToLeft, 120-x, x) - 10, 
           y = ifelse(ToLeft, 160/3-y, y)) %>% 
    left_join(plays, by =c('gameId', 'playId')) %>% 
    filter(ballCarrierId == nflId) %>% 
    select(gameId, playId, frameId, x_car = x, y_car = y)
  
  
  data_ball <- week_data %>% 
    filter(club != 'football') %>% 
    # Standardize all plays to the right
    mutate(ToLeft = playDirection == "left",
           x = ifelse(ToLeft, 120-x, x) - 10, 
           y = ifelse(ToLeft, 160/3-y, y),
           rad_dir = dir*pi/180,
           vector_x = -s*sin(rad_dir)+x,
           vector_y = -s*cos(rad_dir)+y) %>% 
    # Join ball location (maybe do with ballcarrier instead)
    left_join(ball_loc, by = c('gameId', 'playId', 'frameId')) %>% 
    # group_by(gameId, playId, frameId) %>% 
    mutate(distance_to_ball = sqrt((x-x_car)^2 + (y-y_car)^2)) %>% 
    # ungroup() %>% 
    # Add play info to remove sacks and scrambles
    inner_join(plays_pbp, by = c('gameId' = 'game_id', 'playId' = 'play_id')) %>% 
    # Create variables for pitch control
    mutate(radio = radius(distance_to_ball),
           speed_ratio = s^2/(15)^2,
           scal_x = (radio+(radio*speed_ratio))/2,
           scal_y = (radio-(radio*speed_ratio))/2,
           mu_x = -s*sin((dir*pi/180))*0.5+x,
           mu_y = -s*cos((dir*pi/180))*0.5+y,
           # Tags for when our desired frames start (ballcarrier posession)
           initial = ifelse(
             event == 'handoff' | event == 'run' | event == 'lateral' | event == 'pass_outcome_caught', TRUE, FALSE
           )) %>% 
    # head(100) %>% 
    group_by(gameId, playId, nflId) %>% 
    # Keep the frames after our "inital" tag
    filter(any(initial)) %>% 
    filter(row_number() >= min(which(initial))) %>% 
    ungroup()
  
  data_ball
}



# Week Processing ---------------------------------------------------------


week1 <- read_csv('data/tracking_week_1.csv')
week1_proc <- data_process(week1) %>% 
  write_parquet('clean_data/week1.parquet')

week2 <- read_csv('data/tracking_week_2.csv')
week2_proc <- data_process(week2) %>% 
  write_parquet('clean_data/week2.parquet')

week3 <- read_csv('data/tracking_week_3.csv')
week3_proc <- data_process(week3) %>% 
  write_parquet('clean_data/week3.parquet')

week4 <- read_csv('data/tracking_week_4.csv')
week4_proc <- data_process(week4) %>% 
  write_parquet('clean_data/week4.parquet')

week5 <- read_csv('data/tracking_week_5.csv')
week5_proc <- data_process(week5) %>% 
  write_parquet('clean_data/week5.parquet')

week6 <- read_csv('data/tracking_week_6.csv')
week6_proc <- data_process(week6) %>% 
  write_parquet('clean_data/week6.parquet')

week7 <- read_csv('data/tracking_week_7.csv')
week7_proc <- data_process(week7) %>% 
  write_parquet('clean_data/week7.parquet')

week8 <- read_csv('data/tracking_week_8.csv')
week8_proc <- data_process(week8) %>% 
  write_parquet('clean_data/week8.parquet')

week9 <- read_csv('data/tracking_week_9.csv')
week9_proc <- data_process(week9) %>% 
  write_parquet('clean_data/week9.parquet')




