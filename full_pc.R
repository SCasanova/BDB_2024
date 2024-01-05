library(tidyverse)
library(arrow)
library(nflverse)


# Get offensive and defensive teams for each play
plays <- read_csv('data/plays.csv') %>% 
  select(game=gameId, play=playId, club = possessionTeam) %>% 
  mutate(team = 'off') %>% 
  bind_rows(
    read_csv('data/plays.csv') %>% 
      select(game = gameId, play = playId, club = defensiveTeam) %>% 
      mutate(team = 'def')
  )


# Get defenseive team
# defense <- load_pbp(2022) %>% 
#   select(game = old_game_id, play =play_id, defteam) %>% 
#   mutate(game = as.numeric(game))

# Get the tackling player
defenders <- read_csv('data/tackles.csv') %>% 
  select(game = gameId, play = playId, nflId)

week_pitch_control <- function(week_to_get, defensive = F){
  flag <- ifelse(defensive, 'def-', '')
  # Tracking data for the week
  week_data <- read_csv(glue::glue('data/tracking_week_{num}.csv', 
                                   num = str_sub(week_to_get, start=5)))
  
  # Ball carrier ID to eliminate them from PC model
  bc <- read_csv('data/plays.csv') %>% 
    select(gameId, playId, player = ballCarrierId)
  
  # Players and their club (for this week)
  players <-week_data %>% 
    select(nflId, club) %>% 
    distinct()
  
  # get all the file partitions for this week
  filenames <- list.files(glue::glue("./partitions/{week_to_get}/"), 
                          full.names = TRUE)
  
  for(f in filenames){
    # Read file
    df <- read_parquet(f) %>% 
      # Eliminte the BC from each play
      anti_join(bc, by = c('game' = 'gameId', 'play' = 'playId', 'player' = 'player')) %>%
      # Add the club variable for each player
      left_join(players, by = c('player' = 'nflId')) %>% 
      # Indicate which team is offense and defense for each play
      left_join(plays, by = c('game', 'play', 'club'))

  # if(defensive){
  #   df <- df %>% 
  #     inner_join(defenders, by = c('game', 'play', 'player' = 'nflId'))
  # }
    
    
    if(defensive){
      # pc <- df %>%
      #   group_by(game,play,frame,x,y,club) %>% 
      #   summarise(suma = sum(influ)) %>% 
      #   ungroup() %>% 
      #   pivot_wider(names_from = team, values_from = suma) %>% 
      #   mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
      #                 PC = ifelse(home+away > 1 , 1 , home+away)) %>% 
      #   select(game,play, frame, x,y, PC)
    } else{
      pc <- df %>%
        group_by(game,play,frame,x,y,team) %>% 
        # get the influence sum for each coordinate for each team
        summarise(suma = sum(influ)) %>% 
        ungroup() %>% 
        # Wide format so only onw row per frame per coordinate
        pivot_wider(names_from = team, values_from = suma) %>% 
        # Compute PC by subtractin off from def (pure def = 1 thanks to plogis)
        mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
               PC = plogis(def-off)) %>% 
        select(game,play, frame, x,y, PC) %>% 
        group_by(game,play, frame) %>% 
        # Fill remaining coordintes with 0.5 for each frame
        complete(
          x = 0:100,
          y = 0:54,
          fill = list(PC= 0.5)
        ) %>% 
        ungroup()
    }
    
    
    folder <- ifelse(defensive, 'def_pc', 'pitch_control')
      
      write_parquet(pc, glue::glue('{folder}/{flag}pc-{week_to_get}{rest}', 
                                   rest = str_sub(f, start = -10)))
    
  }
}



week_pitch_control('week1')
week_pitch_control('week2')
week_pitch_control('week3')
week_pitch_control('week4')
week_pitch_control('week5')
week_pitch_control('week6')
week_pitch_control('week7')
week_pitch_control('week8')
week_pitch_control('week9')


