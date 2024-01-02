library(tidyverse)
library(arrow)
library(nflverse)


# Home team and away team to standardize team names

games <- read_csv('data/games.csv') %>% 
  select(week, homeTeamAbbr, visitorTeamAbbr)


# Get defenseive team
defense <- load_pbp(2022) %>%
  select(game = old_game_id, play =play_id, defteam) %>%
  mutate(game = as.numeric(game))

# Get the tackling player
defenders <- read_csv('data/tackles.csv') %>% 
  select(game = gameId, play = playId, nflId)

week_pitch_control <- function(week_to_get, individual = F){
  flag <- ifelse(defensive, 'def-', '')
  week_data <- read_parquet(glue::glue('clean_data/{week_to_get}.parquet'))
  
  bc <- read_csv('data/plays.csv') %>% 
    select(gameId, playId, player = ballCarrierId)
  
  players_to_get <-week_data %>% 
    group_by(gameId, playId, frameId) %>% 
    filter(distance_to_ball <= 6) %>% 
    ungroup() %>% 
    select(game = gameId,play =  playId, frame = frameId, player = nflId, team = club)
  
  individual_defenders <- players_to_get %>% 
    semi_join(defense, by = c('game', 'play', 'team' = 'defteam'))
    
  players <-week_data %>% 
    select(nflId, club) %>% 
    distinct()
  
  games_week <- games %>% 
    filter(`week` == str_sub(week_to_get, start=5) %>% as.numeric())
  
  teams <- inner_join(players, games_week, by =c('club'= 'homeTeamAbbr' )) %>% 
    mutate(team = club, club = 'home') %>% 
    bind_rows(
      inner_join(players, games_week, by =c('club'= 'visitorTeamAbbr' )) %>% 
        mutate(team =club,club = 'away')
    ) %>% 
    select(nflId, club, team)
  
  filenames <- list.files(glue::glue("./partitions/{week_to_get}/"), 
                          full.names = TRUE)
  
  for(f in filenames){
    df <- read_parquet(f) %>% 
      anti_join(bc, by = c('game' = 'gameId', 'play' = 'playId', 'player' = 'player')) %>% 
      left_join(teams, by = c('player' = 'nflId')) 
    
    if(individual){
      df <- df %>% 
        inner_join(players_to_get %>% select(-team), by = c('game', 'play','frame', 'player')) %>% 
        filter(influ >= 0.01)
    }
    
    
    if(individual){
      for(player in 1:nrow(individual_defenders)){
        player_info <-  individual_defenders[player, ]
        pc <-  df %>% 
          filter((game == player_info$game & 
                 play == player_info$play & 
                 frame == player_info$frame) & 
                   (player == player_info$player | team != player_info$team)) %>% 
          group_by(game,play,frame,x,y,club) %>% 
          summarise(suma = sum(influ)) %>% 
          ungroup() %>% 
          pivot_wider(names_from = club, values_from = suma) %>% 
          mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
                 home = ifelse('home' %in% names(.), home, 0),
                 away = ifelse('away' %in% names(.), home, 0),
                 PC = ifelse(home+away > 1 , 1 , home+away)) %>% 
          select(game,play, frame, x,y, PC) %>% 
          mutate(defender = player_info$player)
      }
      pc <- df %>%
        group_by(game,play,frame,x,y,club) %>% 
        summarise(suma = sum(influ)) %>% 
        ungroup() %>% 
        pivot_wider(names_from = club, values_from = suma) %>% 
        mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
               PC = ifelse(home+away > 1 , 1 , home+away)) %>% 
        select(game,play, frame, x,y, PC)
    } else{
      pc <- df %>%
        group_by(game,play,frame,x,y,club) %>% 
        summarise(suma = sum(influ)) %>% 
        ungroup() %>% 
        pivot_wider(names_from = club, values_from = suma) %>% 
        mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
               PC = plogis(home-away)) %>% 
        select(game,play, frame, x,y, PC) %>% 
        group_by(game,play, frame) %>% 
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


