library(arrow)
library(tidyverse)


# Get the ball carrier for each play
plays <- read_csv('data/plays.csv') %>% 
  select(game = gameId, play = playId, ballCarrierId)


value_normalizer <- function(x){
  exp(-abs(x-100)/200)
}


# Get the influence of th ball carrier for each play
value_get <- function(week_to_get){
  filenames <- list.files(glue::glue("./partitions/{week_to_get}/"), 
                          full.names = TRUE)
  
  for(f in filenames){
    read_parquet(f) %>% 
      inner_join(plays, by = c('game', 'play', 'player' = 'ballCarrierId')) %>%
      select(game,play,frame,x,y,value = influ) %>% 
      write_parquet(., glue::glue('value_map/value-{week_to_get}{rest}', 
                                   rest = str_sub(f, start = -10)))
  }
}

value_get('week1')
value_get('week2')
value_get('week3')
value_get('week4')
value_get('week5')
value_get('week6')
value_get('week7')
value_get('week8')
value_get('week9')

read_parquet('value_map/value-week1-0.parquet')
