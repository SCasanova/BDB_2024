library(tidyverse)
library(arrow)



get_quality <- function(all = FALSE){
  
  # get all filenames for pitch control
  filenames <- list.files(glue::glue("./pitch_control/"), 
                          full.names = TRUE)

  for(f in filenames){
    # Read first file
    pc <- read_parquet(f) 
    
    # Extract week name 
    week_num <- f %>% str_sub(21, 25)
    
    # Get matching path for value map same week
    path <- glue::glue("value_map/value-{week_num}-{part}.parquet",
                       data_name = f %>% str_sub(20, -1),
                       part = seq(0,3))
  
    # All value for week
    pc_value <- bind_rows(
      read_parquet(path[1]),
      read_parquet(path[2]),
      read_parquet(path[3]),
      read_parquet(path[4])
    ) %>% 
      filter(game %in% pc$game) %>% 
      left_join(pc, by = c('game', 'play', 'frame', 'x', 'y'))
    
    # Use the tackle pre process to get min distance and only tackling players
    tackler_info <- tackles_preprocess(read_parquet(glue::glue('clean_data/{week_num}.parquet')), only_tacklers = !all) %>% 
      filter(gameId %in% pc$game) %>% 
      group_by(gameId, playId, nflId) %>% 
      filter(min(distance_to_ball) <= 5) %>% 
      select(game = gameId, play = playId, frame = frameId, tackler = nflId, mu_x, mu_y, rad_dir, scal_x, scal_y, radio, distance_to_ball, min_dist)
    
    # get the elipse focus (using eigenvalue decomp of bivariate normal)
    focus <- tackler_info %>% 
      mutate(f1_1 = mu_x-sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*sin(rad_dir),
             f1_2 = mu_y-sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*cos(rad_dir),
             f2_1 = mu_x+sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*sin(rad_dir),
             f2_2 = mu_y+sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*cos(rad_dir)
      ) %>% 
      select(game, play,frame, tackler, min_dist, distance_to_ball, f1_1,f1_2, f2_1, f2_2, scal_x,scal_y)
    
    # Extract the elipse PC for the frame and player
    final <- pc_value %>% 
      inner_join(focus, by = c('game', 'play', 'frame'), relationship = 'many-to-many') %>% 
      filter(sqrt((x-f1_1)^2 + (y-f1_2)^2) + sqrt((x-f2_1)^2 + (y-f2_2)^2) <= 5*pmax(scal_x, scal_y)) %>% 
      select(game, play, frame, x,y, PC, value, tackler, min_dist, distance_to_ball) %>% 
      group_by(game, play,frame, tackler) %>% 
      mutate(quality = PC*value) %>% 
      # Relevant statistics after the cross (and min dist for tackling moment)
      summarise(sum_quality = sum(quality),
                mean_quality = mean(quality),
                median_quality = median(quality),
                max_quality = max(quality),
                min_dist = last(min_dist),
                distance = last(distance_to_ball)) %>% 
      ungroup()
    

    
    folder <- ifelse(all, 'all_quality', 'quality')
    prefix <- ifelse(all, 'all-qty', 'qty')
    
    write_parquet(final, glue::glue('{folder}/{prefix}-{week_num}{rest}',
                  rest = str_sub(f, start = -10)))
    
  }
}
  
  
get_quality(all=F)
get_quality(all=T)

  
