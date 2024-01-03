library(tidyverse)
library(arrow)



get_quality <- function(){
  
  filenames <- list.files(glue::glue("./pitch_control/"), 
                          full.names = TRUE)

  for(f in filenames){
    pc <- read_parquet(f)
    
    week_num <- f %>% str_sub(21, 25)
    path <- glue::glue("value_map/value-{week_num}-{part}.parquet",
                       data_name = f %>% str_sub(20, -1),
                       part = seq(0,3))
  
    
    value <- bind_rows(
      read_parquet(path[1]),
      read_parquet(path[2]),
      read_parquet(path[3]),
      read_parquet(path[4])
    )
    
    tackler_info <- tackles_preprocess(read_parquet(glue::glue('clean_data/{week_num}.parquet'))) %>% 
      select(game = gameId, play = playId, frame = frameId, tackler = nflId, mu_x, mu_y, rad_dir, scal_x, scal_y, radio, min_dist)
    
    
    focus <- tackler_info %>% 
      mutate(f1_1 = mu_x-sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*sin(rad_dir),
             f1_2 = mu_y-sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*cos(rad_dir),
             f2_1 = mu_x+sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*sin(rad_dir),
             f2_2 = mu_y+sqrt((2*pmax(scal_x, scal_y))^2-  (2*pmin(scal_x, scal_y))^2)*cos(rad_dir)
      ) 
    
    
    ellipse_frames <- pc %>% 
      left_join(focus, by = c('game', 'play', 'frame'), relationship = 'many-to-many') %>% 
      filter(sqrt((x-f1_1)^2 + (y-f1_2)^2) + sqrt((x-f2_1)^2 + (y-f2_2)^2) <= 4*pmax(scal_x, scal_y)) %>% 
      select(game, play, frame, x,y, PC, tackler, min_dist)
    
    
    final <- inner_join(ellipse_frames, value, by = c('game', 'play', 'frame', 'x', 'y'), relationship = 'many-to-many') %>% 
      group_by(game, play,frame, tackler) %>% 
      mutate(quality = PC*value) %>% 
      summarise(sum_quality = sum(quality),
                mean_quality = mean(quality),
                median_quality = median(quality),
                max_quality = max(quality),
                min_dist = last(min_dist)) %>% 
      ungroup()
    
    write_parquet(final, glue::glue('quality/qty-{week_num}{rest}',
                  rest = str_sub(f, start = -10)))
    
  }
}
  
  
get_quality()


  
