library(tidyverse)
library(arrow)


data <- read_parquet('pitch_control/pc-week1-0.parquet')


one_frame <- data %>% 
  filter(game == 2022090800, play == 56, frame == 6)

players <- read_parquet('clean_data/week1.parquet') %>%  
  filter(gameId == 2022090800, playId == 56, frameId == 6) %>% 
  rename(frame  = frameId) 

ggplot(one_frame, aes(x,y))+
  geom_raster(aes(fill = PC), interpolate = F, alpha = 0.6)+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('red', 'white', 'blue'))+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  scale_color_manual(values = c('firebrick', 'blue'))+
  geom_point(data = players, aes(x,y, color = club), size = 3)+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))

