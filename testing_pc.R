library(tidyverse)
library(arrow)


data <- read_parquet('pitch_control/pc-week1-0.parquet')
plays <- read_csv('data/plays.csv')
tackles <- read_csv('data/tackles.csv')


one_frame <- data %>% 
  filter(game == 2022090800, play == 56, frame == 6)

players <- read_parquet('clean_data/week1.parquet') %>%  
   filter(gameId == 2022090800, playId == 56, frameId == 6) %>% 
   rename(frame  = frameId) %>% left_join(tackles, c('gameId', 'playId', 'nflId'))

ball_carrier <- plays %>% 
  select(gameId, playId, ballCarrierId, playDescription) %>% 
  rename(nflId = ballCarrierId) %>% 
  inner_join(players, c('gameId', 'playId', 'nflId')) 

ggplot(one_frame, aes(x,y))+
  geom_raster(aes(fill = PC), interpolate = F, alpha = 0.6)+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('red', 'white', 'blue'))+
  scale_x_continuous(limits = c(5,60))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed() +
  scale_color_manual(values = c('firebrick', 'blue'))+
  geom_point(data = players, aes(x,y, color = club), size = 3)+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm"))) +
  geom_point(data = ball_carrier, aes(x,y), size = 3, color = c('#935e38'))+
  geom_point(data = filter(players, tackle==1), aes(x,y), size = 3, color = c('green'))+
  xlab("Distance from ofensive team's own end zone")+
  ylab("Y coordinate") +
  labs(title = "Tackle made by J. Ramsey over S. Diggs", subtitle = "Ball carrier shown with brown dot and tackler with green dot")

