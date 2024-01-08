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
  geom_raster(aes(fill = PC), interpolate = T, alpha = 0.8)+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('#C60C30', 'white', '#003594'))+
  scale_x_continuous(limits = c(10,60), breaks = seq(10,60, 10))+
  scale_y_continuous(limits = c(0,54))+
  # coord_fixed() +
  theme_minimal()+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               size = 1.5,
               arrow = arrow(length = unit(2, "mm"))) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    text = element_text(size = 15)
  )+
  scale_color_manual(values = c('#C60C30', '#003594'))+
  geom_point(data = players %>% rename(Team = club), aes(x,y, color = Team), size = 5)+
  geom_point(data = ball_carrier, aes(x,y), 
             size = 3, 
             color = c('#e8c83f'))+
  geom_point(data = filter(players, tackle==1), aes(x,y), 
             size = 3, 
             color = c('#257a30'))+
  labs(title = "Tackle made by J. Ramsey over S. Diggs", 
       subtitle = "Ball carrier shown in yellow and tackler in green",
       x = "Distance from ofensive team's own end zone",
       y = ''
       )

ggsave('figures/pitch_control.png', device = 'png', dpi = 'retina', width = 8, height = 5)


