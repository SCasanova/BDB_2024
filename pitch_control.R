library(tidyverse)
library(arrow)


week1 <- read_csv('data/tracking_week_1.csv')
games <- read_csv('data/games.csv') %>% 
  select(week, homeTeamAbbr, visitorTeamAbbr) %>% 
  filter(week==1)

players <-week1 %>% 
  select(nflId, club) %>% 
  distinct()

teams <- inner_join(players, games, by =c('club'= 'homeTeamAbbr' )) %>% 
  mutate(club = 'home') %>% 
  bind_rows(
    inner_join(players, games, by =c('club'= 'visitorTeamAbbr' )) %>% 
      mutate(club = 'away')
  ) %>% 
  select(nflId, club)



una_semana_part <-read_parquet('partitions/week1/week1-0.parquet') %>% 
  # head(10000000) %>%
  left_join(teams, by = c('player' = 'nflId'))

personalfix::time_check({
  pc1 <- una_semana_part %>%
    # head(100000000) %>% 
    group_by(game,play,frame,x,y,club) %>% 
    summarise(suma = sum(influ)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = club, values_from = suma) %>% 
    mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
           PC = plogis(home-away)) %>% 
    select(game,play, x,y, PC) %>% 
    complete(
      x = 0:100,
      y = 0:54,
      fill = list(PC= 0.5)
    )
})


players <- data_ball %>%  
  filter(gameId == '2022090800', playId == 1946, frameId == 40, 
         nflId %in% c(43335, 38577))

ggplot(pc_1, aes(x, y)) +
  geom_raster(aes(fill = PC), interpolate = F, alpha = 0.8)+
  scale_fill_viridis_c()+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  geom_point(data = players, aes(x,y, color = club), size = 5)+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('red', 'white', 'blue'))


# Animada -----------------------------------------------------------------
library(tidyverse) 
library(mvnfast) # Multi-variada fast
library(parallel)
library(gganimate)



pc_2 <- read_parquet('pitch_control/pc-week1-2.parquet') %>% 
  filter(game == '2022091108' & play == '2208')

# una_jugada <- map_var2 %>% t() %>% 
#   data.frame() %>% 
#   unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
#   # filter(game == '2022090800', play == 56) %>% 
#   left_join(teams, by = c('player' = 'nflId'))

# pc_2 <- una_jugada %>% 
#   filter(frame <= 30) %>%
#   group_by(x,y,frameId=frame,club) %>% 
#   summarise(suma = sum(influ)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = c(club), values_from = suma) %>% 
#   mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
#          PC = plogis(LA-BUF)) %>% 
#   select(x,y,frameId, PC) %>% 
#   group_by(frameId) %>%
#   complete(
#     x = 0:100,
#     y = 0:54,
#     fill = list(PC= 0.5)
#   ) %>% 
#   ungroup()

players <- data_ball %>%  
  filter(gameId == '2022091108', playId == 2208, frameId ==6) %>% 
  rename(frame  = frameId) 

plot <- ggplot(pc_2 %>% filter(frame == 6), aes(x, y)) +
  geom_raster(aes(fill = PC), interpolate = F, alpha = 0.6)+
  scale_fill_gradientn(limits  = range(0, 1),
                       colours = c('red', 'white', 'blue'))+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  scale_color_manual(values = c('green', 'firebrick', 'navy'))+
  geom_point(data = players, aes(x,y, color = club), size = 3)+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))

animate(plot,fps=20)

