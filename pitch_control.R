logistic <- function(x){
  1/(1 + exp(-x))
}

teams <- week1 %>% 
  select(nflId, club) %>% 
  distinct()

un_cuadro <- map_var2 %>% t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
  filter(game == '2022090800', play == 56, frame == 1) %>% 
  left_join(teams, by = c('player' = 'nflId'))
  

pc_1 <- un_cuadro %>% 
  group_by(x,y,club) %>% 
  summarise(suma = sum(influ)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(club), values_from = suma) %>% 
  mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
         PC = logistic(LA-BUF)) %>% 
  select(x,y, PC) %>% 
  complete(
    x = 0:100,
    y = 0:54,
    fill = list(PC= 0.5)
  )

players <- data_ball %>%  
  filter(gameId == '2022090800', playId == 56, frameId == 1)

ggplot(pc_1, aes(x, y)) +
  geom_raster(aes(fill = PC), interpolate = F, alpha = 0.8)+
  scale_fill_viridis_c()+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  geom_point(data = players, aes(x,y, color = club), size = 5)+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))


# Animada -----------------------------------------------------------------

library(gganimate)
una_jugada <- map_var2 %>% t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
  filter(game == '2022090800', play == 56) %>% 
  left_join(teams, by = c('player' = 'nflId'))

pc_2 <- una_jugada %>% 
  filter(frame <= 20) %>% 
  group_by(x,y,frameId=frame,club) %>% 
  summarise(suma = sum(influ)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(club), values_from = suma) %>% 
  mutate(across(.cols = everything(), function(x)ifelse(is.na(x), 0, x)),
         PC = logistic(LA-BUF)) %>% 
  select(x,y,frameId, PC) %>% 
  group_by(frameId) %>% 
  complete(
    x = 0:100,
    y = 0:54,
    fill = list(PC= 0.5)
  )

players <- data_ball %>%  
  filter(gameId == '2022090800', playId == 56 & frameId <= 20)

plot <- ggplot(pc_2, aes(x, y)) +
  geom_raster(aes(fill = PC), interpolate = F, alpha = 0.8)+
  scale_fill_viridis_c()+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  geom_point(data = players, aes(x,y, color = club), size = 3)+
  geom_segment(data = players, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))+
  transition_manual(frameId)

animate(plot,fps=25)
