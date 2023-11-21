library(tidyverse)
library(mvnfast)
library(parallel)


frame_influence <- function(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y, game, play, frame,player){
  coords <- expand.grid(
    x= seq(max(round(mu_x)-10, 0), min(round(mu_x)+10, 100)),
    y =seq(max(round(mu_y)-10, 0), min(round(mu_y)+10, 54))
  )
  
  
  normalizer <- pre_proccess(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y)
  df <- cross_join(coords, tibble(px=px,py=py,mu_x=mu_x,mu_y=mu_y,rad_dir=rad_dir,scal_x=scal_x,scal_y=scal_y))

    list(
      game = game,
      play = play,
      frame = frame,
      player = player,
      x = df$x,
      y = df$y,
      influ =( mapply(influence_fun, df$x, df$y, df$mu_x,df$mu_y, df$rad_dir, df$scal_x,df$scal_y, SIMPLIFY=FALSE) %>% 
        unlist())/normalizer
    )
  
  
}

pre_proccess <- function(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y){
  sin_dir <- sin(rad_dir)
  cos_dir <- cos(rad_dir)
  mu <-  c(mu_x,mu_y)
  R <- matrix(
    c(cos_dir, -sin_dir, sin_dir, cos_dir),
    ncol = 2
  )
  R_inv <- matrix(
    c(cos_dir, sin_dir, -sin_dir, cos_dir),
    ncol = 2
  )
  S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
  sigma = R%*%S%*%S%*%R_inv
  # sigma[2,1] = sigma[1,2]
  dmvn(c(mu_x,mu_y), mu, sigma)
}

influence_fun <- function(x,y, mu_x, mu_y, rad_dir, scal_x,scal_y){
  sin_dir <- sin(rad_dir)
  cos_dir <- cos(rad_dir)
  mu <-  c(mu_x,mu_y)
  R <- matrix(
    c(cos_dir, -sin_dir, sin_dir, cos_dir),
    ncol = 2
  )
  R_inv <- matrix(
    c(cos_dir, sin_dir, -sin_dir, cos_dir),
    ncol = 2
  )
  S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
  sigma = R%*%S%*%S%*%R_inv
  # sigma[2,1] = sigma[1,2]
  dmvn(c(x,y), mu, sigma)
}




  df <- data_ball %>% 
    select(gameId, playId, frameId,nflId, radio, s, rad_dir, vector_x, vector_y, px = x,py = y,mu_x,mu_y,scal_x,scal_y) %>% 
    filter(!is.na(rad_dir)) %>% 
    head(1) %>% 
    data.frame()
  
  
personalfix::time_check({
  map_var2 <-mcmapply(frame_influence,df$px,df$py,df$mu_x,df$mu_y,df$rad_dir,df$scal_x,df$scal_y,df$gameId, df$playId, df$frameId, df$nflId, mc.cores=parallel::detectCores()-1)

})

test <- map_var2 %>% t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
  filter(game == '2022090800', play == 56, player ==52536, frame == 2)


map_var2 %>% t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
  group_by(game, play, frame, player) %>% 
  summarise(inf = min(influ)) %>% 
  pull(inf) %>% max()

ggplot(test, aes(x,y,z=influ))+
  geom_contour_filled(breaks = seq(0.2,1,0.2))+
  ggplot2::xlim(c(0,100))+
  ggplot2::ylim(c(0,54))+
  coord_fixed()
  scale_y_continuous(limits = c(0,40))+
  scale_x_continuous(limits = c(0,100))+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Blues'))




