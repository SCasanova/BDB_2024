library(tidyverse) 
library(mvnfast) # Multi-variada fast
library(parallel) # Para paralelizar


# Esta es la primera funcion que llamamos:
# Toma las coordenada, la media, los eigenvalores (scal) y los datos de la jugada
frame_influence <- function(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y, game, play, frame,player){
  # Calculamos un box de 20x20 para calcular la influencia 
  # (se puede optimizar con el angulo y velocidad)
  coords <- expand.grid(
    x= seq(max(round(mu_x)-10, 0), min(round(mu_x)+10, 100)),
    y =seq(max(round(mu_y)-10, 0), min(round(mu_y)+10, 54))
  )
  
  # La constante normalizadora (valor maximo de influencia)
  # La calculamos una sola vez por eficiencia
  normalizer <- pre_proccess(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y)
  # Lo agregamos como columna
  df <- cross_join(coords, tibble(px=px,py=py,mu_x=mu_x,mu_y=mu_y,rad_dir=rad_dir,scal_x=scal_x,scal_y=scal_y))

  # Regresamos una lista con toda la info y el calculo de influencia
  list(
      game = game,
      play = play,
      frame = frame,
      player = player,
      x = df$x,
      y = df$y,
      # Calculo de influencia vectorizado para todo el grid
      influ =( mapply(influence_fun, df$x, df$y, df$mu_x,df$mu_y, df$rad_dir, df$scal_x,df$scal_y, SIMPLIFY=FALSE) %>% 
        unlist())/normalizer
    )
  
  
}

# Función que se corre una sola vez por grid para la constante normalizadora
pre_proccess <- function(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y){
  # Seno y coseno de direccion en radianes
  sin_dir <- sin(rad_dir)
  cos_dir <- cos(rad_dir)
  # Vector de medias
  mu <-  c(mu_x,mu_y)
  # Matriz de rotacion
  R <- matrix(
    c(cos_dir, -sin_dir, sin_dir, cos_dir),
    ncol = 2
  )
  # Inversa calculada a mano
  R_inv <- matrix(
    c(cos_dir, sin_dir, -sin_dir, cos_dir),
    ncol = 2
  )
  # Matriz de eigenvalores 
  S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
  # Calculo de matriz de covarianzas
  sigma = R%*%S%*%S%*%R_inv
  # Normal bi-variada con estos datos (calculada para x = mu_x, y = mu_y)
  dmvn(c(mu_x,mu_y), mu, sigma)
}

influence_fun <- function(x,y, mu_x, mu_y, rad_dir, scal_x,scal_y){
  # Seno y coseno de direccion en radianes
  sin_dir <- sin(rad_dir)
  cos_dir <- cos(rad_dir)
  mu <-  c(mu_x,mu_y)
  # Matriz de rotacion
  R <- matrix(
    c(cos_dir, -sin_dir, sin_dir, cos_dir),
    ncol = 2
  )
  # Inversa calculada a mano
  R_inv <- matrix(
    c(cos_dir, sin_dir, -sin_dir, cos_dir),
    ncol = 2
  )
  # Matriz de eigenvalores (escalamiento)
  S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
  # Calculo de matriz de covarianzas
  sigma = R%*%S%*%S%*%R_inv
  # Normal bi-variada con estos datos
  dmvn(c(x,y), mu, sigma)
}




# Tests -------------------------------------------------------------------



df <- data_ball %>%
  filter(club != 'football') %>% 
  select(gameId, playId, frameId,nflId, radio, s, rad_dir, vector_x, vector_y, px = x,py = y,mu_x,mu_y,scal_x,scal_y) %>% 
  filter(!is.na(rad_dir)) %>% 
  head(1) %>% 
  data.frame()
  
  
personalfix::time_check({
  # Se vectoriza y paraleliza sobre todos nuestros frames
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




