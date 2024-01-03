library(tidyverse) 
library(mvnfast) # Multi-variada fast
library(parallel) # Para paralelizar
library(microbenchmark)


value <- frame_influence(25,25,25,25,0,4,4,1,1,1,'a') %>%  t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
  select(x,y,value =influ)

pc <- frame_influence(25,25,25,25,0,4,4,1,1,1,'b') %>%  t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ)) %>% 
  mutate(influ = plogis(influ)) %>% 
  select(x,y,pc =influ)


inner_join(pc, value, by = c('x','y')) %>% 
  mutate(quality = pc*value) %>% 
  summarise(sum(quality))

# First function call (once per frame call)
# Takes the player coordinates, the mean,  eigenvalues(scaling factors), 
# direction and play metadata
frame_influence <- function(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y, game, play, frame,player){
  # We calculate a box based on the direction of movement
  coords <- expand.grid(
    x= seq(max(round(mu_x)-round(scal_x*2), 0), min(round(mu_x)+round(scal_x*2), 120)),
    y =seq(max(round(mu_y)-ceiling(scal_y*2.8), 0), min(round(mu_y)+ceiling(scal_y*2.8), 54))
  )
  
  # We calculate the normalizing constant (max influence value) once for effiency
  normalizer <- pre_proccess(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y)
  # Now we create a df with our coordinate grid and previous parameters
  # Possible to compute sine and cosine before (maybe matrices)
  df <- cross_join(coords, tibble(px=px,py=py,mu_x=mu_x,mu_y=mu_y,rad_dir=rad_dir,scal_x=scal_x,scal_y=scal_y))

  # List output with play metadata and the vectorized output for all coordinates influene
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

# Functino that creates the normalizing constant (run once per player per frame)
pre_proccess <- function(px,py,mu_x,mu_y,rad_dir,scal_x,scal_y){
  # Compute sine and cosine
  sin_dir <- sin(rad_dir)
  cos_dir <- cos(rad_dir)
  # Mean vector
  mu <-  c(mu_x,mu_y)
  # Rotation matrix
  R <- matrix(
    c(cos_dir, -sin_dir, sin_dir, cos_dir),
    ncol = 2
  )
  # Hand-calulated 2x2 inverse for efficiency
  R_inv <- matrix(
    c(cos_dir, sin_dir, -sin_dir, cos_dir),
    ncol = 2
  )
  # Eigenvalue (scaling) matrix
  S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
  # Covariance matrix computing
  sigma = R%*%S%*%S%*%R_inv
  # We calculate the bi-variate normal value for the location of mu
  dmvn(mu, mu, sigma)
} 

influence_fun <- function(x,y, mu_x, mu_y, rad_dir, scal_x,scal_y){
  # Compute sine and cosine
  sin_dir <- sin(rad_dir)
  cos_dir <- cos(rad_dir)
  # Mean vector
  mu <-  c(mu_x,mu_y)
  # Rotation matrix
  R <- matrix(
    c(cos_dir, -sin_dir, sin_dir, cos_dir),
    ncol = 2
  )
  # Hand-calulated 2x2 inverse for efficiency
  R_inv <- matrix(
    c(cos_dir, sin_dir, -sin_dir, cos_dir),
    ncol = 2
  )
  # Eigenvalue (scaling) matrix
  S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
  # Covariance matrix computing
  sigma = R%*%S%*%S%*%R_inv
  # Get bi-variate normal for the coordinates
  dmvn(c(x,y), mu, sigma)
}




# Tests -------------------------------------------------------------------



# df <- data_ball %>%
#   filter(club != 'football') %>% 
#   select(gameId, playId, frameId,nflId, radio, s, rad_dir, vector_x, vector_y, px = x,py = y,mu_x,mu_y,scal_x,scal_y) %>% 
#   filter(!is.na(rad_dir)) %>%
#   # filter(gameId == 2022090800 & playId == 146) %>%
#   data.frame()
  

library(arrow)

df <- read_parquet('clean_data/week9.parquet') %>% 
  filter(club != 'football') %>%
  select(gameId, playId, frameId,nflId, radio, s, rad_dir, vector_x, vector_y, px = x,py = y,mu_x,mu_y,scal_x,scal_y) %>% 
  filter(!is.na(rad_dir))


# Parallelized and vectorized run
map_var2 <-mcmapply(frame_influence,df$px,df$py,df$mu_x,df$mu_y,df$rad_dir,df$scal_x,df$scal_y,df$gameId, df$playId, df$frameId, df$nflId, mc.cores=parallel::detectCores()-1)

final <- map_var2 %>% t() %>% 
  data.frame() %>% 
  unnest(cols = c(game, play, frame, player, x, y, influ))


write_parquet(final, 'parquet_pc/week9_pc.parquet')



