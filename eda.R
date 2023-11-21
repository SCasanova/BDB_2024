library(tidyverse)
library(nflverse)
library(gganimate)
library(mvnfast)



radius <- function(distance){
  purrr::map_dbl(
    distance,
    function(x){
      if(x <= 17){
        exp(x*0.1144653)+3
      } else{
        10
      }
    }
  )
}

test <- tibble::tibble(
  x = seq(0,30, 0.1),
  y = radius(seq(0,30, 0.1))
)

ggplot(test, aes(x,y))+
  geom_line()+
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,2))

week1 <- read_csv('data/tracking_week_1.csv')

ball_loc <- week1 %>% 
  filter(club == 'football') %>% 
  group_by(gameId, playId, frameId) %>% 
  select(x_ball = x, y_ball = y) %>% 
  ungroup()

data_ball <- week1 %>% 
  mutate(ToLeft = playDirection == "left",
         x = ifelse(ToLeft, 120-x, x) - 10, 
         y = ifelse(ToLeft, 160/3-y, y),
         rad_dir = dir*pi/180,
         vector_x = -s*sin(rad_dir)+x,
         vector_y = -s*cos(rad_dir)+y) %>% 
  left_join(ball_loc, by = c('gameId', 'playId', 'frameId')) %>% 
  group_by(gameId, playId, frameId) %>% 
  mutate(distance_to_ball = sqrt((x-x_ball)^2 + (y-y_ball)^2)) %>% 
  ungroup() %>% 
  mutate(radio = radius(distance_to_ball),
         speed_ratio = s^2/13^2,
         scal_x = (radio+(radio*speed_ratio))/2,
         scal_y = (radio-(radio*speed_ratio))/2,
         mu_x = -s*sin((dir*pi/180))*0.5+x,
         mu_y = -s*cos((dir*pi/180))*0.5+y)



# influence_fun <- function(x,y,px,py,mu_x,mu_y,rad_dir,scal_x,scal_y){
#     mu <-  c(mu_x,mu_y)
#     R <- matrix(
#       c(cos(rad_dir), -sin(rad_dir), sin(rad_dir), cos(rad_dir)),
#       ncol = 2
#     )
#     R_inv <- matrix(
#       c(cos(rad_dir), sin(rad_dir), -sin(rad_dir), cos(rad_dir)),
#       ncol = 2
#     )
#     S = matrix(c(scal_y,0,0,scal_x), ncol = 2)
#     sigma = R%*%S%*%S%*%R_inv
#     sigma[sigma<= 1e-7] <-  0
#     sigma[2,1] = sigma[1,2]
#     dmvn(c(x,y), mu, sigma)/ dmvn(c(mu_x,mu_y), mu, sigma)
# }





personalfix::time_check({
df <- data_ball %>% 
  select(gameId, playId, frameId,nflId, radio, s, dir, vector_x, vector_y, px = x,py = y,mu_x,mu_y,dir,scal_x,scal_y) %>% 
  filter(!is.na(dir)) %>% 
  head(1) %>%
  data.frame()

coords <- expand.grid(
  x= seq(max(round(max(df$mu_x, na.rm = T))-20, 0), min(round(max(df$mu_x ,na.rm = T))+20, 100)),
  y =seq(max(round(max(df$mu_y, na.rm = T))-20, 0), min(round(max(df$mu_y, na.rm = T))+20, 54))
)


# coords <- expand.grid(
#   x= seq(0,100),
#   y =seq(0,53)
# )


df <- cross_join(coords, df)


  map_var <- mapply(influence_fun, df$x, df$y, df$px,df$py,df$mu_x,df$mu_y,df$dir,df$scal_x,df$scal_y)

})

personalfix::time_check({
  df <- data_ball %>% 
    select(gameId, playId, frameId,nflId, radio, s, dir, vector_x, vector_y, px = x,py = y,mu_x,mu_y,dir,scal_x,scal_y) %>% 
    filter(!is.na(dir)) %>% 
    head(1) %>%
    data.frame()
  
  coords <- expand.grid(
    x= seq(max(round(max(df$mu_x, na.rm = T))-20, 0), min(round(max(df$mu_x ,na.rm = T))+20, 100)),
    y =seq(max(round(max(df$mu_y, na.rm = T))-20, 0), min(round(max(df$mu_y, na.rm = T))+20, 54))
  )
  
  
  # coords <- expand.grid(
  #   x= seq(0,100),
  #   y =seq(0,53)
  # )
  
  
  df <- cross_join(coords, df)
  
  
  map_var <- parallel::mcmapply(influence_fun, df$x, df$y, df$px,df$py,df$mu_x,df$mu_y,df$dir,df$scal_x,df$scal_y,mc.cores=parallel::detectCores()-1)
  
})


bind_cols(df, influ =map_var) %>% 
  ggplot(aes(x,y,z=influ))+
  geom_contour_filled(breaks = seq(0.2,1,0.2), alpha = 0.6)+
  geom_contour(alpha = 0.2, breaks = seq(0.2,1,0.2))+
  scale_y_continuous(limits = c(0,40))+
  scale_x_continuous(limits = c(0,100))+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Blues'))+
  geom_point(aes(x =px, y = py))+
  geom_segment(aes(x = px, y = py, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))

  
bind_cols(df10, influ =map_var2) %>% 
  ggplot(aes(x,y,z=influ))+
  geom_contour_filled(breaks = seq(0.2,1,0.2))+
  scale_y_continuous(limits = c(0,40))+
  scale_x_continuous(limits = c(0,100))+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Blues'))+
 
  


bind_cols(df, influ =map_var) %>% 
 ggplot(aes(x,y,z=influ))+
  geom_contour_filled(breaks = seq(0.2,1,0.2))+
  scale_y_continuous(limits = c(0,40))+
  scale_x_continuous(limits = c(0,100))+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Blues'))
  

influence_fun(30,31)

influ <- tibble(
  expand.grid(
    seq(15,50,1),
    seq(15,50,1)
  )
) %>% 
  mutate(i = purrr::map2(Var1,Var2,influence_fun) %>% unlist())


ggplot(influ, aes(Var1, Var2, z = i))+
  geom_contour_filled(bins = 5)


p <- week1 %>% 
  filter(playId ==55, frameId <= 30) %>% 
  ggplot(aes(x = x, y = y, fill = club)) + 
  geom_point(pch = 21, size = 3) +
  scale_colour_brewer(palette = "Set2") + 
  scale_x_continuous(breaks = c(0:10)*5,) + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank())+
  geom_segment(aes(x = x, y = y, xend = vector_x, yend = vector_y),
               arrow = arrow(length = unit(2, "mm")))+
  transition_time(frameId)
  
animate(p,fps=10)


