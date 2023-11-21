library(tidyverse)

data <- read_csv('~/Downloads/FIFA_WC_Russia_DB.csv') %>% 
  janitor::clean_names() %>% 
  mutate(pct_bets = bets/sum(.$bets))



model <- lm(games_won ~ mkt_value+
     player_age+
     mkt_value_per_player+ 
     player_height+
     coach_wage+
     pct_bets+
     goals_for+
     goals_against+
     invested_money+
     squad_formation
     ,
   data = data)

summary(model)

francia <- data[data$country == 'France',]

predict(model, francia)
