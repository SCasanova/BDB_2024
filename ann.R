library(keras)
library(arrow)
library(nflverse)
library(tidyverse)

# Get games for training
def_pc <- read_parquet('def_pc/def-pc-week1-0.parquet') %>% 
  bind_rows(
    read_parquet('def_pc/def-pc-week1-1.parquet'),
    read_parquet('def_pc/def-pc-week1-2.parquet'),
    read_parquet('def_pc/def-pc-week1-3.parquet'),
    read_parquet('def_pc/def-pc-week2-0.parquet'),
    read_parquet('def_pc/def-pc-week2-1.parquet'),
    read_parquet('def_pc/def-pc-week2-2.parquet'),
    read_parquet('def_pc/def-pc-week2-3.parquet'),
    read_parquet('def_pc/def-pc-week3-0.parquet'),
    read_parquet('def_pc/def-pc-week3-1.parquet'),
    read_parquet('def_pc/def-pc-week3-2.parquet'),
    read_parquet('def_pc/def-pc-week3-3.parquet')
  ) %>% 
  # PC greater than 10%
  filter(PC>= 0.1)


# Week data for tracking info
weeks <- read_csv('data/tracking_week_1.csv') %>% 
  bind_rows(
    read_csv('data/tracking_week_2.csv'),
    read_csv('data/tracking_week_3.csv')
  ) %>% 
  filter(gameId %in% def_pc$game) %>% 
  select(game = gameId, play = playId, frame = frameId, nflId, x, y)

# Play data for ball carrier identification
plays <- read_csv('data/plays.csv') %>% 
  filter(gameId %in% def_pc$game) %>% 
  select(game = gameId, play = playId, ballCarrierId)

# Get tracking data for ball carrier
carr_loc <- weeks %>% 
  left_join(plays, by =c('game', 'play')) %>% 
  filter(ballCarrierId == nflId) %>% 
  select(game, play, frame, x_car = x, y_car = y)

# Add BC location to PC data
ann_set <- def_pc %>% 
  left_join(carr_loc, c('game', 'play', 'frame')) %>% 
  select(x,y,x_car,y_car,PC)

# Train/validation split
set.seed(12)
rows <- caret::createDataPartition(ann_set$PC, p = 0.8)$Resample1

ann_train <- ann_set[rows,]
ann_val <- ann_set[-rows,]

# Design model (single layer)
single_model <- keras_model_sequential() %>%
  layer_dense(units = 448,
              input_shape = c(4),
              kernel_initializer = initializer_random_normal(),
              activation = "relu") %>%
  layer_dense(name = "Output",
              activation = 'sigmoid',
              units=1)

# Compilation with optimizer
single_model %>% compile(
  loss = 'mae',
  optimizer =  optimizer_adam(learning_rate = 0.001), 
  metrics = list("mean_squared_error")
)



# Correct format for train and val X and Y
X <- ann_train %>% 
  select(-PC) %>% 
  # slice(5000000:10000000) %>%
  as.matrix()

Y <- ann_train %>% 
  select(PC)%>% 
  # slice(5000000:10000000) %>%
  as.matrix()

VX <- ann_val %>% 
  select(-PC) %>% 
  # slice(5000:10000) %>%
  as.matrix()

VY <- ann_val %>% 
  select(PC)%>% 
  # slice(5000:10000) %>%
  as.matrix()

# Model training
single_model %>% 
  fit(X,
      Y, 
      epochs = 50,
      batch = 2^9,
      validation_data = list(VX, VY),
      callbacks = callback_early_stopping(
        monitor = "val_loss",
        patience = 10,
        restore_best_weights = TRUE
      )
    )


# Testing
def_pc_test <- read_parquet('def_pc/def-pc-week4-0.parquet') %>% 
  filter(game == 2022092900, play == 57, frame == 17)

weeks_test <-week4_proc %>% 
  filter(gameId %in% def_pc_test$game) %>% 
  select(game = gameId, play = playId, frame = frameId, nflId, x, y, club)

plays_test <- read_csv('data/plays.csv') %>% 
  filter(gameId %in% def_pc_test$game) %>% 
  select(game = gameId, play = playId, ballCarrierId)

carr_loc_test <- weeks_test %>% 
  left_join(plays_test, by =c('game', 'play')) %>% 
  filter(ballCarrierId == nflId) %>% 
  select(game, play, frame, x_car = x, y_car = y)


library(gganimate)
carr_loc_test %>% 
  filter(game == 2022092900, play == 57) %>% 
  ggplot(aes(x_car,y_car))+
  geom_point()+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()
  


test_set <- def_pc_test %>% 
  left_join(carr_loc_test, c('game', 'play', 'frame')) %>% 
  select(x,y,x_car,y_car,PC)

test_x <- test_set %>% 
  select(-PC) %>% 
  as.matrix()

pred_pc <- single_model %>% 
  predict(test_x)

test_full <- bind_cols(test_set, value = as.vector(pred_pc)) %>% 
  # complete(
  #   x = 0:100,
  #   y = 0:54,
  #   fill = list(value= 0, PC = 0.5),
  # ) %>% 
  select(x,y,value, PC) 


ggplot(test_full, aes(x, y, color = value)) +
  geom_point()+
  annotate('text', x = 28.2, y = 17.6, label ='BC', color = 'red')+
  scale_x_continuous(limits = c(0,100))+
  scale_y_continuous(limits = c(0,54))+
  coord_fixed()+
  geom_point(data =weeks_test %>% filter(game == 2022091803, play == 57, frame == 10 ), aes(x,y, color = NULL))
  

