library(tidyverse)
library(arrow)
library(nflverse)


filenames <- list.files(glue::glue("./quality/"), 
                        full.names = TRUE)

quality <- tibble()
for(f in filenames){
  quality <- bind_rows(quality, read_parquet(f))
}

quality <- quality %>% 
  group_by(game, play, tackler) %>% 
  mutate(frame_before = lead(min_dist)) %>% 
  ungroup()

scrambles <- load_pbp(2022) %>% 
  select(old_game_id, play_id, pass, rush, sack, qb_scramble, play_type, fumble) %>% 
  filter(qb_scramble == 1 | fumble == 1) %>% 
  mutate(game_id = as.numeric(old_game_id))

tackles <- read_csv('data/tackles.csv') %>% 
  filter(tackle != pff_missedTackle | assist != pff_missedTackle) %>% 
  mutate(tackling = ifelse(tackle == 1 | assist == 1, 1, 0)) %>% 
  select(game = gameId,play = playId,nflId, tackling)


fitting <- 
  inner_join(quality %>% filter(min_dist == 1 & distance <= 2), 
             tackles, 
             by = c('game', 'play', 'tackler' = 'nflId')) %>% 
  anti_join(scrambles, by = c('game' = 'game_id', 'play' = 'play_id')) %>% 
  distinct()

rows <- caret::createDataPartition(fitting$tackling, p= 0.75)$Resample1


train <- fitting[rows,]
test <- fitting[-rows,]

ggplot(train, aes(as.factor(tackling), sum_quality, 
                  group = as.factor(tackling), 
                  fill =as.factor(tackling) ))+
  geom_boxplot(alpha = 0.8)+
  theme_minimal()+
  scale_fill_manual(values  = c('#bd243d', '#4c8a4f'))+
  theme(legend.position = 'none')+
  labs(
    x = 'Tackle',
    y = 'Total Quality of Control'
  )

ggsave('figures/boxplot_regression.png',
       device = 'png',
       dpi = 'retina',
       width = 10,
       height = 8
       )

  # filter(tackling == 0 | (tackling == 1 & sum_quality >= 5.9))

# fitting2 <- 
#   inner_join(quality %>% filter(frame_before == 1 | min_dist ==1), 
#              tackles, 
#              by = c('game', 'play', 'tackler' = 'nflId')) %>% 
#   anti_join(scrambles, by = c('game' = 'game_id', 'play' = 'play_id')) %>% 
#   distinct() %>% 
#   group_by(game, play, tackler) %>% 
#   summarise(sum_quality =sum(sum_quality),
#             tackling = last(tackling))

ggplot(fitting %>% filter(tackling == 1), aes(sample = sum_quality))+
  geom_qq(distribution = stats::qunif)

# ggplot(fitting2 %>% filter(tackling == 1), aes(y =sum_quality))+
#   geom_boxplot()

ggplot(fitting %>% filter(tackling == 0), aes(sample = sum_quality))+
  geom_qq(distribution = stats::qunif)

ggplot(fitting %>% filter(tackling == 1) , aes(sum_quality))+
  geom_histogram()

model <- glm(tackling ~ sum_quality, 
    data = fitting,
    family = binomial(link = 'probit'),
    )

# saveRDS(model, 'clean_data/model.RDS')

readRDS('clean_data/model.RDS')

prob <- function(suma){
  exp(-1.1180  + 0.2277  *suma)/ 
    (1 + exp(-1.1180  + 0.2277  *suma))
} 

prob_cloglog <- function(suma){
  exp(-0.64504  + 0.09964  *suma)/ 
    (1 + exp(-0.64504  + 0.09964  *suma))
} 

prob_new<- function(suma){
  exp(-1.2134  + 0.2324  *suma)/ 
    (1 + exp(-1.2134  + 0.2324  *suma))
} 


results <- tibble(
  quality = fitting$sum_quality,
  result = fitting$tackling,
  value = model$fitted.values
)


# tests -------------------------------------------------------------------



model_logit <- glm(tackling ~ sum_quality, 
             data = train,
             family = binomial(link = 'logit'),
)

saveRDS(model_logit, 'clean_data/model_logit.rds')

model_proit <- glm(tackling ~ sum_quality, 
                   data = train,
                   family = binomial(link = 'probit'),
)

model_log <- glm(tackling ~ sum_quality, 
                   data = train,
                   family = binomial(link = 'cloglog'),
)

test_logit <- test %>% 
  mutate(prediction = predict(model_logit, test) %>% plogis(),
         logloss = LogLoss(prediction, tackling))


mean(test_logit$logloss)

caret::confusionMatrix( as.factor(test_logit$prediction), as.factor(test_logit$tackling) )

test_probit <- test %>% 
  mutate(prediction = predict(model_proit, test) %>% plogis(),
         logloss = LogLoss(prediction, tackling))

mean(test_probit$logloss)

caret::confusionMatrix( as.factor(test_probit$prediction), as.factor(test_probit$tackling))

test_cloglog <- test %>% 
  mutate(prediction = predict(model_log, test) %>% plogis(),
         logloss = LogLoss(prediction, tackling))

mean(test_cloglog$logloss)

caret::confusionMatrix(as.factor(test_cloglog$prediction), as.factor(test_cloglog$tackling))





# end ---------------------------------------------------------------------



line <- tibble(
  x = seq(0,21),
  y = prob_new(seq(0,21))
)

library(patchwork)

logistic_line <- ggplot(data = line, aes(x, y))+
  geom_line(data = line, aes(x,y))+
  # geom_point(aes(y = result))+
  theme_minimal()+
  labs(
    x = 'Pitch Control Quality',
    y = 'Probabilty of Succesful Tackle'
  )+
  ylim(c(0,1))+
  xlim(0,21)

hist1 <- ggplot(results %>% filter(result == 1), aes(quality))+ 
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.7,
    alpha = 0.7,
    fill = '#4c8a4f',
    position = 'identity'
  )+
  labs(
    y= 'Succesful Tackles'
  )+
  theme_void()+
  xlim(0,21)+
  geom_segment(aes(x = mean(results %>% 
                              filter(result == 1) %>% 
                              pull(quality)),
                   xend = mean(results %>% 
                              filter(result == 1) %>% 
                              pull(quality)),
                   y = -0.01, yend = 0.15),
               linetype = 'dashed')


hist2 <- ggplot(results %>% filter(result == 0), aes(quality))+ 
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.67,
    alpha = 0.7,
    fill = '#bd243d',
    position = 'identity'
  )+
  scale_y_reverse()+
  labs(y = 'Missed Tackles (PFF)')+
  theme_void()+
  xlim(0,21)+
  geom_segment(aes(x = mean(results %>% 
                              filter(result == 0) %>% 
                              pull(quality)),
                   xend = mean(results %>% 
                                 filter(result == 0) %>% 
                                 pull(quality)),
                   y = -0.01, yend = 0.15),
               linetype = 'dashed')

hist1/logistic_line/hist2

ggsave('figures/succesful_failed.png',
       device = 'png',
       dpi = 'retina',
       width = 5,
       height = 6
       )


 ggplot(results2, aes(sum_quality, max_quality, color = as.factor(result)))+
  geom_point()

ggplot(fitting2, aes(sum_quality, fill = as.factor(tackling)))+
  geom_histogram(aes(y=after_stat(density)), 
                 binwidth = 0.6, 
                 alpha = 0.4, 
                 position = 'identity')

ggplot(results2, aes(sum_quality, fill = as.factor(result)))+
  geom_histogram(aes(y=after_stat(density)), 
                 binwidth = 0.4, 
                 alpha = 0.4, 
                 position = 'identity')



