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
    family = 'binomial')

saveRDS(model, 'clean_data/model.RDS')

prob <- function(suma){
  exp(-1.1180  + 0.2277  *suma)/ 
    (1 + exp(-1.1180  + 0.2277  *suma))
} 


results <- tibble(
  quality = fitting$sum_quality,
  result = fitting$tackling,
  value = model$fitted.values
)


line <- tibble(
  x = seq(0,21),
  y = prob(seq(0,21))
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
  xlim(0,21)


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
  xlim(0,21)

hist1/logistic_line/hist2

ggsave('figures/succesful_failed.png',
       device = 'png',
       dpi = 'retina',
       width = 4,
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



