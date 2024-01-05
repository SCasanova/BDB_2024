library(tidyverse)
library(arrow)
library(patchwork)

info <- read_parquet('clean_data/week9.parquet') %>% 
  filter(distance_to_ball == 0 & s >= 9 & rad_dir >= 2.3 & rad_dir <= 3.5) %>%
  filter(gameId == '2022110608' & playId == 1403 & frameId == 21)

data <- read_parquet('value_map/value-week9-3.parquet') %>% 
  filter(game == '2022110608' & play == 1403 & frame == 21)


moving <- ggplot(data, aes(x,y))+
  geom_contour_filled(aes(z=value), breaks = seq(0.2,1,0.2), binwidth = 2)+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(9, 'Blues'))+
  geom_point(data = info, aes(x =x, y = y), size = 4)+
  geom_segment(data = info, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               size= 1.5,
               arrow = arrow(length = unit(4, "mm")))+
  theme_void()+
  labs(
    fill = 'Influence Level',
    subtitle = '9.5 yards/sec',
    caption = '19.4 mph or 31.3 kph'
  )+
  theme(
    plot.subtitle = element_text(hjust = 0.5)
  )

info <- read_parquet('clean_data/week9.parquet') %>% 
  filter(distance_to_ball == 0 & s ==0 ) %>% 
  filter(gameId == '2022110602' & playId == 253 & frameId == 55)

data <- read_parquet('value_map/value-week9-1.parquet') %>% 
  filter(game == '2022110602' & play == 253 & frame == 55)


static <- ggplot(data, aes(x,y))+
  geom_contour_filled(aes(z=value), breaks = seq(0.2,1,0.2), binwidth = 2, show.legend = F)+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(6, 'Blues'))+
  geom_point(data = info, aes(x =x, y = y), size = 4)+
  geom_segment(data = info, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               size= 1.5)+
  theme_void()+
  labs(
    fill = 'Influence Level',
    subtitle = '0 yards/sec'
  )+
  theme(
    plot.subtitle = element_text(hjust = 0.5)
  )

static+moving

ggsave('figures/influence.png', 
       device = 'png', 
       dpi = 'retina',
       width = 5,
       height = 2.5)


ggplot(final, aes(x,y))+
  geom_contour_filled(aes(z=influ), breaks = seq(0.05,1,0.05), binwidth = 2, show.legend = F)+
  coord_fixed()


# Tables ------------------------------------------------------------------

library(gt)
library(gtExtras)
library(nflverse)


player_info <- load_rosters(2022) %>% 
  select(gsis_it_id, position, team, headshot_url)

table_data <- grade_pff %>% 
  mutate(tackler = as.character(tackler)) %>% 
  left_join(player_info, by = c('tackler' = 'gsis_it_id')) %>% 
  select(displayName, headshot_url, position_tackle:tackles)

table_data %>%
  arrange( technique) %>%
  filter(tackles >= 11) %>% 
  head(15) %>% 
  gt() %>%
  tab_header(
    title = 'Bottom Players: Control Based Tackling Technique',
  ) %>%
  cols_label(
    headshot_url = '',
  ) %>%
  # tab_spanner(
  #   label = "Desempeño",
  #   columns = c(accuracy, time)
  # ) %>%
  cols_align(
    align = "center"
  ) %>%
  cols_align(
    align = "left",
    displayName
  ) %>%
  gtExtras::gt_img_rows(
    headshot_url
  ) %>%
  data_color(
    columns = `technique`,
    method = "numeric",
    palette = c("#A50026", "#006837"),
    domain = c(0.18,-0.3)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique)
  ) %>% 
  cols_label(
    displayName = 'Player',
    # position = 'Pos',
    position_tackle = 'Avg. Control at Tackle',
    tackle_pct = 'Tackle %',
    technique = 'Technique +/-',
    tackles = 'Tackles #'
  ) %>% 
  tab_options(data_row.padding = px(10)) %>% 
  gtsave("figures/bottom.png", expand = 3)
