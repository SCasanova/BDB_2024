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


# Value Map ---------------------------------------------------------------

info <- read_parquet('clean_data/week9.parquet') %>% 
  # filter(distance_to_ball == 0 & s >= 9 & rad_dir >= 2.3 & rad_dir <= 3.5) %>%
  filter(gameId == '2022110608' & playId == 1403 & frameId == 21 & nflId == 47987)

data <- read_parquet('value_map/value-week9-3.parquet') %>% 
  filter(game == '2022110608' & play == 1403 & frame == 21)

ggplot(data, aes(x,y))+
  geom_contour_filled(aes(z=value), breaks = seq(0.2,1,0.2), binwidth = 2)+
  coord_fixed()+
  scale_fill_manual(values = RColorBrewer::brewer.pal(4, 'Greens'))+
  geom_point(data = info, aes(x =x, y = y), size = 7)+
  geom_segment(data = info, aes(x = x, y = y, xend = vector_x, yend = vector_y),
               size= 2.5,
               arrow = arrow(length = unit(7, "mm")))+
  theme_void()+
  theme(
    legend.position = 'none',
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave('figures/test.png', 
       device = 'png', 
       bg = 'transparent',
       dpi = 'retina',
       width = 5,
       height = 2.5)


 # Tables ------------------------------------------------------------------

library(gt)
library(gtExtras)
library(nflverse)
library(tidyverse)


player_info <- load_rosters(2022) %>% 
  select(gsis_it_id, position, team, headshot_url)

table_data <- grade_pff %>% 
  mutate(tackler = as.character(tackler)) %>% 
  left_join(player_info, by = c('tackler' = 'gsis_it_id')) %>% 
  select(displayName, position = position.y, headshot_url, position_tackle:tackles)


tab1 <- table_data %>% 
  arrange(-position_tackle) %>% 
  filter(tackles >= 11 & position == 'LB') %>% 
  head(5) %>% 
  select(-position) %>% 
  gt() %>%
  tab_header(
    title = 'Top LBs in Tackle Positioning',
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
    columns = `position_tackle`,
    method = "numeric",
    palette = c("#A50026", "#006837"),
    domain = c(0,1)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Technique +/-',
    tackles = 'Tackles #'
  ) %>% 
  tab_options(data_row.padding = px(7)) %>% 
  gtsave("figures/lbs.png", expand = 2)


table_data %>%
  arrange( -technique) %>%
  filter(tackles >= 11 & position == 'LB') %>% 
  head(10) %>% 
  gt() %>%
  tab_header(
    title = 'Top LBs: Control Based Tackling Performance',
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
    domain = c(0.14,-0.3)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Performance',
    tackles = 'Tackle Events'
  ) %>% 
  tab_options(data_row.padding = px(7),
              table.font.size = px(20)) %>% 
  gtsave("figures/top_lbs.png", expand =3)

table_data %>%
  arrange( -technique) %>%
  filter(tackles >= 11 & position == 'DL') %>% 
  head(10) %>% 
  gt() %>%
  tab_header(
    title = 'Top DLs: Control Based Tackling Performance',
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
    domain = c(0.19,-0.3)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Performance',
    tackles = 'Tackle Events'
  ) %>% 
  tab_options(data_row.padding = px(7),
              table.font.size = px(20)) %>% 
  gtsave("figures/top_dls.png", expand = 3)

table_data %>%
  arrange( -technique) %>%
  filter(tackles >= 11 & position == 'DB') %>% 
  head(10) %>% 
  gt() %>%
  tab_header(
    title = 'Top DBs: Control Based Tackling Performance',
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
    domain = c(0.14,-0.3)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Performance',
    tackles = 'Tackle Events'
  ) %>% 
  tab_options(data_row.padding = px(7),
              table.font.size = px(20)) %>% 
  gtsave("figures/top_dbs.png", expand = 3)



# bottom ------------------------------------------------------------------


table_data %>%
  arrange( technique) %>%
  filter(tackles >= 11 & position == 'LB') %>% 
  head(10) %>% 
  gt() %>%
  tab_header(
    title = 'Bottom LBs: Control Based Tackling Performance',
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
    domain = c(0.14,-0.3)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Performance',
    tackles = 'Tackle Events'
  ) %>% 
  tab_options(data_row.padding = px(7),
              table.font.size = px(20)) %>% 
  gtsave("figures/bottom_lbs.png", expand = 3)

table_data %>%
  arrange( technique) %>%
  filter(tackles >= 11 & position == 'DL') %>% 
  head(10) %>% 
  gt() %>%
  tab_header(
    title = 'Bottom DLs: Control Based Tackling Performance',
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
    domain = c(0.19,-0.19)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Performance',
    tackles = 'Tackle Events'
  ) %>% 
  tab_options(data_row.padding = px(7),
              table.font.size = px(20)) %>% 
  gtsave("figures/bottom_dls.png", expand = 3)

table_data %>%
  arrange( technique) %>%
  filter(tackles >= 11 & position == 'DB') %>% 
  head(10) %>% 
  gt() %>%
  tab_header(
    title = 'Bottom DBs: Control Based Tackling Performance',
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
    domain = c(0.14,-0.24)
  ) %>% 
  fmt_percent(
    columns = c(position_tackle, tackle_pct, technique), decimals = 1
  ) %>% 
  cols_label(
    displayName = 'Player',
    position = 'Pos',
    position_tackle = 'Avg. Tackle Prob.',
    tackle_pct = 'Tackle %',
    technique = 'Performance',
    tackles = 'Tackle Events'
  ) %>% 
  tab_options(data_row.padding = px(7),
              table.font.size = px(18)) %>% 
  gtsave("figures/bottom_dbs.png", expand = 3)
  