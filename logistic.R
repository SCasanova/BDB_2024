library(tidyverse)
library(arrow)


filenames <- list.files(glue::glue("./quality/"), 
                        full.names = TRUE)

training <- tibble()
for(f in filenames){
  training <- bind_rows(training, read_parquet(f))
}


tackles <- read_csv('data/tackles.csv') %>% 
  filter(tackle != pff_missedTackle | assist != pff_missedTackle) %>% 
  mutate(tackling = ifelse(tackle == 1 | assist == 1, 1, 0))
