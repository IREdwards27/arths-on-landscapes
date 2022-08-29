
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# read in foliage and ground arthropod observations - code format pulls most recent date

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])

# read in taxa

taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])

# ????
foliage_arths %>% 
  left_join(
    taxa,
    by = c('ITISID' = 'tsn')) %>% 
  filter(!is.na(family)) %>% 
  group_by(BeatSheetFK) %>% 
  summarize(fam_div = n_distinct(family))
