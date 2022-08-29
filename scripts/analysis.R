
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

# read in beat sheets

beatsheets <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^beatsheets')])

# read in pitfall traps

pitfalls <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^pitfallsurveys')])

# read in trees

trees <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])

# read in circles

circles <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^circles')])


# dataframe processing ----------------------------------------------------


foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(BeatSheetFK) %>% 
  summarize(fam_div = n_distinct(family)) %>% 
  left_join(
    beatsheets %>% 
      select(BeatSheetID, TreeFK),
    by = c('BeatSheetFK' = 'BeatSheetID')) %>% 
  left_join(
    trees %>% 
      select(TreeID, CircleFK),
    by = c('TreeFK' = 'TreeID')) %>% 
  left_join(
    circles %>% 
      select(CircleID, SiteFK),
    by = c('CircleFK' = 'CircleID')) %>% 
  group_by(SiteFK) %>% 
  summarize(mean_fam_div = mean(fam_div))
