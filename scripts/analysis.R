
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


# foliage dataframe processing (site level) -------------------------------------

# calculate total family diversity observed at each site (not standardized by number of surveys)
foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
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
  summarize(fam_div = n_distinct(family))

# calculate abundance as mean # of arths per survey (not standardized for missing values)
foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(BeatSheetFK) %>% 
  summarize(arths_per_survey = sum(Quantity, na.rm = T)) %>% 
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
  summarize(mean_arths_per_survey = mean(arths_per_survey, na.rm = T))

# calculate abundance as mean biomass per survey (not standardized for missing values)
foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(BeatSheetFK) %>% 
  summarize(mass_per_survey = sum(TotalMass, na.rm = T)) %>% 
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
  summarize(mean_mass_per_survey = mean(mass_per_survey, na.rm = T))


# ground dataframe processing (site level) -------------------------------------

# calculate family diversity (not standardized)
ground_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>%
  left_join(
    pitfalls,
    by = 'PitfallID') %>% 
  left_join(
    circles,
    by = 'CircleID') %>% 
  group_by(SiteFK) %>% 
  summarize(fam_div = n_distinct(family))

# calculate abundance as # of arths per trap (not standardized)
ground_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>%
  group_by(PitfallID) %>% 
  summarize(arths_per_trap = sum(Number, na.rm = T)) %>% 
  left_join(
    pitfalls,
    by = 'PitfallID') %>% 
  left_join(
    circles,
    by = 'CircleID') %>% 
  group_by(SiteFK) %>% 
  summarize(mean_arths_per_trap = mean(arths_per_trap, na.rm = T))

# calculate abundance as biomass per trap (not standardized)
ground_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>%
  group_by(PitfallID) %>% 
  summarize(mass_per_trap = sum(TotalMass, na.rm = T)) %>% 
  left_join(
    pitfalls,
    by = 'PitfallID') %>% 
  left_join(
    circles,
    by = 'CircleID') %>% 
  group_by(SiteFK) %>% 
  summarize(mean_mass_per_trap = mean(mass_per_trap, na.rm = T))


# next steps --------------------------------------------------------------

# join arth stats to landscape stats
# join ground arth stats to circle stats
# join foliage arth stats to tree stats?
# review model possibilities for site-level and circle-level modeling - notes from ENEC 562
## for site-level, what can best account for small sample size (6 replicates)?
## predictor variable selection using what type of model selection?
## start with GLM and go from there?

# what has to happen first?
## finish bug ID
## review uncertain identifications
## add IDs and taxonomies to taxa not in ITIS
## add mass estimations or protocol for beat sheets with lost arths