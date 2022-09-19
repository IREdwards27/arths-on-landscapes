
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)

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

sites <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^sites')])


# taxon composition assessment -------------------------------------

# calculate total number of observations for each class of foliage arths
foliage_arths %>% 
  filter(!is.na(TaxonID)) %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(class) %>% 
  summarize(number_observed = sum(Quantity, na.rm = T))

# calculate total number of observations of foliage arths by order
foliage_arths %>% 
  filter(!is.na(TaxonID)) %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(!is.na(family)) %>% 
  group_by(order) %>% 
  summarize(number_observed = sum(Quantity, na.rm = T))

# calculate total number of observations for each class for ground arths
ground_arths %>% 
  filter(!is.na(TaxonID)) %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(class) %>% 
  summarize(number_observed = sum(Number, na.rm = T))

# calculate total number of observations for each order for ground arths
ground_arths %>% 
  filter(!is.na(TaxonID)) %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(!is.na(family)) %>% 
  group_by(order) %>% 
  summarize(number_observed = sum(Number, na.rm = T))


# setting up analysis dataframes ------------------------------------------

# foliage arth site-level dataframe with family diversity and mean arthropod biomass per survey
foliage_sites <- foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(
    order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>% 
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
  summarize(fam_div = n_distinct(family)) %>% 
  left_join(
    foliage_arths %>% 
      left_join(
        taxa,
        by = 'TaxonID') %>% 
      filter(
        order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>% 
      group_by(BeatSheetFK) %>% 
      summarize(mass_per_survey = sum(TotalMass, na.rm = T)) %>% 
      right_join(
        beatsheets %>% 
          select(BeatSheetID, TreeFK),
        by = c('BeatSheetFK' = 'BeatSheetID')) %>% 
      mutate(mass_per_survey = if_else(
        is.na(mass_per_survey),
        true = 0,
        false = mass_per_survey)) %>% 
      left_join(
        trees %>% 
          select(TreeID, CircleFK),
        by = c('TreeFK' = 'TreeID')) %>% 
      left_join(
        circles %>% 
          select(CircleID, SiteFK),
        by = c('CircleFK' = 'CircleID')) %>% 
      group_by(SiteFK) %>% 
      summarize(mean_mass_per_survey = mean(mass_per_survey, na.rm = T)),
    by = 'SiteFK') %>% 
  left_join(
    sites %>% 
      select(c(SiteID, forest_2km, pafrac_5km, nlsi_2km, nlsi_5km:mn_night_temp, forest_1km, aw_cai_mn_1km, nlsi_1km, cai_mn_1km)),
    by = c('SiteFK' = 'SiteID'))

# ground arth site-level dataframe with family diversity and mean arthropod biomass per survey
ground_sites <- ground_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(
    order %in% c('Araneae','Archaeognatha','Coleoptera','Isopoda', 'Opiliones', 'Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>% 
  left_join(
    pitfalls,
    by = 'PitfallID') %>% 
  left_join(
    circles,
    by = 'CircleID') %>% 
  group_by(SiteFK) %>% 
  summarize(fam_div = n_distinct(family)) %>% 
  left_join(
    ground_arths %>% 
      left_join(
        taxa,
        by = 'TaxonID') %>% 
      filter(
        order %in% c('Araneae','Archaeognatha','Coleoptera','Isopoda', 'Opiliones', 'Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>%
      group_by(PitfallID) %>% 
      summarize(mass_per_trap = sum(TotalMass, na.rm = T)) %>% 
      left_join(
        pitfalls,
        by = 'PitfallID') %>% 
      left_join(
        circles,
        by = 'CircleID') %>% 
      group_by(SiteFK) %>% 
      summarize(mean_mass_per_trap = mean(mass_per_trap, na.rm = T)),
    by = 'SiteFK') %>% 
  left_join(
    sites %>% 
      select(c(SiteID, forest_2km, pafrac_5km, nlsi_2km, nlsi_5km:mn_night_temp, forest_1km, aw_cai_mn_1km, nlsi_1km, cai_mn_1km)),
    by = c('SiteFK' = 'SiteID'))


# visualization -----------------------------------------------------------

ggplot(foliage_sites) +
  geom_point(aes(
    x = forest_1km,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(foliage_sites) +
  geom_point(aes(
    x = cai_mn_1km,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(foliage_sites) +
  geom_point(aes(
    x = nlsi_1km,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(foliage_sites) +
  geom_point(aes(
    x = mn_day_temp,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(foliage_sites) +
  geom_point(aes(
    x = mn_night_temp,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(ground_sites) +
  geom_point(aes(
    x = forest_1km,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_sites) +
  geom_point(aes(
    x = cai_mn_1km,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_sites) +
  geom_point(aes(
    x = nlsi_1km,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_sites) +
  geom_point(aes(
    x = mn_day_temp,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_sites) +
  geom_point(aes(
    x = mn_night_temp,
    y = mean_mass_per_trap,
    color = fam_div))

# modeling ----------------------------------------------------------------

ground_abundance_model <- lm(
  mean_mass_per_trap ~ forest_1km + cai_mn_1km + nlsi_1km,
  data = ground_sites)

summary(ground_abundance_model)

ground_diversity_model <- lm(
  fam_div ~ forest_1km + cai_mn_1km + nlsi_1km,
  data = ground_sites)

summary(ground_diversity_model)

foliage_abundance_model <- lm(
  mean_mass_per_survey ~ forest_1km + cai_mn_1km + nlsi_1km,
  data = foliage_sites)

summary(foliage_abundance_model)

foliage_diversity_model <- lm(
  fam_div ~ forest_1km + cai_mn_1km + nlsi_1km,
  data = foliage_sites)

summary(foliage_diversity_model)


# next steps --------------------------------------------------------------

# separate abundance and diversity frames - abundance should be one row for the mass in each survey so modeling calculates mean, total diversity cannot be - how do we handle this?
# review model possibilities - notes from ENEC 562
## for site-level, what can best account for small sample size (6 replicates)?
## predictor variable selection using what type of model selection?
## start with GLM and go from there?

# what has to happen first?
## finish bug ID
## review uncertain identifications
## add IDs and taxonomies to taxa not in ITIS
## add mass estimations or protocol for beat sheets with lost arths
## select arth groups to analyze