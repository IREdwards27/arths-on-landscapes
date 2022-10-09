
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

# foliage arth circle-level dataframe with family diversity, mean arthropod biomass per survey, cicle environmental variables, and forest cover w/in 1km radius
foliage_circles <- foliage_arths %>% 
  # filter to confident IDs while still working on bug ID
  mutate(Taxon = case_when(
    Taxon == 'Trogossitidae' ~ 'Nitidulidae',
    Taxon %in% c('Ponera','Hypoponera') ~ 'Brachyponera chinensis')) %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(
    order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>% 
  left_join(
    beatsheets %>% 
      select(BeatSheetID, Date, TreeFK),
    by = c('BeatSheetFK' = 'BeatSheetID')) %>% 
  # filter to first two rounds of data collection while finishing bug ID
  filter(as.Date(Date, format = '%m/%d/%Y') < as.Date('2022-06-26')) %>% 
  left_join(
    trees %>% 
      select(TreeID, CircleFK),
    by = c('TreeFK' = 'TreeID')) %>% 
  left_join(
    circles,
    by = c('CircleFK' = 'CircleID')) %>% 
  group_by(CircleFK) %>% 
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
      group_by(CircleFK) %>% 
      summarize(mean_mass_per_survey = mean(mass_per_survey, na.rm = T)),
    by = 'CircleFK') %>% 
  left_join(
    circles,
    by = c('CircleFK' = 'CircleID')) %>% 
  left_join(
    sites %>% 
      select(c(SiteID, forest_1km)),
    by = c('SiteFK' = 'SiteID'))

# ground arth circle-level dataframe with family diversity, mean arthropod biomass per survey, circle-level environmental traits, and 1km forest cover
ground_circles <- ground_arths %>% 
  # filter to confident IDs while still working on bug ID
  mutate(Taxon = case_when(
    Taxon == 'Trogossitidae' ~ 'Nitidulidae',
    Taxon %in% c('Ponera','Hypoponera') ~ 'Brachyponera chinensis')) %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(
    order %in% c('Araneae','Archaeognatha','Coleoptera','Isopoda', 'Opiliones', 'Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>% 
  left_join(
    pitfalls,
    by = 'PitfallID') %>% 
  # filter to first two rounds of data collection while finishing bug ID
  filter(as.Date(DateDeployed, format = '%m/%d/%Y') < as.Date('2022-06-26')) %>% 
  left_join(
    circles,
    by = 'CircleID') %>% 
  group_by(CircleID) %>% 
  summarize(fam_div = n_distinct(family)) %>% 
  left_join(
    ground_arths %>% 
      # filter to confident IDs while still working on bug ID
      mutate(Taxon = case_when(
        Taxon == 'Trogossitidae' ~ 'Nitidulidae',
        Taxon %in% c('Ponera','Hypoponera') ~ 'Brachyponera chinensis')) %>%
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
      group_by(CircleID) %>% 
      summarize(mean_mass_per_trap = mean(mass_per_trap, na.rm = T)),
    by = 'CircleID') %>% 
  left_join(
    circles,
    by = 'CircleID') %>% 
  left_join(
    sites %>% 
      select(c(SiteID, forest_1km)),
    by = c('SiteFK' = 'SiteID'))


# visualization -----------------------------------------------------------

ggplot(foliage_circles) +
  geom_point(aes(
    x = PercentCanopyCover,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(foliage_circles) +
  geom_point(aes(
    x = DistanceToEdgem,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(foliage_circles) +
  geom_point(aes(
    x = forest_1km,
    y = mean_mass_per_survey,
    color = fam_div))

ggplot(ground_circles) +
  geom_point(aes(
    x = LitterDepthmm,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_circles) +
  geom_point(aes(
    x = HerbCoverEstimate,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_circles) +
  geom_point(aes(
    x = DistanceToEdgem,
    y = mean_mass_per_trap,
    color = fam_div))

ggplot(ground_circles) +
  geom_point(aes(
    x = forest_1km,
    y = mean_mass_per_trap,
    color = fam_div))

# modeling ----------------------------------------------------------------

ground_abundance_model <- lm(
  mean_mass_per_trap ~ LitterDepthmm + HerbCoverEstimate + DistanceToEdgem + forest_1km,
  data = ground_circles)

summary(ground_abundance_model)

ground_diversity_model <- lm(
  fam_div ~ LitterDepthmm + HerbCoverEstimate + DistanceToEdgem + forest_1km,
  data = ground_circles)

summary(ground_diversity_model)

foliage_abundance_model <- lm(
  mean_mass_per_survey ~ PercentCanopyCover + DistanceToEdgem + forest_1km,
  data = foliage_circles)

summary(foliage_abundance_model)

foliage_diversity_model <- lm(
  fam_div ~ PercentCanopyCover + DistanceToEdgem + forest_1km,
  data = foliage_circles)

summary(foliage_diversity_model)


# next steps --------------------------------------------------------------


# finish bug ID
# review uncertain identifications
# add mass estimations or protocol for beat sheets with lost arths
# review models with Allen
# start work on dissimilarity models