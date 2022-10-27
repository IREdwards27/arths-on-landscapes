
# setup -------------------------------------------------------------------

library(tidyverse)

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


# generate data frame of foliage arth observations for rechecking ---------

foliage_oddities <- foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(family) %>% 
  summarize(n = n()) %>% 
  filter(n<3)

foliage_rechecks <- foliage_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(
    family %in% c('Tenebrionidae','Melandryidae','Trogossitidae','Nitidulidae','Anyphaenidae','Clubionidae','Berytidae','Phalangiidae',foliage_oddities$family) |
      order %in% c('Siphonophorida','Julida') |
      genus %in% c('Formica','Camponotus','Ponera','Hypoponera','Prenolepis','Paratrechina')) %>%  
  left_join(
    beatsheets,
    by = c('BeatSheetFK' = 'BeatSheetID')) %>% 
  select(c(1:2,TaxonLevel,Taxon,TotalMass,TaxonID,NumberWeighed,TreeFK,Date,Checks))

write_csv(
  foliage_rechecks,
  'data/foliage_rechecks.csv')


# generate data frame of ground arth observations for rechecking ----------

ground_oddities <- ground_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  group_by(family) %>% 
  summarize(n = n()) %>% 
  filter(n<3)

ground_rechecks <- ground_arths %>% 
  left_join(
    taxa,
    by = 'TaxonID') %>% 
  filter(
    family %in% c('Tenebrionidae','Melandryidae','Trogossitidae','Nitidulidae','Anyphaenidae','Clubionidae','Berytidae','Phalangiidae',ground_oddities$family) |
      order %in% c('Siphonophorida','Julida') |
      genus %in% c('Formica','Camponotus','Ponera','Hypoponera','Prenolepis','Paratrechina')) %>% 
  left_join(
    pitfalls,
    by = 'PitfallID') %>% 
  select(c(1:6,TaxonID,CircleID,DateCollected,Checks))

write_csv(
  ground_rechecks,
  'data/ground_rechecks.csv')
