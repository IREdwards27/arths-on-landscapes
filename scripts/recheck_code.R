
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

read_csv('data/foliage_rechecks.csv') %>% 
  rbind(foliage_arths %>% 
          left_join(
            taxa,
            by = 'TaxonID') %>% 
          filter(
            family %in% c('Araneidae','Theridiidae','Linyphiidae','Tetragnathidae')) %>%  
          left_join(
            beatsheets,
            by = c('BeatSheetFK' = 'BeatSheetID')) %>% 
          select(c(1:2,TaxonLevel,Taxon,TotalMass,TaxonID,NumberWeighed,TreeFK,Date,Checks))) %>% 
  write_csv('data/foliage_rechecks.csv')

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

read_csv('data/ground_rechecks.csv') %>% 
  rbind(ground_arths %>% 
          left_join(
            taxa,
            by = 'TaxonID') %>% 
          filter(
            family %in% c('Araneidae','Theridiidae','Linyphiidae','Tetragnathidae')) %>% 
          left_join(
            pitfalls,
            by = 'PitfallID') %>% 
          select(c(1:6,TaxonID,CircleID,DateCollected,Checks))) %>% 
  write_csv('data/ground_rechecks.csv')

write_csv(
  ground_rechecks,
  'data/ground_rechecks.csv')


# re-binding checked files ------------------------------------------------

foliage_checked <- read_csv('data/foliage_rechecks.csv')

ground_checked <- read_csv('data/ground_rechecks.csv')

checked_beats <- unique(foliage_checked$BeatSheetFK)

updated_beats <- beatsheets %>% 
  mutate(Checks = if_else(
    BeatSheetID %in% checked_beats,
    true = 2,
    false = Checks))

write_csv(updated_beats, str_c('data/beat_sheets_', today(), '.csv'))

updated_foliage_arths <- foliage_arths %>% 
  filter(!FoliageArthID %in% foliage_checked$FoliageArthID) %>% 
  rbind(
    foliage_checked %>% 
      left_join(
        foliage_arths %>% 
          select(FoliageArthID, CCGroup, Length, Quantity, PhotoURL, CCNotes),
        by = 'FoliageArthID') %>% 
      select(names(foliage_arths)))

write_csv(updated_foliage_arths, str_c('data/foliage_arths_', today(), '.csv'))

updated_pits <- pitfalls %>% 
  mutate(Checks = if_else(
    PitfallID %in% ground_checked$PitfallID,
    true = 2,
    false = Checks))

write_csv(updated_pits, str_c('data/pitfalls_', today(), '.csv'))

updated_ground_arths <- ground_arths %>% 
  filter(!GroundArthID %in% ground_checked$GroundArthID) %>% 
  rbind(
    ground_checked %>% 
      left_join(
        ground_arths %>% 
          select(GroundArthID, Notes),
        by = 'GroundArthID') %>% 
      select(names(ground_arths)))

write_csv(updated_ground_arths, str_c('data/ground_arths_', today(), '.csv'))
