# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)

# read in foliage and ground arthropod observations - code format pulls most recent date

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])


# foliage arthropod corrections -------------------------------------------

foliage_corrected <- foliage_arths %>% 
  mutate(
    TaxonLevel = case_when(
      str_detect(
        Taxon,
        '(Isomira)|(Camponotus castaneus)|(Castianeira)|(Vonones)') ~ 'genus',
      str_detect(
        Taxon,
        '(Rhagonycha)|(Melanotus)|(Pardosa)|(Pirata)|(Hibana)|(Hentzia)') ~ 'family',
      TRUE ~ TaxonLevel),
    Taxon = case_when(
      str_detect(Taxon, 'Isomira') ~ 'Isomira',
      str_detect(Taxon, 'Rhagonycha') ~ 'Cantharidae',
      Taxon == 'Melanotus' ~ 'Elateridae',
      Taxon == 'Camponotus castaneus' ~ 'Camponotus',
      str_detect(Taxon, 'Castianeira') ~ 'Castianeira',
      str_detect(Taxon, '(Pardosa)|(Pirata)') ~ 'Lycosidae',
      str_detect(Taxon, 'Hibana') ~ 'Anyphaenidae',
      str_detect(Taxon, 'Hentzia') ~ 'Salticidae',
      str_detect(Taxon, 'Vonones') ~ 'Vonones',
      TRUE ~ Taxon))

ground_corrected <- ground_arths %>% 
  mutate(
    TaxonLevel = case_when(
      str_detect(
        Taxon,
        '(Isomira)|(Camponotus castaneus)|(Castianeira)|(Vonones)') ~ 'genus',
      str_detect(
        Taxon,
        '(Rhagonycha)|(Melanotus)|(Pardosa)|(Pirata)|(Hibana)|(Hentzia)') ~ 'family',
      TRUE ~ TaxonLevel),
    Taxon = case_when(
      str_detect(Taxon, 'Isomira') ~ 'Isomira',
      str_detect(Taxon, 'Rhagonycha') ~ 'Cantharidae',
      Taxon == 'Melanotus' ~ 'Elateridae',
      Taxon == 'Camponotus castaneus' ~ 'Camponotus',
      str_detect(Taxon, 'Castianeira') ~ 'Castianeira',
      str_detect(Taxon, '(Pardosa)|(Pirata)') ~ 'Lycosidae',
      str_detect(Taxon, 'Hibana') ~ 'Anyphaenidae',
      str_detect(Taxon, 'Hentzia') ~ 'Salticidae',
      str_detect(Taxon, 'Vonones') ~ 'Vonones',
      TRUE ~ Taxon))

write_csv(foliage_corrected, str_c('data/foliagearths_', today(), '.csv'))

write_csv(ground_corrected, str_c('data/groundarths_', today(), '.csv'))
