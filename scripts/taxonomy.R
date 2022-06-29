
# setup -------------------------------------------------------------------

library(taxize)
library(tidyverse)

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])


# retrieve ITIS IDs -------------------------------------------------------

# rework this to have a dataframe of taxa with itis id's and all taxonomic levels rather than running the whole thing every time

foliage_arths %>% 
  select(Taxon, TaxonLevel) %>% 
  unique()

get_ids(sci_com = 'Phrurotimpus alarius', db = 'itis')$itis[1] %>% str_extract('[0-9]*')

tax_name('Phrurotimpus alarius', get = unique(foliage_arths$TaxonLevel))
