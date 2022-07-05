
# setup -------------------------------------------------------------------

library(taxize)
library(tidyverse)

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])


# retrieve ITIS IDs -------------------------------------------------------

# rework this to have a dataframe of taxa with itis id's and all taxonomic levels rather than running the whole thing every time

tax_name('Phrurotimpus alarius', get = unique(foliage_arths$TaxonLevel))

get_tsn('Phrurotimpus alarius')

classification('Phrurotimpus alarius', db = 'itis')
