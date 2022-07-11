
# setup -------------------------------------------------------------------

library(taxize)
library(tidyverse)

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])


# retrieve ITIS IDs -------------------------------------------------------

# rework this to have a dataframe of taxa with itis id's and all taxonomic levels rather than running the whole thing every time

taxa <- tibble(
  taxon = unique(c('Polistes fuscatus', foliage_arths$Taxon[!is.na(foliage_arths$Taxon)], ground_arths$Taxon[!is.na(ground_arths$Taxon)]))) %>% 
  cbind(
    tsn = get_tsn(.$taxon) %>% as.numeric())

ranks <- map(
  .x = taxa$tsn[!is.na(taxa$tsn)],
  ~  classification(.x, db = 'itis')[[1]] %>%
    select(1:2) %>% 
    pivot_wider(
      names_from = 'rank',
      values_from = 'name')) %>% 
  bind_rows() %>% 
  cbind(tsn = taxa$tsn)
    
