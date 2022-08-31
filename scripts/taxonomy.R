
# setup -------------------------------------------------------------------

library(taxize)
library(tidyverse)
library(lubridate)

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])


# retrieve ITIS IDs -------------------------------------------------------

# get ITIS IDs for each taxon and write a data frame with taxonomic info
taxa <- tibble(
  # generate a unique list of taxa observed in pitfall traps or beath sheets
  taxon = unique(c('Polistes fuscatus', foliage_arths$Taxon[!is.na(foliage_arths$Taxon)], ground_arths$Taxon[!is.na(ground_arths$Taxon)]))) %>%
  # get the TSN for each taxon from ITIS
  mutate(TaxonID = get_tsn(taxon))

# get full taxonomy for all observed taxa
ranks <- map(
  # pull and reformat taxonomies
  .x = taxa$TaxonID[!is.na(taxa$TaxonID)],
  ~  classification(.x, db = 'itis')[[1]] %>%
    select(1:2) %>%
    pivot_wider(
      names_from = 'rank',
      values_from = 'name')) %>%
  bind_rows() %>%
  cbind(
    TaxonID = taxa$TaxonID[!is.na(taxa$TaxonID)],
    taxon = taxa$taxon[!is.na(taxa$TaxonID)]) %>%
  select(!kingdom:subphylum) %>%
  rbind(taxa[is.na(taxa$TaxonID),] %>% 
          mutate(
            class = NA,
            subclass = NA,
            infraclass = NA,
            superorder = NA,
            order = NA,
            suborder = NA,
            infraorder = NA,
            superfamily = NA,
            family = NA,
            genus = NA,
            species = NA,
            subfamily = NA,
            tribe = NA,
            subgenus = NA,
            subtribe = NA)) %>% 
  relocate(TaxonID:taxon)

write.csv(ranks, file = paste('data/taxa_', today(), '.csv'), row.names = F)


# adding ITIS IDs to observation frames -----------------------------------

taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])

ground_ids <- ground_arths %>% 
  select(!TaxonID) %>% 
  left_join(
    taxa %>% 
      select(TaxonID, taxon),
    by = c('Taxon' = 'taxon'))

write.csv(ground_ids, file = paste('data/groundarths_', today(), '.csv'), row.names = F)

foliage_ids <- foliage_arths %>% 
  select(!TaxonID) %>% 
  left_join(
    taxa %>% 
      select(TaxonID, taxon),
    by = c('Taxon' = 'taxon'))

write.csv(foliage_ids, file = paste('data/foliagearths_', today(), '.csv'), row.names = F)

# how do we incorporate species/taxa not in itis into analyses? will probably need to fill in taxonomy manually and assign an ID 
taxa[is.na(taxa$tsn),]
