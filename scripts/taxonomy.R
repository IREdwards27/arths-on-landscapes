
# setup -------------------------------------------------------------------

library(taxize)
library(tidyverse)
library(lubridate)

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_arths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_arths')])

taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])


# retrieve ITIS IDs -------------------------------------------------------

# get ITIS IDs for each taxon and write a data frame with taxonomic info
new_taxa <- tibble(
  # generate a unique list of taxa observed in pitfall traps or beat sheets
  taxon = unique(c('Iolania perkinsi', foliage_arths$Taxon[!is.na(foliage_arths$Taxon)], ground_arths$Taxon[!is.na(ground_arths$Taxon)]))) %>%
  filter(taxon == 'Iolania perkinsi' | !taxon %in% taxa$taxon) %>% 
  # get the TSN for each taxon from ITIS
  mutate(TaxonID = get_tsn(taxon))

# get full taxonomy for all observed taxa
ranks <- map(
  # pull and reformat taxonomies
  .x = new_taxa$TaxonID[!is.na(new_taxa$TaxonID)],
  ~  classification(.x, db = 'itis')[[1]] %>%
    select(1:2) %>%
    pivot_wider(
      names_from = 'rank',
      values_from = 'name')) %>%
  bind_rows() %>%
  cbind(
    TaxonID = new_taxa$TaxonID[!is.na(new_taxa$TaxonID)],
    taxon = new_taxa$taxon[!is.na(new_taxa$TaxonID)]) %>%
  mutate(subfamily = NA) %>% 
  select(
    c(TaxonID, taxon, class, order, suborder, family, subfamily, genus, species)) %>%
  # this part is only necessary if ITIS doesn't recognize any of the new taxa; if it recognizes all of them, this gives an error (trying to add an empty row)
  # add_row(
  #   TaxonID = NA,
  #   taxon = new_taxa$taxon[is.na(new_taxa$TaxonID)],
  #   class = NA,
  #   order = NA,
  #   suborder = NA,
  #   family = NA,
  #   subfamily = NA,
  #   genus = NA,
  #   species = NA) %>% 
  rbind(taxa)

write.csv(ranks, file = paste('data/taxa_', today(), '.csv'), row.names = F)


# adding ITIS IDs to observation frames -----------------------------------

taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])

ground_ids <- ground_arths %>% 
  select(!TaxonID) %>% 
  left_join(
    taxa %>% 
      select(TaxonID, taxon),
    by = c('Taxon' = 'taxon')) %>% 
  distinct()

write.csv(ground_ids, file = paste('data/ground_arths_', today(), '.csv'), row.names = F)

foliage_ids <- foliage_arths %>% 
  select(!TaxonID) %>% 
  left_join(
    taxa %>% 
      select(TaxonID, taxon),
    by = c('Taxon' = 'taxon')) %>% 
  distinct()

write.csv(foliage_ids, file = paste('data/foliage_arths_', today(), '.csv'), row.names = F)

# how do we incorporate species/taxa not in itis into analyses? will probably need to fill in taxonomy manually and assign an ID 
taxa[is.na(taxa$tsn),]
