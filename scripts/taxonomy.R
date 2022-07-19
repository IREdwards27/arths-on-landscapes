
# setup -------------------------------------------------------------------

library(taxize)
library(tidyverse)
library(lubridate)

foliage_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])


# retrieve ITIS IDs -------------------------------------------------------

# rework this to have a dataframe of taxa with itis id's and all taxonomic levels rather than running the whole thing every time

taxa <- tibble(
  taxon = unique(c('Polistes fuscatus', foliage_arths$Taxon[!is.na(foliage_arths$Taxon)], ground_arths$Taxon[!is.na(ground_arths$Taxon)]))) %>% 
  left_join(
    str_replace(.$taxon, ' ', '\\\\\ ') %>%
          paste0('nameWOInd:', .) %>%
          map_df(~ritis::itis_search(q = .)) %>% 
      select(tsn, nameWOInd),
    by = c('taxon' = 'nameWOInd')) %>% 
  arrange(tsn)

ranks <- map(
  .x = taxa$tsn[!is.na(taxa$tsn)],
  ~  classification(.x, db = 'itis')[[1]] %>%
    select(1:2) %>% 
    pivot_wider(
      names_from = 'rank',
      values_from = 'name')) %>% 
  bind_rows() %>% 
  cbind(
    tsn = taxa$tsn[!is.na(taxa$tsn)],
    taxon = taxa$taxon[!is.na(taxa$tsn)]) %>% 
  select(!kingdom:subphylum) %>% 
  rbind(taxa[is.na(taxa$tsn),] %>% 
          mutate(
            class = NA,
            subclass = NA,
            infraclass = NA,
            order = NA,
            suborder = NA,
            superfamily = NA,
            family = NA,
            infraorder = NA,
            subfamily = NA,
            genus = NA,
            species = NA,
            superorder = NA,
            tribe = NA,
            subgenus = NA,
            subtribe = NA)) %>% 
  relocate(tsn:taxon)

write.csv(ranks, file = paste('data/taxa_', today(), '.csv'), row.names = F)
