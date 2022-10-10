
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(geosphere)

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


# calculating community dissimilarity metrics (foliage) -------------------------

foliage_families <- foliage_arths %>% 
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
  group_by(CircleFK, family) %>%
  summarize(
    n_individuals = sum(Quantity, na.rm = T),
    biomass = sum(TotalMass))

allFams <- sort(unique(foliage_families$family))

family_circles <- map_dfc(
  unique(foliage_families$CircleFK),
  ~ foliage_families %>% 
    filter(
      CircleFK == .x,
      !is.na(family)) %>% 
    bind_rows(tibble(
      family = allFams[!allFams %in% .$family])) %>% 
    mutate(
      CircleFK = if_else(
        is.na(CircleFK),
        true = .x,
        false = CircleFK),
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      logBiomass = log(biomass+1)) %>% 
    select(!n_individuals:biomass) %>% 
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>% 
    t() %>% 
    row_to_names(row_number = 1) %>% 
    as_tibble(rownames = 'family') %>% 
    arrange(family) %>% 
    select(!family)) %>% 
  cbind(family = allFams) %>% 
  relocate(family) %>% 
  as_tibble() %>% 
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))

euclidean <- function(a, b) sqrt(sum((a - b)^2))

euclidean_matrix <- map_dfr(
  .x = 2:31,
  .f = function(s){
    map_dfc(
      .x = 2:31,
      .f = ~ euclidean(family_circles[[s]], family_circles[[.x]])
      )
  }) %>% 
  set_names(names(family_circles)[2:31]) %>% 
  cbind(circle = names(family_circles)[2:31]) %>% 
  relocate(circle)

jaccard <- function(a,b,c) {a/(a+b+c)}

jaccard_matrix <- map_dfr(
  .x = circles$CircleID,
  .f = function(s){
    map_dfc(
      .x = circles$CircleID,
      .f = ~
        1 - jaccard(
          a = sum(foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
          b = sum(!foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
          c = sum(!foliage_families$family[foliage_families$CircleFK == .x] %in% foliage_families$family[foliage_families$CircleFK == s]))
    )
  }) %>% 
  set_names(circles$CircleID) %>% 
  cbind(circle = circles$CircleID) %>% 
  relocate(circle)


# calculating distance metrics (foliage) ------------------------------------

canopy_cover_matrix <- map_dfr(
  .x = 1:30,
  .f = function(s){
    map_dfc(
      .x = 1:30,
      .f = ~ abs(circles$PercentCanopyCover[s] - circles$PercentCanopyCover[.x]))
  }) %>% 
  set_names(circles$CircleID) %>% 
  cbind(circle = circles$CircleID) %>% 
  relocate(circle)

distance_edge_matrix <- map_dfr(
  .x = 1:30,
  .f = function(s){
    map_dfc(
      .x = 1:30,
      .f = ~ abs(circles$DistanceToEdgem[s] - circles$DistanceToEdgem[.x]))
  }) %>% 
  set_names(circles$CircleID) %>% 
  cbind(circle = circles$CircleID) %>% 
  relocate(circle)

distance_matrix <- circles %>% 
  select(Longitude, Latitude) %>% 
  distm() %>% 
  as_tibble() %>% 
  set_names(circles$CircleID) %>% 
  cbind(circle = circles$CircleID) %>% 
  relocate(circle)

forest_matrix <- map_dfr(
  .x = 1:6,
  .f = function(s){
    map_dfc(
      .x = 1:6,
      .f = ~ abs(sites$forest_1km[s] - sites$forest_1km[.x]))
  }) %>% 
  set_names(sites$SiteID) %>% 
  cbind(site1 = sites$SiteID) %>% 
  relocate(site1)


# creating a model-friendly data frame (foliage) ----------------------------

analysis_frame <- euclidean_matrix %>% 
  pivot_longer(
    cols = 2:length(.),
    names_to = 'circle2',
    values_to = 'euclideanDistance') %>% 
  filter(circle != circle2) %>% 
  distinct(euclideanDistance, .keep_all = T) %>% 
  left_join(
    jaccard_matrix %>% 
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'jaccardDissimilarity'),
    by = c('circle','circle2')) %>% 
  left_join(
    canopy_cover_matrix %>% 
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'canopyCover'),
    by = c('circle','circle2')) %>% 
  left_join(
    distance_edge_matrix %>% 
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'distanceToEdge'),
    by = c('circle','circle2')) %>% 
  left_join(
    distance_matrix %>% 
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'geographicDistance'),
    by = c('circle','circle2')) %>% 
  rename('circle1' = 'circle') %>% 
  mutate(
    site1 = case_when(
      str_detect(circle1, 'DF') ~ 'DF',
      str_detect(circle1, 'ERSP') ~ 'ERSP',
      str_detect(circle1, 'JMNP') ~ 'JMNP',
      str_detect(circle1, 'NCBG') ~ 'NCBG',
      str_detect(circle1, 'NCSU') ~ 'NCSU',
      str_detect(circle1, 'UNC') ~ 'UNC'),
    site2 = case_when(
      str_detect(circle2, 'DF') ~ 'DF',
      str_detect(circle2, 'ERSP') ~ 'ERSP',
      str_detect(circle2, 'JMNP') ~ 'JMNP',
      str_detect(circle2, 'NCBG') ~ 'NCBG',
      str_detect(circle2, 'NCSU') ~ 'NCSU',
      str_detect(circle2, 'UNC') ~ 'UNC')) %>% 
  left_join(
    forest_matrix %>% 
      pivot_longer(
        cols = 2:length(.),
        names_to = 'site2',
        values_to = 'forest_1km'),
    by = c('site1','site2'))



# calculating environmental distance metrics (ground) ---------------------

herbaceous_matrix <- map_dfr(
  .x = 1:30,
  .f = function(s){
    map_dfc(
      .x = 1:30,
      .f = ~ abs(circles$HerbCoverEstimate[s] - circles$HerbCoverEstimate[.x]))
  }) %>% 
  set_names(circles$CircleID) %>% 
  cbind(circle = circles$CircleID) %>% 
  relocate(circle)
  
litter_depth_matrix <- map_dfr(
  .x = 1:30,
  .f = function(s){
    map_dfc(
      .x = 1:30,
      .f = ~ abs(circles$LitterDepthmm[s] - circles$LitterDepthmm[.x]))
  }) %>% 
  set_names(circles$CircleID) %>% 
  cbind(circle = circles$CircleID) %>% 
  relocate(circle)
