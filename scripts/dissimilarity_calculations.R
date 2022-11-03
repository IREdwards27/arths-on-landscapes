
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(geosphere)
library(ggpubr)
library(ade4)


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
  mutate(TaxonID = case_when(
    # switch all Trogossitidae to Nitidulidae
    TaxonID == 678393 ~ 114290,
    # switch all Ponera and Hypoponera to Brachyponera chinensis
    TaxonID %in% c(574209,574195) ~ 11,
    TRUE ~ TaxonID)) %>%
  left_join(
    taxa,
    by = 'TaxonID') %>%
  # filter to focal orders
  filter(
    order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>%
  left_join(
    beatsheets %>%
      select(BeatSheetID, Date, TreeFK),
    by = c('BeatSheetFK' = 'BeatSheetID')) %>%
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

foliageFams <- sort(unique(foliage_families$family))

# make a data frame with sites as columns and families as rows
family_circles_foliage <- map_dfc(
  unique(foliage_families$CircleFK),
  ~ foliage_families %>%
    # select families observed at a particular circle
    filter(
      CircleFK == .x,
      !is.na(family)) %>%
    # add families that were not observed at a particular circle
    bind_rows(tibble(
      family = foliageFams[!foliageFams %in% .$family])) %>%
    mutate(
      # populate circle ID to non-observed family rows
      CircleFK = if_else(
        is.na(CircleFK),
        true = .x,
        false = CircleFK),
      # fill in 0 values for non-observed family biomass
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      # take the log of biomass, adding 1/10th smallest value so zero observations can be included
      logBiomass = log(biomass+0.01)) %>%
    # select to relevant columns
    select(!n_individuals:biomass) %>%
    # pivot everything out
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    # transpose it to an uglier but technically better frame
    t() %>%
    # get the circle names out of the first row
    row_to_names(row_number = 1) %>%
    # make it a real boy
    as_tibble(rownames = 'family') %>%
    # put it in alphabetical order by family
    arrange(family) %>%
    # remove family
    select(!family)) %>%
  # put family back in at the end of the map function
  cbind(family = foliageFams) %>%
  # put family at the front for my sanity
  relocate(family) %>%
  # duh
  as_tibble() %>%
  # why these didn't automatically come out numeric, god only knows
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))

# gonna be real, found this on the internet, but it's simple and correct
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# nested map function to apply euclidean function to every possible combination of circles, resulting in an identity matrix for euclidean distance between circle communities
euclidean_matrix_foliage <- map_dfr(
  .x = 2:31,
  .f = function(s){
    map_dfc(
      .x = 2:31,
      .f = ~ euclidean(family_circles_foliage[[s]], family_circles_foliage[[.x]])
      )
  }) %>%
  # setting the column names
  set_names(names(family_circles_foliage)[2:31]) %>%
  # setting the rows to mark for each circle
  cbind(circle = names(family_circles_foliage)[2:31]) %>%
  # again, for my peace of mind
  relocate(circle)

# I came up with this one, because I'm a genius
jaccard <- function(a,b,c) {a/(a+b+c)}

# make a matrix like the euclidean one but for jaccard dissimilarity
jaccard_matrix_foliage <- map_dfr(
  .x = circles$CircleID,
  .f = function(s){
    map_dfc(
      .x = circles$CircleID,
      .f = ~
        # the '1-' makes it dissimilarity instead of similarity
        1 - jaccard(
          # total number of families in both of two particular circles
          a = sum(foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
          # total number of families in the first circle but not the second
          b = sum(!foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
          # total number of families in the second circle but not the first
          c = sum(!foliage_families$family[foliage_families$CircleFK == .x] %in% foliage_families$family[foliage_families$CircleFK == s]))
    )
  }) %>%
  # I explained this part up there
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

tree_species <- trees %>% 
  select(CircleFK, Species) %>% 
  distinct()

trees_jaccard_matrix <- map_dfr(
  .x = circles$CircleID,
  .f = function(s){
    map_dfc(
      .x = circles$CircleID,
      .f = ~
        # the '1-' makes it dissimilarity instead of similarity
        1 - jaccard(
          # total number of families in both of two particular circles
          a = sum(tree_species$Species[tree_species$CircleFK == s] %in% tree_species$Species[tree_species$CircleFK == .x]),
          # total number of families in the first circle but not the second
          b = sum(!tree_species$Species[tree_species$CircleFK == s] %in% tree_species$Species[tree_species$CircleFK == .x]),
          # total number of families in the second circle but not the first
          c = sum(!tree_species$Species[tree_species$CircleFK == .x] %in% tree_species$Species[tree_species$CircleFK == s]))
    )
  }) %>%
  # I explained this part up there
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)


# calculating distance metrics (foliage) ------------------------------------

# make a matrix for difference in percent canopy cover between circles
canopy_cover_matrix <- dist(circles$PercentCanopyCover, diag = T, upper = T) %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

# these ones all work the same - this one is for distance to edge
distance_road_matrix <- dist(circles$DistanceToEdgem, diag = T, upper = T) %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

# this one's a little different because it uses the geosphere package, which has it's own "give me a matrix" function, so I just had to adjust format
distance_matrix <- circles %>%
  select(Longitude, Latitude) %>%
  distm() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

circles_forest <- circles %>%
  left_join(
    sites %>%
      select(SiteID, forest_1km),
    by = c('SiteFK' = 'SiteID'))

forest_matrix <- dist(circles_forest$forest_1km, diag = T, upper = T) %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle1 = circles$CircleID) %>%
  relocate(circle1)


# creating a model-friendly data frame (foliage) ----------------------------

analysis_frame_foliage <- euclidean_matrix_foliage %>%
  # shifts from a matrix to a frame defined by the first two columns - sorry, Hadley - corresponding to the euclidean distance between the two
  pivot_longer(
    cols = 2:length(.),
    names_to = 'circle2',
    values_to = 'euclideanDistance') %>%
  # remove rows for circles against themselves - not useful data
  filter(circle != circle2) %>%
  # take advantage of the lack of identical distances between any two site pairs to remove the duplicates resulting from flipping out a matrix
  distinct(euclideanDistance, .keep_all = T) %>%
  left_join(
    # pretty much the same thing
    jaccard_matrix_foliage %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'jaccardDissimilarity'),
    by = c('circle','circle2')) %>%
  left_join(
    # pretty much the same thing
    trees_jaccard_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'treeDissimilarity'),
    by = c('circle','circle2')) %>%
  left_join(
    canopy_cover_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'canopyCover'),
    by = c('circle','circle2')) %>%
  left_join(
    distance_road_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'distanceToRoad'),
    by = c('circle','circle2')) %>%
  left_join(
    distance_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'geographicDistance'),
    by = c('circle','circle2')) %>%
  rename('circle1' = 'circle') %>%
  left_join(
    forest_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'forest_1km'),
    by = c('circle1','circle2')) %>%
  mutate(
    # make a unique identifier (fine, Hadley)
    circles = str_c(circle1, circle2, sep = '_'),
    # convert canopy cover from percent to proportion
    canopyCover = canopyCover/100,
    # convert distance to edge from m to km
    distanceToRoad = distanceToRoad/1000,
    # convert geographic distance from m to km
    geographicDistance = geographicDistance/1000) %>%
  select(!circle1:circle2) %>%
  relocate(circles)

write_csv(
  analysis_frame_foliage,
  str_c('data/foliage_dissimilarity_', today(), '.csv'))

analysis_frame_foliage <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_dissimilarity')])

# initial modeling of foliage arths ---------------------------------------

summary(lm(
  euclideanDistance ~ jaccardDissimilarity,
  data = analysis_frame_foliage))

# p = 0.0249
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(canopy_cover_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0002
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# p = 0.9308
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(distance_road_matrix[2:31]), 
  nrepet = 9999)

# p = 0.3891
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0014
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(trees_jaccard_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.1392
euclidean_foliage_mod_full <- lm(
  euclideanDistance ~ canopyCover + geographicDistance + forest_1km,
  data = analysis_frame_foliage)

# R^2 = 0.088
euclidean_foliage_mod_canopy <- lm(
  euclideanDistance ~ canopyCover,
  data = analysis_frame_foliage)

# R^2 = 0.054
euclidean_foliage_mod_forest <- lm(
  euclideanDistance ~ forest_1km,
  data = analysis_frame_foliage)

# R^2 = 0.127
euclidean_foliage_mod_trees <- lm(
  euclideanDistance ~ treeDissimilarity,
  data = analysis_frame_foliage)

# R^2 = 0.1893
euclidean_foliage_mod_env <- lm(
  euclideanDistance ~ canopyCover + forest_1km + treeDissimilarity,
  data = analysis_frame_foliage)

# R^2 = 0.0009
euclidean_foliage_mod_dist <- lm(
  euclideanDistance ~ geographicDistance,
  data = analysis_frame_foliage)

# p = 0.0673
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(canopy_cover_matrix[2:31]), 
  nrepet = 9999)

# p = 0.7634
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(distance_road_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0001
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0585
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0011
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(trees_jaccard_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.1898
jaccard_foliage_mod_full <- lm(
  jaccardDissimilarity ~ canopyCover + geographicDistance + forest_1km + treeDissimilarity,
  data = analysis_frame_foliage)

# R^2 = 0.1886
jaccard_foliage_mod_env <- lm(
  jaccardDissimilarity ~ canopyCover + forest_1km + treeDissimilarity,
  data = analysis_frame_foliage)

# R^2 = 0.052
jaccard_foliage_mod_canopy <- lm(
  jaccardDissimilarity ~ canopyCover,
  data = analysis_frame_foliage)

# R^2 = 0.092
jaccard_foliage_mod_forest <- lm(
  jaccardDissimilarity ~ forest_1km,
  data = analysis_frame_foliage)

# R^2 = 0.1253
jaccard_foliage_mod_trees <- lm(
  jaccardDissimilarity ~ treeDissimilarity,
  data = analysis_frame_foliage)

# R^2 = 0.034
jaccard_foliage_mod_dist <- lm(
  jaccardDissimilarity ~ geographicDistance,
  data = analysis_frame_foliage)


# calculating environmental distance metrics (ground) ---------------------

herbaceous_matrix <- dist(circles$HerbCoverEstimate, diag = T, upper = T) %>% 
  as.matrix() %>% 
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

litter_depth_matrix <- dist(circles$LitterDepthmm, diag = T, upper = T) %>% 
  as.matrix() %>% 
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)


# calculating community dissimilarity metrics (ground) --------------------

ground_families <- ground_arths %>%
  # filter to confident IDs while still working on bug ID
  mutate(TaxonID = case_when(
    # switch all Trogossitidae to Nitidulidae
    TaxonID == 678393 ~ 114290,
    # switch all Ponera and Hypoponera to Brachyponera chinensis
    TaxonID %in% c(574209,574195) ~ 11,
    TRUE ~ TaxonID)) %>%
  left_join(
    taxa,
    by = 'TaxonID') %>%
  # filter to focal orders
  filter(
      order %in% c('Araneae','Archaeognatha','Coleoptera','Isopoda', 'Opiliones', 'Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>%
  left_join(
    pitfalls %>%
      select(PitfallID, DateCollected, CircleID),
    by = 'PitfallID') %>%
  left_join(
    circles,
    by = 'CircleID') %>%
  group_by(CircleID, family) %>%
  summarize(
    n_individuals = sum(Number, na.rm = T),
    biomass = sum(TotalMass))

groundFams <- sort(unique(ground_families$family))

# make a data frame with sites as columns and families as rows
family_circles_ground <- map_dfc(
  unique(ground_families$CircleID),
  ~ ground_families %>%
    # select families observed at a particular circle
    filter(
      CircleID == .x,
      !is.na(family)) %>%
    # add families that were not observed at a particular circle
    bind_rows(tibble(
      family = groundFams[!groundFams %in% .$family])) %>%
    mutate(
      # populate circle ID to non-observed family rows
      CircleID = if_else(
        is.na(CircleID),
        true = .x,
        false = CircleID),
      # fill in 0 values for non-observed family biomass
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      # take the log of biomass, adding 1/10th the smallest occurring value so zero observations are viable
      logBiomass = log(biomass+0.01)) %>%
    # select to relevant columns
    select(!n_individuals:biomass) %>%
    # pivot everything out
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    # transpose it to an uglier but technically better frame
    t() %>%
    # get the circle names out of the first row
    row_to_names(row_number = 1) %>%
    # make it a real boy
    as_tibble(rownames = 'family') %>%
    # put it in alphabetical order by family
    arrange(family) %>%
    # remove family
    select(!family)) %>%
  # put family back in at the end of the map function
  cbind(family = groundFams) %>%
  # put family at the front for my sanity
  relocate(family) %>%
  # duh
  as_tibble() %>%
  # why these didn't automatically come out numeric, god only knows
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))

# nested map function to apply euclidean function to every possible combination of circles, resulting in an identity matrix for euclidean distance between circle communities
euclidean_matrix_ground <- map_dfr(
  .x = 2:31,
  .f = function(s){
    map_dfc(
      .x = 2:31,
      .f = ~ euclidean(family_circles_ground[[s]], family_circles_ground[[.x]])
    )
  }) %>%
  # setting the column names
  set_names(names(family_circles_ground)[2:31]) %>%
  # setting the rows to mark for each circle
  cbind(circle = names(family_circles_ground)[2:31]) %>%
  # again, for my peace of mind
  relocate(circle)

# make a matrix like the euclidean one but for jaccard dissimilarity
jaccard_matrix_ground <- map_dfr(
  .x = circles$CircleID,
  .f = function(s){
    map_dfc(
      .x = circles$CircleID,
      .f = ~
        # the '1-' makes it dissimilarity instead of similarity
        1 - jaccard(
          # total number of families in both of two particular circles
          a = sum(ground_families$family[ground_families$CircleID == s] %in% ground_families$family[ground_families$CircleID == .x]),
          # total number of families in the first circle but not the second
          b = sum(!ground_families$family[ground_families$CircleID == s] %in% ground_families$family[ground_families$CircleID == .x]),
          # total number of families in the second circle but not the first
          c = sum(!ground_families$family[ground_families$CircleID == .x] %in% ground_families$family[ground_families$CircleID == s]))
    )
  }) %>%
  # I explained this part up there
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)



# creating a model-friendly data frame (ground) ---------------------------

analysis_frame_ground <- euclidean_matrix_ground %>%
  # shifts from a matrix to a frame defined by the first two columns - sorry, Hadley - corresponding to the euclidean distance between the two
  pivot_longer(
    cols = 2:length(.),
    names_to = 'circle2',
    values_to = 'euclideanDistance') %>%
  # remove rows for circles against themselves - not useful data
  filter(circle != circle2) %>%
  # take advantage of the lack of identical distances between any two site pairs to remove the duplicates resulting from flipping out a matrix
  distinct(euclideanDistance, .keep_all = T) %>%
  left_join(
    # pretty much the same thing
    jaccard_matrix_ground %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'jaccardDissimilarity'),
    by = c('circle','circle2')) %>%
  left_join(
    herbaceous_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'herbaceousCover'),
    by = c('circle','circle2')) %>%
  left_join(
    distance_road_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'distanceToRoad'),
    by = c('circle','circle2')) %>%
  left_join(
    distance_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'geographicDistance'),
    by = c('circle','circle2')) %>%
  left_join(
    litter_depth_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'litterDepth'),
    by = c('circle','circle2')) %>%
  rename('circle1' = 'circle') %>%
  left_join(
    forest_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = 'circle2',
        values_to = 'forest_1km'),
    by = c('circle1','circle2')) %>%
  mutate(
    # make a unique identifier (fine, Hadley)
    circles = str_c(circle1, circle2, sep = '_'),
    # convert distance to edge from m to km
    distanceToRoad = distanceToRoad/1000,
    # convert geographic distance from m to km
    geographicDistance = geographicDistance/1000) %>%
  select(!circle1:circle2) %>%
  relocate(circles)

write_csv(
  analysis_frame_ground,
  str_c('data/ground_dissimilarity_', today(), '.csv'))

analysis_frame_ground <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_dissimilarity')])


# initial modeling of ground arths ---------------------------------------

summary(lm(
  euclideanDistance ~ jaccardDissimilarity,
  data = analysis_frame_ground))

# p = 0.4939
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(herbaceous_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0001
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# p = 0.1835
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(distance_road_matrix[2:31]), 
  nrepet = 9999)

# p = 0.5913
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(litter_depth_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0006
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.5098
euclidean_ground_mod_full <- lm(
  euclideanDistance ~ distanceToRoad + geographicDistance + forest_1km,
  data = analysis_frame_ground)

# R^2 = 0.4956
euclidean_ground_mod_env <- lm(
  euclideanDistance ~ distanceToRoad + forest_1km,
  data = analysis_frame_ground)

# R^2 = 0.008
euclidean_ground_mod_road <- lm(
  euclideanDistance ~ distanceToRoad,
  data = analysis_frame_ground)

# R^2 = 0.494
euclidean_ground_mod_forest <- lm(
  euclideanDistance ~ forest_1km,
  data = analysis_frame_ground)

# R^2 = 0.094
euclidean_ground_mod_dist <- lm(
  euclideanDistance ~ geographicDistance,
  data = analysis_frame_ground)

# p = 0.2433
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(herbaceous_matrix[2:31]), 
  nrepet = 9999)

# p = 0.2940
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(distance_road_matrix[2:31]), 
  nrepet = 9999)

# p = 0.2798
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(litter_depth_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0001
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# p = 0.0564
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.2613
jaccard_ground_mod_full <- lm(
  jaccardDissimilarity ~ litterDepth + distanceToRoad + geographicDistance + forest_1km,
  data = analysis_frame_ground)

# R^2 = 0.2463
jaccard_ground_mod_env <- lm(
  jaccardDissimilarity ~ distanceToRoad + forest_1km + litterDepth,
  data = analysis_frame_ground)

# R^2 = 0.003
jaccard_ground_mod_road <- lm(
  jaccardDissimilarity ~ distanceToRoad,
  data = analysis_frame_ground)

# R^2 = 0.245
jaccard_ground_mod_forest <- lm(
  jaccardDissimilarity ~ forest_1km,
  data = analysis_frame_ground)

# R^2 = 0.091
jaccard_ground_mod_litter <- lm(
  jaccardDissimilarity ~ litterDepth,
  data = analysis_frame_ground)

# R^2 = 0.035
jaccard_ground_mod_dist <- lm(
  jaccardDissimilarity ~ geographicDistance,
  data = analysis_frame_ground)

