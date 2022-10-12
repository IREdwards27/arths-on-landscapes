
# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(geosphere)

# # read in foliage and ground arthropod observations - code format pulls most recent date
# 
# foliage_arths <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])
# 
# ground_arths <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^groundarths')])
# 
# # read in taxa
# 
# taxa <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])
# 
# # read in beat sheets
# 
# beatsheets <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^beatsheets')])
# 
# # read in pitfall traps
# 
# pitfalls <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^pitfallsurveys')])
# 
# # read in trees
# 
# trees <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])
# 
# # read in circles
# 
# circles <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^circles')])
# 
# sites <- read_csv(
#   list.files('data', full.names = T)[str_detect(list.files('data'), '^sites')])


# calculating community dissimilarity metrics (foliage) -------------------------

# foliage_families <- foliage_arths %>% 
#   # filter to confident IDs while still working on bug ID
#   mutate(Taxon = case_when(
#     Taxon == 'Trogossitidae' ~ 'Nitidulidae',
#     Taxon %in% c('Ponera','Hypoponera') ~ 'Brachyponera chinensis')) %>% 
#   left_join(
#     taxa,
#     by = 'TaxonID') %>% 
#   # filter to focal orders
#   filter(
#     order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>% 
#   left_join(
#     beatsheets %>% 
#       select(BeatSheetID, Date, TreeFK),
#     by = c('BeatSheetFK' = 'BeatSheetID')) %>% 
#   # filter to first two rounds of data collection while finishing bug ID
#   filter(as.Date(Date, format = '%m/%d/%Y') < as.Date('2022-06-26')) %>% 
#   left_join(
#     trees %>% 
#       select(TreeID, CircleFK),
#     by = c('TreeFK' = 'TreeID')) %>% 
#   left_join(
#     circles,
#     by = c('CircleFK' = 'CircleID')) %>% 
#   group_by(CircleFK, family) %>%
#   summarize(
#     n_individuals = sum(Quantity, na.rm = T),
#     biomass = sum(TotalMass))
# 
# foliageFams <- sort(unique(foliage_families$family))
# 
# # make a data frame with sites as columns and families as rows
# family_circles <- map_dfc(
#   unique(foliage_families$CircleFK),
#   ~ foliage_families %>% 
#     # select families observed at a particular circle
#     filter(
#       CircleFK == .x,
#       !is.na(family)) %>% 
#     # add families that were not observed at a particular circle
#     bind_rows(tibble(
#       family = foliageFams[!foliageFams %in% .$family])) %>% 
#     mutate(
#       # populate circle ID to non-observed family rows
#       CircleFK = if_else(
#         is.na(CircleFK),
#         true = .x,
#         false = CircleFK),
#       # fill in 0 values for non-observed family biomass
#       biomass = if_else(
#         is.na(biomass),
#         true = 0,
#         false = biomass),
#       # take the log of biomass, adding one so zero observations results in value of zero
#       # consider altering c value to <1/2 smallest value
#       logBiomass = log(biomass+1)) %>% 
#     # select to relevant columns
#     select(!n_individuals:biomass) %>% 
#     # pivot everything out
#     pivot_wider(
#       names_from = family,
#       values_from = logBiomass) %>% 
#     # transpose it to an uglier but technically better frame
#     t() %>% 
#     # get the circle names out of the first row
#     row_to_names(row_number = 1) %>% 
#     # make it a real boy
#     as_tibble(rownames = 'family') %>% 
#     # put it in alphabetical order by family
#     arrange(family) %>% 
#     # remove family
#     select(!family)) %>% 
#   # put family back in at the end of the map function
#   cbind(family = foliageFams) %>% 
#   # put family at the front for my sanity
#   relocate(family) %>% 
#   # duh
#   as_tibble() %>% 
#   # why these didn't automatically come out numeric, god only knows
#   mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))
# 
# # gonna be real, found this on the internet, but it's simple and correct
# euclidean <- function(a, b) sqrt(sum((a - b)^2))
# 
# # nested map function to apply euclidean function to every possible combination of circles, resulting in an identity matrix for euclidean distance between circle communities
# euclidean_matrix <- map_dfr(
#   .x = 2:31,
#   .f = function(s){
#     map_dfc(
#       .x = 2:31,
#       .f = ~ euclidean(family_circles[[s]], family_circles[[.x]])
#       )
#   }) %>% 
#   # setting the column names
#   set_names(names(family_circles)[2:31]) %>% 
#   # setting the rows to mark for each circle
#   cbind(circle = names(family_circles)[2:31]) %>% 
#   # again, for my peace of mind
#   relocate(circle)
# 
# # I came up with this one, because I'm a genius
# jaccard <- function(a,b,c) {a/(a+b+c)}
# 
# # make a matrix like the euclidean one but for jaccard dissimilarity
# jaccard_matrix <- map_dfr(
#   .x = circles$CircleID,
#   .f = function(s){
#     map_dfc(
#       .x = circles$CircleID,
#       .f = ~
#         # the '1-' makes it dissimilarity instead of similarity
#         1 - jaccard(
#           # total number of families in both of two particular circles
#           a = sum(foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
#           # total number of families in the first circle but not the second
#           b = sum(!foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
#           # total number of families in the second circle but not the first
#           c = sum(!foliage_families$family[foliage_families$CircleFK == .x] %in% foliage_families$family[foliage_families$CircleFK == s]))
#     )
#   }) %>% 
#   # I explained this part up there
#   set_names(circles$CircleID) %>% 
#   cbind(circle = circles$CircleID) %>% 
#   relocate(circle)


# calculating distance metrics (foliage) ------------------------------------

# # make a matrix for difference in percent canopy cover between circles
# canopy_cover_matrix <- map_dfr(
#   .x = 1:30,
#   .f = function(s){
#     map_dfc(
#       .x = 1:30,
#       # I never thought I'd be so grateful for subtraction
#       .f = ~ abs(circles$PercentCanopyCover[s] - circles$PercentCanopyCover[.x]))
#   }) %>% 
#   set_names(circles$CircleID) %>% 
#   cbind(circle = circles$CircleID) %>% 
#   relocate(circle)
# 
# # these ones all work the same - this one is for distance to edge
# distance_edge_matrix <- map_dfr(
#   .x = 1:30,
#   .f = function(s){
#     map_dfc(
#       .x = 1:30,
#       .f = ~ abs(circles$DistanceToEdgem[s] - circles$DistanceToEdgem[.x]))
#   }) %>% 
#   set_names(circles$CircleID) %>% 
#   cbind(circle = circles$CircleID) %>% 
#   relocate(circle)
# 
# # this one's a little different because it uses the geosphere package, which has it's own "give me a matrix" function, so I just had to adjust format
# distance_matrix <- circles %>% 
#   select(Longitude, Latitude) %>% 
#   distm() %>% 
#   as_tibble() %>% 
#   set_names(circles$CircleID) %>% 
#   cbind(circle = circles$CircleID) %>% 
#   relocate(circle)
# 
# # this one only differs by site, so it's even simpler - difference in percent forest cover within a 1km radius
# forest_matrix <- map_dfr(
#   .x = 1:6,
#   .f = function(s){
#     map_dfc(
#       .x = 1:6,
#       .f = ~ abs(sites$forest_1km[s] - sites$forest_1km[.x]))
#   }) %>% 
#   set_names(sites$SiteID) %>% 
#   cbind(site1 = sites$SiteID) %>% 
#   relocate(site1)


# creating a model-friendly data frame (foliage) ----------------------------

# analysis_frame_foliage <- euclidean_matrix %>% 
#   # shifts from a matrix to a frame defined by the first two columns - sorry, Hadley - corresponding to the euclidean distance between the two
#   pivot_longer(
#     cols = 2:length(.),
#     names_to = 'circle2',
#     values_to = 'euclideanDistance') %>% 
#   # remove rows for circles against themselves - not useful data
#   filter(circle != circle2) %>% 
#   # take advantage of the lack of identical distances between any two site pairs to remove the duplicates resulting from flipping out a matrix
#   distinct(euclideanDistance, .keep_all = T) %>% 
#   left_join(
#     # pretty much the same thing
#     jaccard_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'jaccardDissimilarity'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     canopy_cover_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'canopyCover'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     distance_edge_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'distanceToEdge'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     distance_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'geographicDistance'),
#     by = c('circle','circle2')) %>% 
#   rename('circle1' = 'circle') %>% 
#   # adding site IDs to bind forest cover data
#   mutate(
#     site1 = case_when(
#       str_detect(circle1, 'DF') ~ 'DF',
#       str_detect(circle1, 'ERSP') ~ 'ERSP',
#       str_detect(circle1, 'JMNP') ~ 'JMNP',
#       str_detect(circle1, 'NCBG') ~ 'NCBG',
#       str_detect(circle1, 'NCSU') ~ 'NCSU',
#       str_detect(circle1, 'UNC') ~ 'UNC'),
#     site2 = case_when(
#       str_detect(circle2, 'DF') ~ 'DF',
#       str_detect(circle2, 'ERSP') ~ 'ERSP',
#       str_detect(circle2, 'JMNP') ~ 'JMNP',
#       str_detect(circle2, 'NCBG') ~ 'NCBG',
#       str_detect(circle2, 'NCSU') ~ 'NCSU',
#       str_detect(circle2, 'UNC') ~ 'UNC')) %>% 
#   left_join(
#     forest_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'site2',
#         values_to = 'forest_1km'),
#     by = c('site1','site2')) %>% 
#   mutate(
#     # make a unique identifier (fine, Hadley)
#     circles = str_c(circle1, circle2, sep = '_'),
#     # convert canopy cover from percent to proportion
#     canopyCover = canopyCover/100,
#     # convert distance to edge from m to km
#     distanceToEdge = distanceToEdge/1000,
#     # convert geographic distance from m to km
#     geographicDistance = geographicDistance/1000) %>% 
#   select(!c(circle1:circle2, site1:site2)) %>% 
#   relocate(circles)
# 
# write_csv(
#   analysis_frame_foliage,
#   str_c('data/foliage_dissimilarity_', today(), '.csv'))

analysis_frame_foliage <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_dissimilarity')])

# visualizing foliage arthropod stats -------------------------------------

ggplot(analysis_frame_foliage) +
  geom_point(aes(
    x = jaccardDissimilarity,
    y = euclideanDistance))

# jitter may be helpful for forest cover for visualization
map(
  analysis_frame_foliage[,2:3],
  function(r){
    map(
      analysis_frame_foliage[,4:7],
      function(p){ ggplot(analysis_frame_foliage) +
          geom_point(aes(
            x = p,
            y = r))
      })
  })


# initial modeling of foliage arths ---------------------------------------

summary(lm(
  euclideanDistance ~ jaccardDissimilarity,
  data = analysis_frame_foliage))

euclidean_foliage_mod_full <- lm(
  euclideanDistance ~ canopyCover + distanceToEdge + geographicDistance + forest_1km,
  data = analysis_frame_foliage)

euclidean_foliage_mod_env <- lm(
  euclideanDistance ~ canopyCover + distanceToEdge + forest_1km,
  data = analysis_frame_foliage)

euclidean_foliage_mod_dist <- lm(
  euclideanDistance ~ geographicDistance,
  data = analysis_frame_foliage)

jaccard_foliage_mod_full <- lm(
  jaccardDissimilarity ~ canopyCover + distanceToEdge + geographicDistance + forest_1km,
  data = analysis_frame_foliage)

jaccard_foliage_mod_env <- lm(
  jaccardDissimilarity ~ canopyCover + distanceToEdge + forest_1km,
  data = analysis_frame_foliage)

jaccard_foliage_mod_dist <- lm(
  jaccardDissimilarity ~ geographicDistance,
  data = analysis_frame_foliage)


# calculating environmental distance metrics (ground) ---------------------

# herbaceous_matrix <- map_dfr(
#   .x = 1:30,
#   .f = function(s){
#     map_dfc(
#       .x = 1:30,
#       .f = ~ abs(circles$HerbCoverEstimate[s] - circles$HerbCoverEstimate[.x]))
#   }) %>% 
#   set_names(circles$CircleID) %>% 
#   cbind(circle = circles$CircleID) %>% 
#   relocate(circle)
#   
# litter_depth_matrix <- map_dfr(
#   .x = 1:30,
#   .f = function(s){
#     map_dfc(
#       .x = 1:30,
#       .f = ~ abs(circles$LitterDepthmm[s] - circles$LitterDepthmm[.x]))
#   }) %>% 
#   set_names(circles$CircleID) %>% 
#   cbind(circle = circles$CircleID) %>% 
#   relocate(circle)


# creating a model-friendly data frame (ground) ---------------------------

# analysis_frame_ground <- euclidean_matrix %>% 
#   # shifts from a matrix to a frame defined by the first two columns - sorry, Hadley - corresponding to the euclidean distance between the two
#   pivot_longer(
#     cols = 2:length(.),
#     names_to = 'circle2',
#     values_to = 'euclideanDistance') %>% 
#   # remove rows for circles against themselves - not useful data
#   filter(circle != circle2) %>% 
#   # take advantage of the lack of identical distances between any two site pairs to remove the duplicates resulting from flipping out a matrix
#   distinct(euclideanDistance, .keep_all = T) %>% 
#   left_join(
#     # pretty much the same thing
#     jaccard_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'jaccardDissimilarity'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     herbaceous_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'herbaceousCover'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     distance_edge_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'distanceToEdge'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     distance_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'geographicDistance'),
#     by = c('circle','circle2')) %>% 
#   left_join(
#     litter_depth_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'circle2',
#         values_to = 'litterDepth'),
#     by = c('circle','circle2')) %>% 
#   rename('circle1' = 'circle') %>% 
#   # adding site IDs to bind forest cover data
#   mutate(
#     site1 = case_when(
#       str_detect(circle1, 'DF') ~ 'DF',
#       str_detect(circle1, 'ERSP') ~ 'ERSP',
#       str_detect(circle1, 'JMNP') ~ 'JMNP',
#       str_detect(circle1, 'NCBG') ~ 'NCBG',
#       str_detect(circle1, 'NCSU') ~ 'NCSU',
#       str_detect(circle1, 'UNC') ~ 'UNC'),
#     site2 = case_when(
#       str_detect(circle2, 'DF') ~ 'DF',
#       str_detect(circle2, 'ERSP') ~ 'ERSP',
#       str_detect(circle2, 'JMNP') ~ 'JMNP',
#       str_detect(circle2, 'NCBG') ~ 'NCBG',
#       str_detect(circle2, 'NCSU') ~ 'NCSU',
#       str_detect(circle2, 'UNC') ~ 'UNC')) %>% 
#   left_join(
#     forest_matrix %>% 
#       pivot_longer(
#         cols = 2:length(.),
#         names_to = 'site2',
#         values_to = 'forest_1km'),
#     by = c('site1','site2')) %>% 
#   mutate(
#     # make a unique identifier (fine, Hadley)
#     circles = str_c(circle1, circle2, sep = '_'),
#     # convert distance to edge from m to km
#     distanceToEdge = distanceToEdge/1000,
#     # convert geographic distance from m to km
#     geographicDistance = geographicDistance/1000) %>% 
#   select(!c(circle1:circle2, site1:site2)) %>% 
#   relocate(circles)
# 
# write_csv(
#   analysis_frame_ground,
#   str_c('data/ground_dissimilarity_', today(), '.csv'))

analysis_frame_ground <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_dissimilarity')])

# visualizing ground arthropod stats -------------------------------------

ggplot(analysis_frame_ground) +
  geom_point(aes(
    x = jaccardDissimilarity,
    y = euclideanDistance))

map(
  analysis_frame_ground[,2:3],
  function(r){
    map(
      analysis_frame_ground[,4:8],
      function(p){ ggplot(analysis_frame_ground) +
          geom_point(aes(
            x = p,
            y = r))
      })
  })


# initial modeling of ground arths ---------------------------------------

summary(lm(
  euclideanDistance ~ jaccardDissimilarity,
  data = analysis_frame_ground))

summary(lm(
  euclideanDistance ~ herbaceousCover + distanceToEdge + geographicDistance + litterDepth + forest_1km,
  data = analysis_frame_ground))

summary(lm(
  jaccardDissimilarity ~ herbaceousCover + distanceToEdge + geographicDistance + litterDepth + forest_1km,
  data = analysis_frame_ground))
