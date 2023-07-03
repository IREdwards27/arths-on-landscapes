
### preparing data ----------------------------------------------------------


## setup -------------------------------------------------------------------

# install necessary packages, listed below, before proceeding
# load necessary packages
library(tidyverse)
library(ggpubr)
library(ggfortify)
library(janitor)
library(ade4)
library(raster)
library(rgdal)
library(ggnewscale)
library(corrplot)
library(sf)

# read in data on observations of foliage arthropods
foliage_arths <- read_csv(
  # identifies the file by name, without specifying date tag - only the most recent version of a table should be saved at any time
  list.files("data", full.names = T)[str_detect(list.files("data"), "^foliage_arths")])

# read in data on observations of ground arthropods
ground_arths <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^ground_arths")])

# read in list of observed taxa
taxa <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^taxa")])

# read in data on beat sheet surveys
beatsheets <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^beat_sheets")])

# read in data on pitfall trap runs
pitfalls <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^pitfalls")])

# read in data on survey trees
trees <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^trees")])

# read in data on sampling plots - note that plots are referred to as both circles and sampling plots throughout this script, and these two terms refer to the same thing
circles <- read_csv("data/circles_2022-10-03.csv")

# read in data on sampling sites
sites <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^sites")])

# read in the raster file of land cover classes in the study area
nlcd <- raster("data/nlcd_local") %>% 
  # the raster seems to have picked up a weird extra chunk of extent on the right edge - cropping it out
  crop(extent(c(1493025,1551500,1541175,1598865)))

# convert the circle coordinates to a spatial object
circles_sf <- 
  circles %>% 
  st_as_sf(
    coords = c('Longitude', 'Latitude'),
    crs = 4326) %>% 
  st_transform(crs = raster::crs(nlcd))

# make a reclass raster for forest classes = 1, others = 0
forest_rcl <- 
  matrix(
    data = c(
      0,40,0,
      40,43,1,
      43,100,0),
    ncol = 3,
    byrow = T)

# reclassify the NLCD raster as above
nlcd_forest <- raster::reclassify(
  x = nlcd,
  rcl = forest_rcl)

# calculate the proportion forest cover within 1km of each circle and join it to the circle data - note that this names over the original circles object
circles <-  raster::extract(
  x = nlcd_forest,
  y = circles_sf,
  buffer = 1000,
  fun = mean,
  na.rm = T,
  df = T) %>% 
  rename(
    forest_1km = nlcd_local) %>% 
  cbind(circles) %>% 
  relocate(
    forest_1km,
    .after = DistanceToEdgem) %>% 
  dplyr::select(!ID)

## calculating community dissimilarity metrics -----------------------------

# create a function to calculate Euclidean distance - takes two vectors of abundance values, a and b, as inputs
euclidean <- function(a, b) sqrt(sum((a - b)^2))

# create a function to calculate Jaccard similarity - takes three vectors of presence/absence values as inputs, where a is the vector of families found at both sampling plots and b and c are the vectors of families found at one sampling plot but not the other
jaccard <- function(a,b,c) {a/(a+b+c)}

# foliage arthropod communities -------------------------------------------

# create a tibble with the number of individuals and total biomass of each family observed at each sampling plot
foliage_families <- foliage_arths %>%
  left_join(
    taxa,
    by = "TaxonID") %>%
  # filter to orders included in analysis
  filter(
    order %in% c("Araneae","Coleoptera","Hemiptera","Opiliones","Orthoptera") | (order == "Hymenoptera" & family == "Formicidae")) %>%
  # join survey data
  left_join(
    beatsheets %>%
      dplyr::select(BeatSheetID, Date, TreeFK),
    by = c("BeatSheetFK" = "BeatSheetID")) %>%
  # join survey tree data
  left_join(
    trees %>%
      dplyr::select(TreeID, CircleFK),
    by = c("TreeFK" = "TreeID")) %>%
  # join sampling plot data
  left_join(
    circles,
    by = c("CircleFK" = "CircleID")) %>%
  # calculate the total number of individuals and total biomass of arthropods in each family observed across all beat sheet surveys at each sampling plot
  group_by(CircleFK, family) %>%
  summarize(
    n_individuals = sum(Quantity, na.rm = T),
    biomass = sum(TotalMass, na.rm = T))

# create an alphabetized vector of all the families observed on any beat sheet survey
foliageFams <- sort(unique(foliage_families$family))

# make a data frame with sites as columns and families as rows, where each cell contains the log-transformed biomass of a family at a sampling plot
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
      # populate circle ID to rows for families not observed at a particular circle
      CircleFK = if_else(
        is.na(CircleFK),
        true = .x,
        false = CircleFK),
      # fill in 0 values for the biomass of families not observed at a particular circle
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      # take the log of biomass, adding 1/10th of the smallest value so zero observations can be included
      logBiomass = log(biomass+0.01)) %>%
    # select to relevant columns
    dplyr::select(!n_individuals:biomass) %>%
    # pivot out so each sampling plot is a column
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    # transpose the frame
    t() %>%
    # get the circle names out of the first row
    row_to_names(row_number = 1) %>%
    # make it a real boy (convert it to a tibble)
    as_tibble(rownames = "family") %>%
    # put it in alphabetical order by family
    arrange(family) %>%
    # remove family
    dplyr::select(!family)) %>%
  # put family back in at the end of the map function
  cbind(family = foliageFams) %>%
  # put family at the front for my sanity
  relocate(family) %>%
  # re-convert to a tibble
  as_tibble() %>%
  # convert contents to numeric
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))

# nested map function to calculate Euclidean distance between every possible combination of circles, resulting in an identity matrix for Euclidean distance between circle communities
euclidean_matrix_foliage <- map_dfr(
  .x = 2:31,
  .f = function(s){
    map_dfc(
      .x = 2:31,
      .f = ~ euclidean(family_circles_foliage[[s]], family_circles_foliage[[.x]])
    )
  }) %>%
  # set the column names
  set_names(names(family_circles_foliage)[2:31]) %>%
  # set the row names
  cbind(circle = names(family_circles_foliage)[2:31]) %>%
  # move the sampling plot ID to the first column
  relocate(circle)

# nested map function to calculate Jaccard dissimilarity between every possible combination of circles, resulting in an identity matrix for Jaccard dissimilarity between circle communities
jaccard_matrix_foliage <- map_dfr(
  .x = circles$CircleID,
  .f = function(s){
    map_dfc(
      .x = circles$CircleID,
      .f = ~
        # the '1-' changes the calculation to a dissimilarity metric
        1 - jaccard(
          # total number of families in both of two particular circles
          a = sum(foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
          # total number of families in the first circle but not the second
          b = sum(!foliage_families$family[foliage_families$CircleFK == s] %in% foliage_families$family[foliage_families$CircleFK == .x]),
          # total number of families in the second circle but not the first
          c = sum(!foliage_families$family[foliage_families$CircleFK == .x] %in% foliage_families$family[foliage_families$CircleFK == s]))
    )
  }) %>%
  # organizational processes as above
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)


# ground arthropod communities -------------------------------------------------------

# create a tibble with the number of individuals and total biomass of each family observed at each sampling plot
ground_families <- ground_arths %>%
  # join in taxon information
  left_join(
    taxa,
    by = "TaxonID") %>%
  # filter to orders included for analysis
  filter(
    order %in% c("Araneae","Archaeognatha","Coleoptera","Isopoda", "Opiliones", "Orthoptera") | (order == "Hymenoptera" & family == "Formicidae")) %>%
  # join in information on pitfall runs
  left_join(
    pitfalls %>%
      dplyr::select(PitfallID, DateCollected, CircleID),
    by = "PitfallID") %>%
  # join in information on sampling plots
  left_join(
    circles,
    by = "CircleID") %>%
  group_by(CircleID, family) %>%
  # calculate the total number of individuals and total biomass of arthropods in each family observed across all pitfall trap runs at each sampling plot
  summarize(
    n_individuals = sum(Number, na.rm = T),
    biomass = sum(TotalMass, na.rm = T))

# create an alphabetized vector of all the families observed Sin any pitfall trap
groundFams <- sort(unique(ground_families$family))

# make a data frame with sites as columns and families as rows
family_circles_ground <- map_dfc(
  unique(ground_families$CircleID),
  ~ ground_families %>%
    # select families observed at a particular sampling plot
    filter(
      CircleID == .x,
      !is.na(family)) %>%
    # add families that were not observed at a particular plot
    bind_rows(tibble(
      family = groundFams[!groundFams %in% .$family])) %>%
    mutate(
      # populate sampling plot ID to non-observed family rows
      CircleID = if_else(
        is.na(CircleID),
        true = .x,
        false = CircleID),
      # fill in 0 values for biomass of families not observed
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      # take the log of biomass, adding 1/10th the smallest occurring value so zero observations are viable
      logBiomass = log(biomass+0.01)) %>%
    # select to relevant columns
    dplyr::select(!n_individuals:biomass) %>%
    # pivot everything out
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    # transpose
    t() %>%
    # get the sampling plot IDs out of the first row
    row_to_names(row_number = 1) %>%
    # make it a real boy
    as_tibble(rownames = "family") %>%
    # put it in alphabetical order by family
    arrange(family) %>%
    # remove family
    dplyr::select(!family)) %>%
  # put family back in at the end of the map function
  cbind(family = groundFams) %>%
  relocate(family) %>%
  as_tibble() %>%
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))

# nested map function to calculate Euclidean distance between every possible combination of sampling plots, resulting in an identity matrix for Euclidean distance between plot communities
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
  relocate(circle)

# make a matrix as above for Jaccard similarity
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
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)


## calculating environmental distance metrics ------------------------------


# tree species dissimilarity between sampling plots -----------------------

# generate a data frame with a row for each tree species found at each sampling plot
tree_species <- trees %>% 
  dplyr::select(CircleFK, Species) %>% 
  distinct()

# use the format for calculating arthropod community Jaccard dissimilarity to calculate sample tree species Jaccard dissimilarity
trees_jaccard_matrix <- map_dfr(
  .x = circles$CircleID,
  .f = function(s){
    map_dfc(
      .x = circles$CircleID,
      .f = ~
        1 - jaccard(
          a = sum(tree_species$Species[tree_species$CircleFK == s] %in% tree_species$Species[tree_species$CircleFK == .x]),
          b = sum(!tree_species$Species[tree_species$CircleFK == s] %in% tree_species$Species[tree_species$CircleFK == .x]),
          c = sum(!tree_species$Species[tree_species$CircleFK == .x] %in% tree_species$Species[tree_species$CircleFK == s]))
    )
  }) %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)


# proportion canopy cover difference between sampling plots ------------------

canopy_cover_matrix <- dist(circles$PercentCanopyCover, diag = T, upper = T) %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)


# difference in distance to a road between sampling plots -----------------

distance_road_matrix <- dist(circles$DistanceToEdgem, diag = T, upper = T) %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

# difference in herbaceous cover class ------------------------------------

herbaceous_matrix <- dist(circles$HerbCoverEstimate, diag = T, upper = T) %>% 
  as.matrix() %>% 
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

# difference in litter depth ----------------------------------------------

litter_depth_matrix <- dist(circles$LitterDepthmm, diag = T, upper = T) %>% 
  as.matrix() %>% 
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

# difference in proportion forest cover in a 1-km radius ---------------------

# calculate the differences in forest cover as shown for other metrics
forest_matrix <- dist(circles$forest_1km, diag = T, upper = T) %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle1 = circles$CircleID) %>%
  relocate(circle1)


## calculating geographic and resistance path distance between plots --------

 # geographic distance
distance_matrix <- circles %>%
  dplyr::select(Longitude, Latitude) %>%
  # the distm function from the geosphere package is substituted for the dist function for simpler calculations using latitude and longitude
  geosphere::distm() %>%
  as_tibble() %>%
  set_names(circles$CircleID) %>%
  cbind(circle = circles$CircleID) %>%
  relocate(circle)

# resistance path distances

# create a reclass matrix for NLCD land cover data based on the criteria listed below
# water, barren, all developed except open space assigned 10, all non-forest plant cover (including agriculture and developed open space) assigned 5, all forest assigned 1
mod1 <- tibble(
  from = unique(nlcd),
  to = c(10,5,rep(10,4),rep(1,3),rep(5,6)))

nlcd_mod1 <- reclassify(x = nlcd, rcl = mod1)

writeRaster(nlcd_mod1, filename = "data/mod1", format = "ascii", overwrite = T)

# the intervening step requires using the CircuitScape GUI to run the calculations for the resistances of shortest paths between sampling plots, using the raster created above and the "circles.txt" file. The following line reads in the output from this operation as a 3-column dataframe

paths <- read_table(
  file = "data/mod1_paths_resistances_3columns",
  col_names = F) %>% 
  # name the output columns - the first two are node IDs derived from sampling plot IDs in order to be compatible with CircuitScape
  rename("nodeID_1" = X1, "nodeID_2" = X2, "resistance" = X3) %>% 
  # re-derive sampling plot IDs from node IDs - node IDs swapped numbers 1-6 for the site codes, in alphabetical order
  mutate(
    circle1 = str_replace(nodeID_1, "^1", "DF") %>% 
      str_replace("^2", "ERSP") %>% 
      str_replace("^3", "JMNP") %>% 
      str_replace("^4", "NCBG") %>% 
      str_replace("^5", "NCSU") %>% 
      str_replace("^6", "UNC"),
    circle2 = str_replace(nodeID_2, "^1", "DF") %>% 
      str_replace("^2", "ERSP") %>% 
      str_replace("^3", "JMNP") %>% 
      str_replace("^4", "NCBG") %>% 
      str_replace("^5", "NCSU") %>% 
      str_replace("^6", "UNC")) %>% 
    dplyr::select(!c(nodeID_1, nodeID_2)) %>% 
  rbind(
    tibble(
      resistance = rep(0, length(circles$CircleID)),
    circle1 = circles$CircleID,
    circle2 = circles$CircleID)) %>% 
  arrange(circle1, circle2)

# the next three code blocks are just the colossal nightmare involved in creating a complete identity matrix for the resistance between pairs of sampling sites where the orders of circle1 and circle2 aren't consistent with existing matrices
paths_matrix1 <- paths %>% 
  pivot_wider(
    names_from = circle2,
    values_from = resistance) %>% 
  rename_with(
    .fn = ~str_c(.,"_1"))

paths_matrix2 <- paths %>% 
  pivot_wider(
    names_from = circle1,
    values_from = resistance) %>% 
  rename_with(
    .fn = ~str_c(.,"_2"))

paths_matrix <- cbind(paths_matrix1, paths_matrix2) %>% 
  mutate(
    DF1 = if_else(
      is.na(DF1_1),
      true = DF1_2,
      false = DF1_1),
    DF2 = if_else(
      is.na(DF2_1),
      true = DF2_2,
      false = DF2_1),
    DF3 = if_else(
      is.na(DF3_1),
      true = DF3_2,
      false = DF3_1),
    DF4 = if_else(
      is.na(DF4_1),
      true = DF4_2,
      false = DF4_1),
    DF5 = if_else(
      is.na(DF5_1),
      true = DF5_2,
      false = DF5_1),
    ERSP1 = if_else(
      is.na(ERSP1_1),
      true = ERSP1_2,
      false = ERSP1_1),
    ERSP2 = if_else(
      is.na(ERSP2_1),
      true = ERSP2_2,
      false = ERSP2_1),
    ERSP3 = if_else(
      is.na(ERSP3_1),
      true = ERSP3_2,
      false = ERSP3_1),
    ERSP4 = if_else(
      is.na(ERSP4_1),
      true = ERSP4_2,
      false = ERSP4_1),
    ERSP6 = if_else(
      is.na(ERSP6_1),
      true = ERSP6_2,
      false = ERSP6_1),
    JMNP1 = if_else(
      is.na(JMNP1_1),
      true = JMNP1_2,
      false = JMNP1_1),
    JMNP2 = if_else(
      is.na(JMNP2_1),
      true = JMNP2_2,
      false = JMNP2_1),
    JMNP3 = if_else(
      is.na(JMNP3_1),
      true = JMNP3_2,
      false = JMNP3_1),
    JMNP5 = if_else(
      is.na(JMNP5_1),
      true = JMNP5_2,
      false = JMNP5_1),
    JMNP8 = if_else(
      is.na(JMNP8_1),
      true = JMNP8_2,
      false = JMNP8_1),
    NCBG1 = if_else(
      is.na(NCBG1_1),
      true = NCBG1_2,
      false = NCBG1_1),
    NCBG4 = if_else(
      is.na(NCBG4_1),
      true = NCBG4_2,
      false = NCBG4_1),
    NCBG5 = if_else(
      is.na(NCBG5_1),
      true = NCBG5_2,
      false = NCBG5_1),
    NCBG6 = if_else(
      is.na(NCBG6_1),
      true = NCBG6_2,
      false = NCBG6_1),
    NCBG8 = if_else(
      is.na(NCBG8_1),
      true = NCBG8_2,
      false = NCBG8_1),
    NCSU10 = if_else(
      is.na(NCSU10_1),
      true = NCSU10_2,
      false = NCSU10_1),
    NCSU11 = if_else(
      is.na(NCSU11_1),
      true = NCSU11_2,
      false = NCSU11_1),
    NCSU2 = if_else(
      is.na(NCSU2_1),
      true = NCSU2_2,
      false = NCSU2_1),
    NCSU3 = if_else(
      is.na(NCSU3_1),
      true = NCSU3_2,
      false = NCSU3_1),
    NCSU9 = if_else(
      is.na(NCSU9_1),
      true = NCSU9_2,
      false = NCSU9_1),
    UNC13 = if_else(
      is.na(UNC13_1),
      true = UNC13_2,
      false = UNC13_1),
    UNC14 = if_else(
      is.na(UNC14_1),
      true = UNC14_2,
      false = UNC14_1),
    UNC3 = if_else(
      is.na(UNC3_1),
      true = UNC3_2,
      false = UNC3_1),
    UNC4 = if_else(
      is.na(UNC4_1),
      true = UNC4_2,
      false = UNC4_1),
    UNC8 = if_else(
      is.na(UNC8_1),
      true = UNC8_2,
      false = UNC8_1),
    circle1 = circle1_1) %>% 
  dplyr::select(63:93) %>% 
  relocate(circle1)

# note that this names over the original paths frame
paths <- paths_matrix %>% 
  pivot_longer(
    cols = 2:31,
    names_to = "circle2",
    values_to = "resistance") %>% 
  distinct()


## making model-friendly data frames ----------------------------------------


# foliage arthropods ------------------------------------------------------

analysis_frame_foliage <- euclidean_matrix_foliage %>%
  # shifts from a matrix to a frame defined by the first two columns - sorry, Hadley - corresponding to the Euclidean distance between the two
  pivot_longer(
    cols = 2:length(.),
    names_to = "circle2",
    values_to = "euclideanDistance") %>%
  # remove rows for sampling plots against against themselves
  filter(circle != circle2) %>%
  # take advantage of the lack of identical distances between any two site pairs to remove the duplicates resulting from flipping out a matrix
  distinct(euclideanDistance, .keep_all = T) %>%
  # join in other dissimilarity or distance metrics
  left_join(
    # each frame is processed the same way as shown above for Euclidean distance
    jaccard_matrix_foliage %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "jaccardDissimilarity"),
    by = c("circle","circle2")) %>%
  left_join(
    trees_jaccard_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "treeDissimilarity"),
    by = c("circle","circle2")) %>%
  left_join(
    canopy_cover_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "canopyCover"),
    by = c("circle","circle2")) %>%
  left_join(
    distance_road_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "distanceToRoad"),
    by = c("circle","circle2")) %>%
  left_join(
    distance_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "geographicDistance"),
    by = c("circle","circle2")) %>%
  rename("circle1" = "circle") %>%
  left_join(
    forest_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "forest_1km"),
    by = c("circle1","circle2")) %>%
  left_join(
    paths,
    by = c("circle1", "circle2")) %>% 
  mutate(
    # make a unique identifier (fine, Hadley)
    circles = str_c(circle1, circle2, sep = "_"),
    # convert canopy cover from percent to proportion
    canopyCover = canopyCover/100,
    # convert distance to edge from m to km
    distanceToRoad = distanceToRoad/1000,
    # convert geographic distance from m to km
    geographicDistance = geographicDistance/1000) %>%
  # remove the old identifier columns
  dplyr::select(!circle1:circle2) %>%
  # place the new identifier column first
  relocate(circles)


# ground arthropods -------------------------------------------------------

analysis_frame_ground <- euclidean_matrix_ground %>%
  pivot_longer(
    cols = 2:length(.),
    names_to = "circle2",
    values_to = "euclideanDistance") %>%
  filter(circle != circle2) %>%
  distinct(euclideanDistance, .keep_all = T) %>%
  left_join(
    jaccard_matrix_ground %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "jaccardDissimilarity"),
    by = c("circle","circle2")) %>%
  left_join(
    herbaceous_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "herbaceousCover"),
    by = c("circle","circle2")) %>%
  left_join(
    distance_road_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "distanceToRoad"),
    by = c("circle","circle2")) %>%
  left_join(
    distance_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "geographicDistance"),
    by = c("circle","circle2")) %>%
  left_join(
    litter_depth_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "litterDepth"),
    by = c("circle","circle2")) %>%
  rename("circle1" = "circle") %>%
  left_join(
    forest_matrix %>%
      pivot_longer(
        cols = 2:length(.),
        names_to = "circle2",
        values_to = "forest_1km"),
    by = c("circle1","circle2")) %>%
  left_join(
    paths,
    by = c("circle1", "circle2")) %>% 
  mutate(
    circles = str_c(circle1, circle2, sep = "_"),
    distanceToRoad = distanceToRoad/1000,
    geographicDistance = geographicDistance/1000) %>%
  dplyr::select(!circle1:circle2) %>%
  relocate(circles)

### conducting analyses -----------------------------------------------------


## foliage arthropods ------------------------------------------------------

# use Mantel tests to calculate p-values for correlations between matrices where observations are non-independent, in this case due to any one sampling plot being included in the calculation for its distance to other plots.
# use linear regression to calculate R^2 values where p < 0.1

# Euclidean distance versus difference in proportion canopy cover
# p ~ 0.4
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(canopy_cover_matrix[2:31]), 
  nrepet = 9999)

# Euclidean distance versus difference in proportion forest cover
# p = 0.0001
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.19
summary(lm(
  euclideanDistance ~ forest_1km,
  data = analysis_frame_foliage))

# Euclidean distance versus geographic distance, p < 0.005
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.11
summary(lm(
  euclideanDistance ~ geographicDistance,
  data = analysis_frame_foliage))

# Euclidean distance versus resistance, p = 0.0001
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(paths_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.26
summary(lm(
  euclideanDistance ~ resistance,
  data = analysis_frame_foliage))

# Euclidean distance versus Jaccard dissimilarity of sample tree species
# p < 0.005
mantel.rtest(
  as.dist(euclidean_matrix_foliage[2:31]), 
  as.dist(trees_jaccard_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.22
summary(lm(
  euclideanDistance ~ treeDissimilarity,
  data = analysis_frame_foliage))

# Jaccard dissimilarity versus difference in proportion canopy cover
# p < 0.1
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(canopy_cover_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.04
summary(lm(
  jaccardDissimilarity ~ canopyCover,
  data = analysis_frame_foliage))

# Jaccard dissimilarity versus proportion forest cover
# p = 0.0001
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.14
summary(lm(
  jaccardDissimilarity ~ forest_1km,
  data = analysis_frame_foliage))

# Jaccard dissimilarity versus geographic distance
# p < 0.005
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.09
summary(lm(
  jaccardDissimilarity ~ geographicDistance,
  data = analysis_frame_foliage))

# Jaccard dissimilarity versus resistance
# p = 0.0001
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(paths_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.25
summary(lm(
  jaccardDissimilarity ~ resistance,
  data = analysis_frame_foliage))

# Jaccard dissimilarity versus Jaccard dissimilarity of tree species
# p = 0.0002
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(trees_jaccard_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.16
summary(lm(
  jaccardDissimilarity ~ treeDissimilarity,
  data = analysis_frame_foliage))

# principal component analysis --------------------------------------------

# modify the foliage_families dataframe to add rows for families not observed at a given site
all_foliage <- map(
  unique(foliage_families$CircleFK),
  function(x){
    all_families <-  unique(foliage_families$family)
    
    in_families <-  foliage_families %>% 
      filter(CircleFK == x) %>% 
      pull(family) %>% 
      unique()
    
    out_families <- all_families[!all_families %in% in_families]
    
    rbind(
      foliage_families %>% 
        filter(CircleFK == x),
      tibble(
        CircleFK = rep(x, length(out_families)),
        family = out_families,
        n_individuals = rep(0, length(out_families)),
        biomass = rep(0, length(out_families))))
  }) %>% 
  bind_rows() %>% 
  arrange(CircleFK, family)

# make a data frame for foliage arthropods that can be PCA'ed - columns are families, rows are circles
foliage_base1 <- map_dfc(
  unique(foliage_families$CircleFK),
  ~ all_foliage %>%
    filter(
      CircleFK == .x,
      !is.na(family)) %>% 
    mutate(
      CircleFK = if_else(
        is.na(CircleFK),
        true = .x,
        false = CircleFK),
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      logBiomass = log(biomass+0.01)) %>%
    dplyr::select(!n_individuals:biomass) %>%
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as_tibble(rownames = "family") %>%
    arrange(family) %>%
    dplyr::select(!family)) %>% 
  cbind(family = unique(all_foliage$family[!is.na(all_foliage$family)])) %>% 
  relocate(family) %>%
  as_tibble() %>%
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x))) %>% 
  # switch rows and columns
  t() %>% 
  # get column names from the first row
  row_to_names(row_number = 1) %>% 
  as_tibble() %>% 
  mutate(across(.fns = as.numeric)) %>% 
  cbind(CircleID = circles$CircleID) %>% 
  left_join(
    circles %>% 
      dplyr::select(CircleID, SiteFK),
    by = "CircleID")

# set row names for the PCA data frame
rownames(foliage_base1) <- circles$CircleID

# conduct the PCA
foliage_pca <- prcomp(foliage_base1[1:(ncol(foliage_base1)-3)], scale = F)

# isolate the families with loadings greater than 0.14 on principal component axes 1 or 2
sub_rot <- foliage_pca$rotation %>% 
  as_tibble(rownames = "family") %>% 
  filter(abs(PC1) > 0.14 | abs(PC2) > 0.14)%>%  
  mutate(highlight = if_else(
    family %in% c("Araneidae","Coccinellidae","Tenebrionidae","Sclerosomatidae"),
    true = "yes",
    false = "no"))

# join PCA and predictor variable dataframes to include both in figures
foliage_loads <- foliage_pca$x %>% 
  as_tibble(rownames = "CircleID") %>% 
  dplyr::select(1:3) %>% 
  left_join(
    circles,
    by = "CircleID")

# assess correlation between PCs and predictor variables
foliage_cor <- cor(foliage_loads[c(2,3,8,11)])

corrplot(
  foliage_cor*foliage_cor,
  method = "number")

# view variance accounted for by PCs
summary(foliage_pca)


## ground arthropods -------------------------------------------------------

# Euclidean distance versus difference in herbaceous cover class
# p ~ 0.5
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(herbaceous_matrix[2:31]), 
  nrepet = 9999)

# Euclidean distance versus difference in proportion forest cover
# p = 0.0001
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.51
summary(lm(
  euclideanDistance ~ forest_1km,
  data = analysis_frame_ground))

# Euclidean distance versus difference in litter depth
# p ~ 0.5
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(litter_depth_matrix[2:31]), 
  nrepet = 9999)

# Euclidean distance versus geographic distance
# p < 0.0005
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.09
summary(lm(
  euclideanDistance ~ geographicDistance,
  data = analysis_frame_ground))

# Euclidean distance versus resistance
# p = 0.0001
mantel.rtest(
  as.dist(euclidean_matrix_ground[2:31]), 
  as.dist(paths_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.08
summary(lm(
  euclideanDistance ~ resistance,
  data = analysis_frame_ground))

# Jaccard dissimilarity versus difference in herbaceous cover class
# p ~ 0.1
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(herbaceous_matrix[2:31]), 
  nrepet = 9999)

# Jaccard dissimilarity versus difference in litter depth
# p ~ 0.2
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(litter_depth_matrix[2:31]), 
  nrepet = 9999)

# Jaccard dissimilarity versus difference in proportion forest cover
# p = 0.0001
mantel.rtest(
  as.dist(jaccard_matrix_ground[2:31]), 
  as.dist(forest_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.27
summary(lm(
  jaccardDissimilarity ~ forest_1km,
  data = analysis_frame_ground))

# Jaccard dissimilarity versus geographic distance
# p < 0.005
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(distance_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.04
summary(lm(
  jaccardDissimilarity ~ geographicDistance,
  data = analysis_frame_ground))

# Jaccard dissimilarity versus resistance
# p = 0.0001
mantel.rtest(
  as.dist(jaccard_matrix_foliage[2:31]), 
  as.dist(paths_matrix[2:31]), 
  nrepet = 9999)

# R^2 = 0.13
summary(lm(
  jaccardDissimilarity ~ resistance,
  data = analysis_frame_ground))

# principal component analysis --------------------------------------------

# add arthropods not at a given sampling plot to the ground_families data frame
all_grounds <- map(
  unique(ground_families$CircleID),
  function(x){
    all_families <-  unique(ground_families$family)
    
    in_families <-  ground_families %>% 
      filter(CircleID == x) %>% 
      pull(family) %>% 
      unique()
    
    out_families <- all_families[!all_families %in% in_families]
    
    rbind(
      ground_families %>% 
        filter(CircleID == x),
      tibble(
        CircleID = rep(x, length(out_families)),
        family = out_families,
        n_individuals = rep(0, length(out_families)),
        biomass = rep(0, length(out_families))))
  }) %>% 
  bind_rows() %>% 
  arrange(CircleID, family)

# make a data frame of ground arthropods that can be PCA'ed - columns are families, rows are circles
ground_base1 <- map_dfc(
  unique(ground_families$CircleID),
  ~ all_grounds %>%
    filter(
      CircleID == .x,
      !is.na(family)) %>%
    mutate(
      CircleID = if_else(
        is.na(CircleID),
        true = .x,
        false = CircleID),
      biomass = if_else(
        is.na(biomass),
        true = 0,
        false = biomass),
      logBiomass = log(biomass+0.01)) %>%
    dplyr::select(!n_individuals:biomass) %>%
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    t() %>%
    row_to_names(row_number = 1) %>%
    as_tibble(rownames = "family") %>%
    arrange(family) %>%
    dplyr::select(!family)) %>%
  cbind(family = unique(all_grounds$family[!is.na(all_grounds$family)])) %>%
  relocate(family) %>%
  as_tibble() %>%
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x))) %>% 
  # switch rows and columns
  t() %>% 
  # get column names from the first row
  row_to_names(row_number = 1) %>% 
  as_tibble() %>% 
  mutate(across(.fns = as.numeric)) %>% 
  cbind(CircleID = circles$CircleID) %>% 
  left_join(
    circles %>% 
      dplyr::select(CircleID, SiteFK),
    by = "CircleID")

# add row names to the data frame
rownames(ground_base1) <- circles$CircleID

# perform the PCA
ground_pca <- prcomp(ground_base1[1:(ncol(ground_base1)-3)], scale = F)

# isolate families with loadings greater and 0.14 on principal component axes 1 or 2
sub_rot2 <- ground_pca$rotation %>% 
  as_tibble(rownames = "family") %>% 
  filter(abs(PC1) > 0.14 | abs(PC2) > 0.14)%>% 
  mutate(highlight = if_else(
    family %in% c("Carabidae","Lycosidae","Armadillidae","Porcellionidae","Rhaphidophoridae"),
    true = "yes",
    false = "no"))

# join PCA and predictor variable dataframes to include both in figures
ground_loads <- ground_pca$x %>% 
  as_tibble(rownames = "CircleID") %>% 
  dplyr::select(1:3) %>% 
  left_join(
    circles,
    by = "CircleID")

# assess correlation between PCs and predictor variables
ground_cor <- cor(ground_loads[c(2,3,7,11)])

corrplot(
  ground_cor*ground_cor,
  method = "number")

# view variance accounted for by PCs
summary(ground_pca)


### generating figures ------------------------------------------------------

# create a theme for figures
ul_theme2 <- theme(
  axis.title = element_text(size = 10),
  panel.border = element_rect(fill = NA, color = "darkslategray", size = 1.2),
  panel.grid = element_line(color = "cornsilk3"),
  panel.background = element_rect(fill = "gray100"),
  plot.margin = unit(c(0.05,0.1,0.1,0.1), unit = "npc"),
  axis.title.x = element_text(vjust = 1))

# set a colorblind-friendly pallet
colorz  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## foliage arthropod figures -----------------------------------------------


# scatter plots -----------------------------------------------------------

# Euclidean distance versus difference in canopy cover
foliage_plot_euclidean_canopy <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = canopyCover,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = "\u0394 Local Canopy Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus difference in proportion forest cover
foliage_plot_euclidean_forest <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = forest_1km,
    y = euclideanDistance)) +
  geom_point() +
  # add a linear regression trendline
  geom_smooth(
    method = "lm",
    se = F,
    color = "#009E73") +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  # add R^2 and p-values
  annotate(
    "text",
    x = 0.75,
    y = 7.5,
    label = "R^2 == 0.19",
    parse = T,
    color = "#009E73") +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus Jaccard dissimilarity of sample trees
foliage_plot_euclidean_trees <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = treeDissimilarity,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#009E73") +
  labs(
    x = "Tree Species Dissimilarity",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 0.75,
    y = 7.5,
    label = "R^2 == 0.22",
    parse = T,
    color = "#009E73") +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus geographic distance
foliage_plot_euclidean_distance <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = geographicDistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Geographic Distance (km)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 35,
    y = 7.5,
    label = "R^2 == 0.11",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus resistance
foliage_plot_euclidean_paths <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = resistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Resistance of Shortest Path",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,35), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 7.5,
    y = 7.5,
    label = "R^2 == 0.26",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus difference in proportion canopy cover
foliage_plot_jaccard_canopy <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = canopyCover,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = "\u0394 Local Canopy Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus difference in proportion forest cover
foliage_plot_jaccard_forest <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = forest_1km,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#009E73") +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 0.75,
    y = 0.2,
    label = "R^2 == 0.14",
    parse = T,
    color = "#009E73") +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity of arthropod communities versus Jaccard dissimilarity of sampled tree species
foliage_plot_jaccard_trees <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = treeDissimilarity,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#009E73") +
  labs(
    x = "Tree Species Dissimilarity",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 0.75,
    y = 0.2,
    label = "R^2 == 0.16",
    parse = T,
    color = "#009E73") +
  theme(axis.title = element_text(size = 14))

# Jaccard distance versus geographic distance
foliage_plot_jaccard_distance <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = geographicDistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Geographic Distance (km)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 35,
    y = 0.2,
    label = "R^2 == 0.09",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# Jaccard distance versus resistance
foliage_plot_jaccard_paths <- ggplot(
  data = analysis_frame_foliage,
  mapping = aes(
    x = resistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Resistance of Shortest Path",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 7.5,
    y = 0.2,
    label = "R^2 == 0.25",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# compile all plots with Euclidean distance on the y-axis
foliage_euclidean_plots <- ggarrange( 
  foliage_plot_euclidean_forest, 
  foliage_plot_euclidean_canopy,
  foliage_plot_euclidean_trees,
  foliage_plot_euclidean_distance,
  foliage_plot_euclidean_paths,
  ncol = 1,
  labels = c("a)","c)","e)","g)","i)"),
  label.x = -0.01)

# compile all plots with Jaccard dissimilarity on the y-axis
foliage_jaccard_plots <- ggarrange( 
  foliage_plot_jaccard_forest,
  foliage_plot_jaccard_canopy,
  foliage_plot_jaccard_trees,
  foliage_plot_jaccard_distance,
  foliage_plot_jaccard_paths,
  ncol = 1,
  labels = c("b)","d)","f)","h)","j)"),
  label.x = -0.01)

# combine all plots
foliage_plots <- ggarrange(
  foliage_euclidean_plots,
  foliage_jaccard_plots,
  ncol = 2)

# save plots
ggsave(
  "figures/figure2.jpg",
  plot = foliage_plots,
  width = 9.75,
  height = 13.5,
  units = "in")


# PCA plot (foliage) ------------------------------------------------------

foliage_pca_plot <- ggplot() +
  geom_point(
    data = foliage_loads,
    mapping = aes(
      x = PC1,
      y = PC2,
      shape = SiteFK,
      fill = forest_1km,
      color = forest_1km),
    size = 3) +
  scale_shape_manual(values = c(21:24,8,25)) +
  scale_color_gradient(
    low = colorz[2],
    high = colorz[4]) +
  scale_fill_gradient(
    low = colorz[2],
    high = colorz[4],
    guide = "none") +
  labs(
    shape = "Site Code",
    color = "Proportion Forest Cover") +
  new_scale_color() +
  scale_color_manual(values = c(colorz[1],colorz[7])) +
  geom_segment(
    data = sub_rot,
    mapping = aes(
      x = 0,
      y = 0,
      xend = PC1*15,
      yend = PC2*15,
      color = highlight),
    show.legend = F,
    arrow = arrow(length = unit(0.03, "npc")),
    linewidth = 1.1) +
  labs(
    x = "PC1 (17%)",
    y = "PC2 (11%)") +
  geom_text(
    data = sub_rot %>% 
      mutate(family = case_when(
        family %in% c("Araneidae","Coccinellidae","Tenebrionidae","Sclerosomatidae") ~ family,
        .default = NULL)),
    mapping = aes(
      x = PC1*12.5+0.1,
      y = PC2*20-0.5,
      label = family,
      colour = highlight),
    size = 3.5,
    show.legend = F) +
  ul_theme2 +
  theme(plot.margin = unit(c(0.1,0.05,0.05,0.05), unit = "npc"))

## ground arthropod figures ------------------------------------------------


# scatter plots -----------------------------------------------------------

# Euclidean distance versus difference in herbaceous cover class
ground_plot_euclidean_herb <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = herbaceousCover,
    y = euclideanDistance)) +
  geom_point() +
  labs(
    x = "\u0394 Herbaceous Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus difference in litter depth
ground_plot_euclidean_litter <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = litterDepth,
    y = euclideanDistance)) +
  geom_point() + 
  labs(
    x = "\u0394 Litter Depth (mm)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus difference in proportion forest cover
ground_plot_euclidean_forest <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = forest_1km,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#009E73") +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 0.75,
    y = 7.5,
    label = "R^2 == 0.51",
    parse = T,
    color = "#009E73") +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus geographic distance
ground_plot_euclidean_distance <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = geographicDistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Geographic Distance (km)",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 35,
    y = 7.5,
    label = "R^2 == 0.09",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# Euclidean distance versus resistance
ground_plot_euclidean_paths <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = resistance,
    y = euclideanDistance)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Resistance of Shortest Path",
    y = "Euclidean Distance") +
  scale_y_continuous(limits = c(0,40), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 7.5,
    y = 7.5,
    label = "R^2 == 0.08",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus difference in herbaceous cover class
ground_plot_jaccard_herb <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = herbaceousCover,
    y = jaccardDissimilarity)) +
  geom_point() + 
  labs(
    x = "\u0394 Herbaceous Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus difference in litter depth
ground_plot_jaccard_litter <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = litterDepth,
    y = jaccardDissimilarity)) +
  geom_point() +
  labs(
    x = "\u0394 Litter Depth (mm)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus difference in proportion forest cover
ground_plot_jaccard_forest <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = forest_1km,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#009E73") +
  labs(
    x = "\u0394 1-km Forest Cover",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 0.75,
    y = 0.2,
    label = "R^2 == 0.27",
    parse = T,
    color = "#009E73") +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus geographic distance
ground_plot_jaccard_distance <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = geographicDistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Geographic Distance (km)",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 35,
    y = 0.2,
    label = "R^2 == 0.04",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# Jaccard dissimilarity versus resistance
ground_plot_jaccard_paths <- ggplot(
  data = analysis_frame_ground,
  mapping = aes(
    x = resistance,
    y = jaccardDissimilarity)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = F,
    color = "#E69F00") +
  labs(
    x = "Resistance of Shortest Path",
    y = "Jaccard Dissimilarity") +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  ul_theme2 +
  annotate(
    "text",
    x = 7.5,
    y = 0.2,
    label = "R^2 == 0.13",
    parse = T,
    color = "#E69F00") +
  theme(axis.title = element_text(size = 14))

# compile plots with Euclidean distance on the y-axis
ground_euclidean_plots <- ggarrange(
  ground_plot_euclidean_forest, 
  ground_plot_euclidean_litter,
  ground_plot_euclidean_herb,
  ground_plot_euclidean_distance,
  ground_plot_euclidean_paths,
  ncol = 1,
  labels = c("a)","c)","e)","g)","i)"),
  label.x = -0.01)

# compile plots with Jaccard dissimilarity on the y-axis
ground_jaccard_plots <- ggarrange(
  ground_plot_jaccard_forest,  
  ground_plot_jaccard_litter,
  ground_plot_jaccard_herb,
  ground_plot_jaccard_distance,
  ground_plot_jaccard_paths,
  ncol = 1,
  labels = c("b)","d)","f)","h)","j)"),
  label.x = -0.01)

# compile all plots
ground_plots <- ggarrange(
  ground_euclidean_plots,
  ground_jaccard_plots,
  ncol = 2)

# save plots
ggsave(
  filename = "figures/figure3.jpg",
  plot = ground_plots,
  width = 9.75,
  height = 13.5,
  units = "in")


# PCA plot (ground) -------------------------------------------------------

ground_pca_plot <- ggplot() +
  geom_point(
    data = ground_loads,
    mapping = aes(
      x = PC1,
      y = PC2,
      shape = SiteFK,
      fill = forest_1km,
      color = forest_1km),
    size = 3) +
  scale_shape_manual(values = c(21:24,8,25)) +
  scale_color_gradient(
    low = colorz[2],
    high = colorz[4]) +
  scale_fill_gradient(
    low = colorz[2],
    high = colorz[4],
    guide = "none") +
  labs(
    shape = "Site Code",
    color = "Proportion Forest Cover") +
  new_scale_color() +
  scale_color_manual(values = c(colorz[1], colorz[7])) +
  geom_segment(
    data = sub_rot2,
    mapping = aes(
      x = 0,
      y = 0,
      xend = PC1*15,
      yend = PC2*15,
      color = highlight),
    show.legend = F,
    arrow = arrow(length = unit(0.03, "npc")),
    linewidth = 1.1) +
  labs(
    x = "PC1 (37%)",
    y = "PC2 (12%)") +
  geom_text(
    data = sub_rot2 %>% 
      mutate(family = case_when(
        family %in% c("Carabidae","Lycosidae","Armadillidae","Porcellionidae","Rhaphidophoridae") ~ family,
        .default = NULL)),
    mapping = aes(
      x = PC1*15,
      y = PC2*17+0.02/PC2,
      label = family,
      color = highlight),
    size = 3.5,
    show.legend = F) +
  ul_theme2 +
  theme(plot.margin = unit(c(0.1,0.05,0.05,0.05), unit = "npc"))

pca_plots <- ggarrange(
  foliage_pca_plot,
  ground_pca_plot,
  labels = c("a)","b)"),
  ncol = 1,
  legend = "right",
  common.legend = T)

ggsave(
  plot = pca_plots,
  filename = "figures/figure4.jpg",
  width = 6,
  height = 8,
  units = "in")


### supplemental ------------------------------------------------------------


## resistance models -------------------------------------------------------


# read in the resistance values between sampling plots using three different raster classification models of resistance
paths <- read_table(
  file = "data/all10_paths_resistances_3columns",
  col_names = F) %>% 
  rename("all10_resistance" = X3) %>% 
  full_join(
    read_table(
      file = "data/mod1_paths_resistances_3columns",
      col_names = F) %>% 
      rename("mod1_resistance" = X3),
    by = c("X1","X2")) %>% 
  full_join(
    read_table(
      file = "data/mod2_paths_resistances_3columns",
      col_names = F) %>% 
      rename("mod2_resistance" = X3),
    by = c("X1","X2")) %>% 
  rename("nodeID_1" = X1, "nodeID_2" = X2) %>% 
  # replace node IDs used for CircuitScape with sampling plot IDs
  mutate(
    circle1 = str_replace(nodeID_1, "^1", "DF") %>% 
      str_replace("^2", "ERSP") %>% 
      str_replace("^3", "JMNP") %>% 
      str_replace("^4", "NCBG") %>% 
      str_replace("^5", "NCSU") %>% 
      str_replace("^6", "UNC"),
    circle2 = str_replace(nodeID_2, "^1", "DF") %>% 
      str_replace("^2", "ERSP") %>% 
      str_replace("^3", "JMNP") %>% 
      str_replace("^4", "NCBG") %>% 
      str_replace("^5", "NCSU") %>% 
      str_replace("^6", "UNC"),
    circles = str_c(circle1, circle2, sep = "_")) %>% 
  dplyr::select(!c(nodeID_1,nodeID_2,circle1,circle2)) %>% 
  relocate(circles)

# calculate R^2 values for resistances using different models
summary(lm(paths$mod1_resistance ~ paths$all10_resistance))

# plot model resistances against one another with R^2 values displayed
all10_mod1 <- ggplot(paths) +
  geom_point(
    aes(
      x = all10_resistance,
      y = mod1_resistance)) +
  labs(
    x = "Path Resistance from Model 1",
    y = "Path Resistance from Model 2") +
  annotate(
    "text",
    x = 7.5,
    y = 2,
    label = "R^2 == 0.99",
    parse = T) +
  ul_theme2

summary(lm(paths$mod2_resistance ~ paths$all10_resistance))

all10_mod2 <- ggplot(paths) +
  geom_point(
    aes(
      x = all10_resistance,
      y = mod2_resistance)) +
  labs(
    x = "Path Resistance from Model 1",
    y = "Path Resistance from Model 3") +
  annotate(
    "text",
    x = 7.5,
    y = 2,
    label = "R^2 == 0.95",
    parse = T) +
  ul_theme2

summary(lm(paths$mod2_resistance ~ paths$mod1_resistance))

mod1_mod2 <- ggplot(paths) +
  geom_point(
    aes(
      x = mod1_resistance,
      y = mod2_resistance)) +
  labs(
    x = "Path Resistance from Model 2",
    y = "Path Resistance from Model 3") +
  annotate(
    "text",
    x = 7.5,
    y = 2,
    label = "R^2 == 0.94",
    parse = T) +
  ul_theme2

# make a blank plot
blank <- ggplot() +
  theme_void()

# combine plots into figure
modplots <- ggarrange(
  all10_mod1,
  all10_mod2,
  mod1_mod2,
  blank,
  labels = c("a)","b)","c)","d)"))

# save combined figure
ggsave(
  "figures/resistance_supplemental.jpg",
  plot = modplots,
  width = 6.5,
  height = 6,
  units = "in")

