# load required packages

library(tidyverse)
library(raster)
library(sf)
library(gdm)

# read in and format files

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
circles_raw <- read_csv("data/circles_2022-10-03.csv")

# read in data on sampling sites
sites <- read_csv(
  list.files("data", full.names = T)[str_detect(list.files("data"), "^sites")])

# read in flight info
ground_flight <- read_csv('data/ground_families.csv')
foliage_flight <- read_csv('data/foliage_families.csv')

# read in the raster file of land cover classes in the study area
nlcd <- raster("data/nlcd_local") %>% 
  # the raster seems to have picked up a weird extra chunk of extent on the right edge - cropping it out
  crop(extent(c(1493025,1551500,1541175,1598865)))

# convert the circle coordinates to a spatial object
circles_sf <- circles_raw %>% 
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

# calculate the proportion forest cover within 1km of each circle and join it to the circle data
circles <-  raster::extract(
  x = nlcd_forest,
  y = circles_sf,
  buffer = 1000,
  fun = mean,
  na.rm = T,
  df = T) %>% 
  rename(
    forest_1km = nlcd_local) %>% 
  cbind(circles_raw) %>% 
  relocate(
    forest_1km,
    .after = DistanceToEdgem) %>% 
  dplyr::select(!ID) %>% 
  cbind(
    st_coordinates(circles_sf)) %>% 
  dplyr::arrange(CircleID)

# resistance path distances

# create a reclass matrix for NLCD land cover data based on the criteria listed below
# water, barren, all developed except open space assigned 10, all non-forest plant cover (including agriculture and developed open space) assigned 5, all forest assigned 1
#mod1 <- tibble(
#  from = unique(nlcd),
#  to = c(10,5,rep(10,4),rep(1,3),rep(5,6)))

#nlcd_mod1 <- reclassify(x = nlcd, rcl = mod1)

#writeRaster(nlcd_mod1, filename = "data/mod1", format = "ascii", overwrite = T)

# the intervening step requires using the CircuitScape GUI to run the calculations for the resistances of shortest paths between sampling plots, using the raster created above and the "circles.txt" file. The following line reads in the output from this operation as a 3-column dataframe

# format resistance as a distance matrix
paths_temp <- read_table(
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
  dplyr::select(!c(nodeID_1, nodeID_2))
  
paths <- paths_temp %>% 
  rbind(
    tibble(
      resistance = rep(0,30),
      circle1 = unique(circles$CircleID),
      circle2 = unique(circles$CircleID))) %>% 
  rbind(
    tibble(
      resistance = paths_temp$resistance,
      circle1 = paths_temp$circle2,
      circle2 = paths_temp$circle1)) %>% 
  arrange(circle1, circle2) %>% 
  pivot_wider(
    names_from = "circle2",
    values_from = "resistance") %>% 
  rename("CircleID" = "circle1") %>% 
  relocate(DF1, .after = CircleID) %>% 
  as.matrix()

# create a function to calculate Jaccard similarity - takes three vectors of presence/absence values as inputs, where a is the vector of families found at both sampling plots and b and c are the vectors of families found at one sampling plot but not the other
jaccard <- function(a,b,c) {a/(a+b+c)}

# generate a data frame with a row for each tree species found at each sampling plot
tree_species <- trees %>% 
  dplyr::select(CircleFK, Species) %>% 
  distinct()

# calculate tree species dissimilarity between circles
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
  cbind(CircleID = circles$CircleID) %>%
  relocate(CircleID) %>% 
  as.matrix()

# create a tibble with the total biomass of each family observed at each sampling plot
foliage_abundances <- foliage_arths %>%
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
  summarize(biomass = sum(TotalMass, na.rm = T)) %>% 
  pivot_wider(
    names_from = family,
    values_from = biomass) %>% 
  mutate(
    across(
      .cols = 1:58,
      .fns = ~ if_else(
        condition = is.na(.x),
        true = 0,
        false = .x))) %>% 
  rename("CircleID" = "CircleFK")

foliage_presence <- foliage_abundances %>% 
  mutate(
    across(
      .cols = 1:58,
      .fns = ~ if_else(
        condition = .x > 0,
        true = 1,
        false = 0)))

foliage_jaccard_data <- formatsitepair(
  bioData = foliage_presence,
  bioFormat = 1,
  dist = "jaccard",
  abundance = F,
  siteColumn = "CircleID",
  XColumn = "X",
  YColumn = "Y",
  predData = circles %>% 
    dplyr::select(c(CircleID,PercentCanopyCover,forest_1km,X,Y)),
  distPreds = list(paths,trees_jaccard_matrix))

foliage_jaccard_gdm <- gdm.varImp(
  spTable = foliage_jaccard_data,
  geo = T,
  nPerm = 999)

# ground arthropod analysis
ground_abundances <- ground_arths %>%
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
  summarize(biomass = sum(TotalMass, na.rm = T)) %>% 
  pivot_wider(
    names_from = family,
    values_from = biomass) %>% 
  mutate(
    across(
      .cols = 1:37,
      .fns = ~ if_else(
        condition = is.na(.x),
        true = 0,
        false = .x)))

ground_presence <- ground_abundances %>% 
  mutate(
    across(
      .cols = 1:37,
      .fns = ~ if_else(
        condition = .x > 0,
        true = 1,
        false = 0)))

ground_jaccard_data <- formatsitepair(
  bioData = ground_presence,
  bioFormat = 1,
  dist = "jaccard",
  abundance = F,
  siteColumn = "CircleID",
  XColumn = "X",
  YColumn = "Y",
  predData = circles %>% 
    dplyr::select(c(CircleID,LitterDepthmm,HerbCoverEstimate,X,Y)),
  distPreds = list(paths))

ground_jaccard_gdm <- gdm.varImp(
  spTable = ground_jaccard_data,
  geo = T,
  nPerm = 999)
