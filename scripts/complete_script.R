
# setup -------------------------------------------------------------------

# install necessary packages, listed below, before proceeding
# load necessary packages
library(tidyverse)
library(ggpubr)
library(ggfortify)
library(janitor)

# read in data on observations of foliage arthropods
foliage_arths <- read_csv(
  # identifies the file by name, without specifying date tag - only the most recent version of a table should be saved at any time
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_arths')])

# read in data on observations of ground arthropods
ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_arths')])

# read in list of observed taxa
taxa <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^taxa')])

# read in data on beat sheet surveys
beatsheets <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^beat_sheets')])

# read in data on pitfall trap runs
pitfalls <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^pitfalls')])

# read in data on survey trees
trees <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])

# read in data on sampling plots
circles <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^circles')])

# read in data on sampling sites
sites <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^sites')])


## calculating community dissimilarity metrics -----------------------------


# foliage arthropod communities -------------------------------------------

# create a tibble with the number of individuals and total biomass of each family observed at each sampling plot
foliage_families <- foliage_arths %>%
  # filter to orders included in analysis
  filter(
    order %in% c('Araneae','Coleoptera','Hemiptera','Opiliones','Orthoptera') | (order == 'Hymenoptera' & family == 'Formicidae')) %>%
  # join survey data
  left_join(
    beatsheets %>%
      select(BeatSheetID, Date, TreeFK),
    by = c('BeatSheetFK' = 'BeatSheetID')) %>%
  # join survey tree data
  left_join(
    trees %>%
      select(TreeID, CircleFK),
    by = c('TreeFK' = 'TreeID')) %>%
  # join sampling plot data
  left_join(
    circles,
    by = c('CircleFK' = 'CircleID')) %>%
  # calculate the total number of individuals and total biomass of arthropods in each family observed across all beat sheet surveys at each survey tree
  group_by(CircleFK, family) %>%
  summarize(
    n_individuals = sum(Quantity, na.rm = T),
    biomass = sum(TotalMass))

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
    select(!n_individuals:biomass) %>%
    # pivot out so each sampling plot is a column
    pivot_wider(
      names_from = family,
      values_from = logBiomass) %>%
    # transpose the frame
    t() %>%
    # get the circle names out of the first row
    row_to_names(row_number = 1) %>%
    # make it a real boy (convert it to a tibble)
    as_tibble(rownames = 'family') %>%
    # put it in alphabetical order by family
    arrange(family) %>%
    # remove family
    select(!family)) %>%
  # put family back in at the end of the map function
  cbind(family = foliageFams) %>%
  # put family at the front for my sanity
  relocate(family) %>%
  # re-convert to a tibble
  as_tibble() %>%
  # convert contents to numeric
  mutate(across(.cols = DF1:UNC8, .fns = ~ as.numeric(.x)))
