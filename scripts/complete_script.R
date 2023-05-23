
# setup -------------------------------------------------------------------

# install necessary packages
library(tidyverse)
library(ggpubr)
library(ggfortify)
library(janitor)
library(ggrepel)

# read in observations of foliage arthropods
foliage_arths <- read_csv(
  # identifies the file by name, without specifying date tag - only the most recent version of a table should be saved at any time
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliage_arths')])

# read in observations of ground arthropods
ground_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^ground_arths')])