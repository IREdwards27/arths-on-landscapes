
# setup -------------------------------------------------------------------

library(rvest)
library(xml2)
library(tidyverse)
library(lubridate)
library(googledrive)
library(googlesheets4)


# update beat sheet survey and arth records -------------------------------

# function to pull surveys and arths from CC site
updateBeatSheets <- function(updateExpertNames = FALSE) {
  
  # Remove old data files
  
  oldfiles <- data.frame(filename = list.files('data/cc_pulls', full.names = T))
  
  unlink(oldfiles$filename)
  
  # Download most recent files from Caterpillars Count backup site
  
  webpage_url <- "https://caterpillarscount.unc.edu/backups/"
  
  webpage <- xml2::read_html(webpage_url)
  
  
  links <- rvest::html_table(webpage)[[1]] %>% 
    tibble::as_tibble(.name_repair = "unique") %>%
    mutate(text_date = word(Name, sep = "_", 1),
           file_type = word(Name, sep = "_", 2),
           date = as.Date(text_date, format = "%Y-%m-%d"))
  
  recent_date = max(links$date, na.rm = TRUE)
  
  base_filenames = c('ArthropodSighting', 'Plant', 'Survey')
  filenames = paste0(recent_date, "_", base_filenames, ".csv")
  
  for(f in filenames) {
    download.file(paste0(webpage_url, f), paste0('data/cc_pulls/', f)) 
  }
  
  if (updateExpertNames) {
    
    source('expert_id_taxon_names.r')
    
    probNames = updateExpertClassification()
    
    return(probNames)
    
  }
  
}

updateBeatSheets()

# read in plants
old_plants <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])

my_surveys <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^beatsheets')]) %>% 
  mutate(
    Date = as.Date(Date, format = '%m/%d/%Y'))

# read in all the CC plants

all_plants <- read_csv(
  list.files('data/cc_pulls/', full.names = T)[str_detect(list.files('data/cc_pulls'), 'Plant.csv$')]) %>%
  select(
    'CCID' = 'ID',
    'TreeID' = 'Code')

# get new CCIDs
old_plants %>% 
  select(!CCID) %>% 
  left_join(
    all_plants,
    by = 'TreeID') %>%
  relocate(TreeID, CCID) %>%
  write_csv(str_c('data/trees_', today(), '.csv'))

unlink(list.files('data', full.names = T)[str_detect(list.files('data'), 'trees') & !str_detect(list.files('data'), as.character(today()))])

# updated plants
my_plants <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^trees')])

# read in all the CC surveys
all_surveys <- read_csv(
  list.files('data/cc_pulls/', full.names = T)[str_detect(list.files('data/cc_pulls'), 'Survey.csv$')])

# select only new surveys
new_surveys <- all_surveys %>%
  left_join(
    my_plants,
    by = c('PlantFK' = 'CCID')) %>% 
  filter(
    UserFKOfObserver == 2832,
    LocalDate > max(my_surveys$Date, na.rm = T),
    ObservationMethod == 'Beat sheet',
    TreeID %in% my_plants$TreeID,
    !str_detect(replace_na(Notes, '-999'), 'CC')) %>% 
  mutate(Observer = case_when(
    UserFKOfObserver == 2832 ~ 'Indigo'),
    Checks = rep(NA, nrow(.))) %>% 
  select(
    'BeatSheetID' = 'ID',
    'TreeFK' = 'TreeID',
    Observer,
    'Date' = 'LocalDate',
    'Time' = 'LocalTime',
    WetLeaves,
    NumberOfLeaves,
    AverageLeafLength,
    Notes,
    Checks) %>% 
  distinct()

# check that there are exactly 5 surveys in each circle (should return an empty table if so)
new_surveys %>% 
  left_join(
    my_plants,
    by = c('TreeFK' = 'TreeID')) %>% 
  distinct() %>% 
  group_by(CircleFK, Date) %>% 
  summarize(n = n()) %>% 
  filter(n != 5)

# read in all the arths
all_arths <- read_csv(
  list.files('data/cc_pulls/', full.names = T)[str_detect(list.files('data/cc_pulls'), 'ArthropodSighting.csv$')])

# read in my arths
my_arths <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^foliagearths')])

# select new arths
new_arths <- all_arths %>% 
  filter(
    SurveyFK %in% new_surveys$BeatSheetID) %>% 
  select(
    'FoliageArthID' = 'ID',
    'BeatSheetFK' = 'SurveyFK',
    'CCGroup' = 'UpdatedGroup',
    Length,
    Quantity,
    PhotoURL,
    'CCNotes' = 'Notes') %>% 
  mutate(
    TaxonLevel = case_when(
      CCGroup == 'ant' ~ 'family',
      CCGroup %in% c('aphid', 'leafhopper', 'truebugs') ~ 'suborder',
      CCGroup %in% c('bee', 'beetle', 'caterpillar', 'moths', 'daddylonglegs', 'fly', 'grasshopper', 'spider') & !str_detect(CCNotes, '(Elateridae)|(Tingidae)|(Mordellidae)') ~ 'order',
      CCGroup == 'other' & str_detect(CCNotes, '(Psocodea)|(Trichoptera)|(Plecoptera)') ~ 'order',
      str_detect(CCNotes, '(Elateridae)|(Tingidae)|(Mordellidae)') ~ 'family'),
    Taxon = case_when(
      CCGroup == 'ant' ~ 'Formicidae',
      CCGroup == 'aphid' ~ 'Sternorrhyncha',
      CCGroup == 'bee' ~ 'Hymenoptera',
      CCGroup == 'beetle' & !str_detect(CCNotes, '(Elateridae)|(Mordellidae)') ~ 'Coleoptera',
      CCGroup %in% c('caterpillar', 'moths') ~ 'Lepidoptera',
      CCGroup == 'fly' ~ 'Diptera',
      CCGroup == 'spider' ~ 'Araneae',
      CCGroup == 'truebugs' & !str_detect(CCNotes, '(Tingidae)|(Corythucha)')~ 'Heteroptera' ,
      CCGroup == 'other' & str_detect(CCNotes, 'Psocodea') ~ 'Psocodea',
      CCGroup == 'other' & str_detect(CCNotes, 'Trichoptera') ~ 'Trichoptera',
      CCGroup == 'leafhopper' ~ 'Auchenorrhyncha',
      CCGroup == 'daddylonglegs' ~ 'Opiliones',
      CCGroup == 'beetle' & str_detect(CCNotes, 'Elateridae') ~ 'Elateridae',
      CCGroup == 'truebugs' & str_detect(CCNotes, 'Tingidae') ~ 'Tingidae',
      CCGroup == 'other' & str_detect(CCNotes, 'Plecoptera') ~ 'Plecoptera',
      CCGroup == 'beetle' & str_detect(CCNotes, 'Mordellidae') ~ 'Mordellidae'),
    ITISID = rep(NA, nrow(.)),
    TotalMass = rep(NA, nrow(.)))

# conditions to be added to case_when once observed

# CCGroup == 'truebugs' & str_detect(CCNotes, 'Corythucha') ~ 'genus'
# CCGroup == 'truebugs' & str_detect(CCNotes, 'Corythucha') ~ 'Corythucha'

# CCGroup == 'grasshopper' ~ 'Orthoptera',
# CCGroup == 'unidentified' ~ NA

# writing the first set of new surveys
# write.csv(
#   new_surveys,
#   str_c('data/beatsheets_', today(), '.csv'),
#   row.names = F)

# bind and save old and new surveys
bind_rows(my_surveys, new_surveys) %>% 
  write.csv(str_c('data/beatsheets_', today(), '.csv'), row.names = F)

# remove old survey csv
unlink(list.files('data', full.names = T)[str_detect(list.files('data'), 'beatsheets') & !str_detect(list.files('data'), as.character(today()))])

# writing the first set of new arths
# write.csv(
#   new_arths,
#   str_c('data/foliagearths_', today(), '.csv'),
#   row.names = F)

# bind and save old and new arths
bind_rows(my_arths, new_arths) %>% 
  write.csv(str_c('data/foliagearths_', today(), '.csv'), row.names = F)

# remove old arth csv
unlink(list.files('data', full.names = T)[str_detect(list.files('data'), 'foliagearths') & !str_detect(list.files('data'), as.character(today()))])

# update pitfall surveys from google sheet --------------------------------

# read in current pitfall surveys

old_pitfalls <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), 'pitfallsurveys')])

# retrieve the sheet with correct column formatting
all_pitfalls <- drive_get('pitfalls2022') %>% 
  gs4_get() %>% 
  read_sheet('Sheet1') %>% 
  mutate(
    DateDeployed = format(DateDeployed, format = '%Y-%m-%d'),
    TimeDeployed = format(TimeDeployed, format = '%H:%M:%S'),
    DateCollected = format(DateCollected, format = '%Y-%m-%d'),
    TimeCollected = format(TimeCollected, format = '%H:%M:%S')) %>% 
  left_join(
    old_pitfalls %>% 
      select('PitfallID', 'Checks'),
    by = 'PitfallID')

# write the new sheet to a csv
write.csv(
  all_pitfalls,
  str_c('data/pitfallsurveys_', today(), '.csv'),
  row.names = F)

# remove old pitfall survey csv
list.files('data', full.names = T)[str_detect(
  string = list.files('data'),
  pattern = 'pitfallsurveys') & !str_detect(
    string = list.files('data'),
    pattern = as.character(today()))] %>% 
  unlink()
