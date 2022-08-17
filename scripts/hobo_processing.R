library(tidyverse)

all_data <- map2_dfr(
  list.files('data/hobo_data', full.names = T)[c(1:5,7)],
  c('DF','ERSP','JMNP','NCBG','NCSU','UNC'),
  ~ read_csv(.x, skip = 1) %>% 
    select(2:3) %>% 
    rename('datetime' = 1,
           'tempC' = 2) %>% 
    cbind(site = rep(.y, nrow(.)))) %>% 
  mutate(
    datetime = lubridate::mdy_hms(datetime, tz = Sys.timezone()))

write.csv(all_data, 'data/temperatures.csv')
