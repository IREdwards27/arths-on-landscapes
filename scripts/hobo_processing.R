library(tidyverse)
library(lubridate)

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

temps <- read_csv('data/temperatures.csv') %>% 
  mutate(datetime = mdy_hm(datetime)) %>% 
  mutate(
    daynight = if_else(
      hour(datetime) %in% c(0:6,21:24),
      true = 'night',
      false = 'day'))

sites <- read_csv(
  list.files('data', full.names = T)[str_detect(list.files('data'), '^sites')])

site_temps <- temps %>% 
  group_by(site, daynight) %>% 
  summarize(mean_temp = mean(tempC)) %>% 
  pivot_wider(
    names_from = daynight,
    values_from = mean_temp) %>% 
  rename(
    mn_day_temp = day,
    mn_night_temp = night) %>% 
  left_join(
    sites,
    by = c('site' = 'SiteID'))

write.csv(site_temps, paste('data/sites_', today(), '.csv'))

