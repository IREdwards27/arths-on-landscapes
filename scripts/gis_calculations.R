
# setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(lubridate)

circles <- read_csv('data/circles_2022-05-13.csv')

roads <- st_read('data/gis_data/NCRoutes.shp') %>% 
  st_zm()

circles_sf <- st_as_sf(
  circles,
  coords = c('Longitude','Latitude'),
  crs = st_crs(4326)) %>% 
  st_transform(crs = st_crs(roads))

area_roads <- st_crop(
  roads,
  y = st_bbox(
    obj = c(
      xmin = as.numeric(st_bbox(circles_sf)$xmin - 10000),
      xmax = as.numeric(st_bbox(circles_sf)$xmax + 10000),
      ymin = as.numeric(st_bbox(circles_sf)$ymin - 10000),
      ymax = as.numeric(st_bbox(circles_sf)$ymax + 10000)),
    crs = st_crs(roads)))

ggplot(area_roads) +
  geom_sf() +
  geom_sf(
    mapping = aes(color = 'red'),
    data = circles_sf)

circles_edged <- circles %>% 
  cbind(
    DistanceToEdgem = as.numeric(st_distance(
      circles_sf,
      st_union(area_roads)))*0.3048)

write_csv(circles_edged, str_c('data/circles_', today(), '.csv'))
