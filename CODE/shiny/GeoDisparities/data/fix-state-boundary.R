# Getting smaller version of state boundary from tigris
# M Kramer - 11 Sept 2019

library(tigris)
library(sf)
library(tidyverse)

new_state <- states(cb = T, resolution = '5m', year = 2010, class = 'sf')

state <- readRDS('CODE/shiny/GeoDisparities/data/stateboundaries.Rds') %>%
  select(GEOID10, REGION10, DIVISION10, NAME10) %>%
  st_set_geometry(NULL) %>%
  left_join(new_state, by = c('NAME10' = 'NAME')) %>%
  st_as_sf()


# This overwrote old version...was 9MB now 1.1 MB
saveRDS(state, 'CODE/shiny/GeoDisparities/data/stateboundaries.Rds')
