## Define standard geography for MoD Geodisparities Mapper modeling
## M. Kramer - 10/16/2019

library(tigris)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

# US County boundaries for 2017
us <- counties(cb = T, resolution = '20m', year = 2017, class = 'sf') %>%
  select(GEOID, NAME)

st_write(us, 'DATA/us_counties_2017.gpkg', delete_layer = T)
