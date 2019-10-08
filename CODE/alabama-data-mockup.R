## Update data for Alabama presentation
## Using VLBW only from INLA runs with county covariates from PMSS

# Looks like i need a variable 'year', and 'state_name'

library(tidyverse)
library(sf)

vlbw <- readRDS('C:/Users/mkram02/Box Sync/VLBW-Izzy/inla-vlbw-long-2aug.rds') %>%
  filter(RACETH != 3,
         YEAR > 2010 & YEAR < 2017) %>%
  dplyr::select(FIPS, YEAR, RACETH, inla_rate) %>%
  spread(RACETH, inla_rate) %>%
  rename(year = YEAR, 
         GEOID = FIPS,
         Black = `2`,
         White = `1`) %>%
  mutate(BW_rr = Black / White,
         BW_rd = Black - White)


pmss <- read_csv("C:/Users/mkram02/Box Sync/create-county-measures/DATA/pmss-county-indicators.csv") %>%
  mutate(state_name = str_split(NAME, ', ', simplify = T)[,2]) %>%
  rename(year = YEAR) %>%
  filter(year > 2010)

a <- readRDS('GeoDisparities/data/alldata.Rds')
us <- readRDS('GeoDisparities/data/alldata.Rds') %>% 
  filter(year == 2016) %>%
  dplyr::select(GEOID)

newdata <- vlbw %>%
  left_join(pmss, by = c('GEOID', 'year')) %>%
  left_join(us, by = 'GEOID') %>%
  as.data.frame() %>%
  st_as_sf()

saveRDS(newdata, 'GeoDisparities/data/newdata.Rds')




a <- readRDS('GeoDisparities/data/alldata.Rds')
