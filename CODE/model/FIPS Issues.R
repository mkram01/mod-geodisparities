# FIPS Code Issues
# Mismatched geographies to shapefile (e.g. 170 instead of 166)

## Setup and load packages ---------------------------------
rm(list = ls())
library(knitr)
library(tidyverse) # data management
library(sf) # spatial manipulation
library(sp) # spatial manipulation (adjacency)
library(spdep) # adjacency
library(tmap)  # mapping
library(INLA)  # Mapping
library(magrittr) # Chaining

# Census Divisions
newenglandfips <- c('09', '23', '25', '33', '44', '50')
midatlanticfips <- c('34', '36', '42')
southatlanticfips <- c('10', '11', '12','13', '24', '37', '45', '51', '54')
escfips <- c('01', '21', '28', '47')
wscfips <- c('05', '22', '40', '48')
encfips <- c('17', '18', '26', '39', '55')
wncfips <- c('19', '20', '27', '29', '31', '38', '46')
pacfips <- c('02', '06', '15', '41', '53')
mountainfips <- c('04', '08', '16', '30', '32', '35', '49', '56')

# South Atlantic -----------------------------------
southatlantic <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode %in% c(2,3),
         substr(combfips,1,2) %in% southatlanticfips,
         dob_yy %in% 2007:2016) %>%
  mutate(black = ifelse(racehisp_recode == 3, 1, 0),
         combfips = factor(combfips)) %>%
  group_by(dob_yy, combfips, black) %>%
  summarise(vptb = sum(vptb) + 1,
            ptb = sum(ptb) + 1,
            births = sum(births) + 1)

## Prep spatial data for region only
#Read in national county shapefile and save in MOD folder as `.gpkg`.
cty_sf <- st_read('../../data/spatial/cb_2016_us_county_500k.shp') %>%
  filter(STATEFP %in% southatlanticfips) %>%
  st_transform(102003) # Albers Equal Area
st_write(cty_sf, '../../data/spatial/southatlantic_county.gpkg', delete_dsn = T)

## Read in spatial data
# This is the data saved above. It is saved as `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.

southatlantic_sp <- st_read('../../data/spatial/southatlantic_county.gpkg') %>%
  inner_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
            ptb = sum(ptb),
            births = sum(births),
            rawvptb = vptb / births * 1000,
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

a <- as.data.frame(table(southatlantic$combfips))
nomatch_southatlantic <- a$Var1[which(!(a$Var1 %in% southatlantic_sp$GEOID))]
bad_southatlantic <- southatlantic[which(southatlantic$combfips %in% nomatch_southatlantic), ]


# West North Central -----------------------------------
wnc <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode %in% c(2,3),
         substr(combfips,1,2) %in% wncfips,
         dob_yy %in% 2007:2016) %>%
  mutate(black = ifelse(racehisp_recode == 3, 1, 0),
         combfips = factor(combfips)) %>%
  group_by(dob_yy, combfips, black) %>%
  summarise(vptb = sum(vptb) + 1,
            ptb = sum(ptb) + 1,
            births = sum(births) + 1)

## Prep spatial data for region only
#Read in national county shapefile and save in MOD folder as `.gpkg`.
cty_sf <- st_read('../../data/spatial/cb_2016_us_county_500k.shp') %>%
  filter(STATEFP %in% wncfips) %>%
  st_transform(102003) # Albers Equal Area
st_write(cty_sf, '../../data/spatial/wnc_county.gpkg', delete_dsn = T)

## Read in spatial data
# This is the data saved above. It is saved as `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.

wnc_sp <- st_read('../../data/spatial/wnc_county.gpkg') %>%
  inner_join(wnc, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
            ptb = sum(ptb),
            births = sum(births),
            rawvptb = vptb / births * 1000,
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

a <- as.data.frame(table(wnc$combfips))
nomatch_wnc <- a$Var1[which(!(a$Var1 %in% wnc_sp$GEOID))]
bad_wnc <- wnc[which(wnc$combfips %in% nomatch_wnc), ]

# Pacific -----------------------------
pac <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode %in% c(2,3),
         substr(combfips,1,2) %in% pacfips,
         dob_yy %in% 2007:2016) %>%
  mutate(black = ifelse(racehisp_recode == 3, 1, 0),
         combfips = factor(combfips)) %>%
  group_by(dob_yy, combfips, black) %>%
  summarise(vptb = sum(vptb) + 1,
            ptb = sum(ptb) + 1,
            births = sum(births) + 1)

## Prep spatial data for region only
#Read in national county shapefile and save in MOD folder as `.gpkg`.
cty_sf <- st_read('../../data/spatial/cb_2016_us_county_500k.shp') %>%
  filter(STATEFP %in% pacfips) %>%
  st_transform(102003) # Albers Equal Area
st_write(cty_sf, '../../data/spatial/pac_county.gpkg', delete_dsn = T)

## Read in spatial data
# This is the data saved above. It is saved as `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.

pac_sp <- st_read('../../data/spatial/pac_county.gpkg') %>%
  inner_join(pac, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
            ptb = sum(ptb),
            births = sum(births),
            rawvptb = vptb / births * 1000,
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

a <- as.data.frame(table(pac$combfips))
nomatch_pac <- a$Var1[which(!(a$Var1 %in% pac_sp$GEOID))]
bad_pac <- pac[which(pac$combfips %in% nomatch_pac), ]

# Merge issues together
allbad <- rbind(bad_southatlantic, bad_wnc, bad_pac)
write.csv(allbad, "FIPS Matching Issues.csv")
