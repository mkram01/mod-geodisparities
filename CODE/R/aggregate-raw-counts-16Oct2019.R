## Aggregating raw NCHS data to counts by county-year-covariates for smoothing
## This is an update to Kevin's original work to be sure that variables like 
## education are getting counted correctly.
## Goal is to create a single all-purpose dataset for input to INLA that has
## all necessary covariate stratifications. Then can aggregate over unnecessary
## covariates.
## DATE: 10/16/2019
## Author: M. Kramer

library(tidyverse)
library(DBI)
library(sf)
library(sp)
library(spdep)

# Connection to remote db
con <- dbConnect(odbc::odbc(), "SQLServer", server = "sphdbprod", 
                 database = "KRAMER_NCHS")

##################################################################
## Checking variables to identify missing patterns over time    ##
##################################################################

# First looking to align with WONDER 
x0 <- tbl(con, 'nchs_births_trunc') %>%
  mutate(stfips = substr(GEOID, 1, 2)) %>%
  filter(!(stfips %in% c('03', '07', '60', '81', '64', '14', '66', '84', '86',
                         '67','89', '68', '71', '76', '69', '70', '95', '43',
                         '72', '74', '52', '78'))) %>%
  group_by(DOB_YY) %>%
  summarise(count = n()) # After removing territories, counts are perfect!

x1 <- tbl(con, 'nchs_births_trunc') %>%
  mutate(stfips = substr(GEOID, 1, 2)) %>%
  filter(DOB_YY == 2016,
         !(stfips %in% c('03', '07', '60', '81', '64', '14', '66', '84', '86',
                         '67','89', '68', '71', '76', '69', '70', '95', '43',
                         '72', '74', '52', '78'))) %>%
  group_by(DPLURAL) %>%
  summarise(count = n()) # PLURALITY ALSO ALIGNS AFTER REMOVAL OF TERRITORIES


# this is the 'new' EDU var...it appears to be decreasingly missing by year 
# likely due to variable adoption of 2003 revised BC
dbListFields(con, 'nchs_births_trunc')
x <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MEDUC) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(MEDUC, count)

# This is the 'old' EDU var...count of years of education
# It seems to be 100% missing 2009+...not sure if this is about how Kate coded data
# or whether this is NCHS
y <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MDEDUC) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(MDEDUC, count)

# No missing of MAGER9
x <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MAGER9) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(MAGER9, count)

# Substantial missing MAR in 2017
x <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MAR) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(MAR, count)

# Checking combgest and obgest
dd <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY) %>%
  summarise(comb_avg = mean(COMBGEST,  na.rm = T),
            comb_min = min(COMBGEST, na.rm = T),
            comb_max = max(COMBGEST, na.rm = T),
            ob_avg = mean(OBGEST, na.rm = T),
            ob_min = min(OBGEST, na.rm = T),
            ob_max = max(OBGEST, na.rm = T)) %>%
  collect()
  # Looks like obgest doesn't really exist...combgest has values of 99 for missing?

# Checking missing MRACEHISP
dd <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MRACEHISP) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(DOB_YY, count) # note -- something weird starting 2014...I may have combined different vars

dd2 <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MBRACE) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(DOB_YY, count) # bridged race categories shrunk way down in 2014

dd3 <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, MRACE) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(DOB_YY, count) # totally missing after 2013

dd4 <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, UMHISP) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(DOB_YY, count) # totally missing after 2013

# THIS VERSION IS AFTER I RECODED IT IN THE ORIGINAL SAS TO SQL
dd4 <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, HISPRACE) %>%
  summarise(count = n()) %>%
  collect() %>%
  spread(DOB_YY, count) # totally missing after 2013



## PLAN: stratify by age, racehisp, county, year 
## OUTCOMES:  -- all among singletons
  # <32 weeks: very preterm birth -- vptb
  # <34 weeks: early preterm (NCHS) -- eptb
  # 34-36 weeks: late preterm (NCHS) -- lptb
  # <37 weeks: preterm birth -- ptb
  # 37-38 weeks: early term birth -- etb
  # NOTE: etb requires only *term* birth denominator, 37-41 weeks

# Geography notes (e.g. changing FIPS)
# Using this document: https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
# for fixing FIPS

# This defines the 'standard' for geographic units across all time
us <- st_read('DATA/us_counties_2017.gpkg')

# This gets a tabulation of unique GEOID x YEAR
x <- tbl(con, 'nchs_births_trunc') %>%
  group_by(DOB_YY, GEOID) %>%
  summarise() %>%
  collect() 
x2 <- as.data.frame(table(x$GEOID,x$DOB_YY))

sf.unique <- us$GEOID # N=3220 unique counties
nchs.unique <- unique(x$GEOID) # N=3147 unique counties

us2 <- us %>%
  filter(GEOID %in% nchs.unique)

# This is list of FIPS in NCHS that are not balanced across years:
unique(x2$Var1[x2$Freq==0])

# These are counties with NCHS data that are NOT in the spatial file
nchs.unique[which(!(nchs.unique %in% us2$GEOID))]

## These are changes to make to FIPS codes to align NCHS with Geography
# 46113 (was Shannon County SD) should be changed to 46102 (Oglala Lakota County SD)
# 02270 (was Wade Hampton Census Area, AK) should be changed to 02158 (Kusilvak Census Area, AK)
# 02280 (was Petersburg Census Area, AK) should be changed to 02195 (Petersberg Borough)
# 51515 (was Bedford City, VA) should be changed to 51019 (Bedford County, VA)
# 02201 (was Prince of Wales-Outer Ketchikan) should be changed to 02198 (Prince of Wales-Hyder)
# 02232 (was Skagway Hoonah-Angoon) should be changed to 02230 (Skagway Municipality, AK)

##################################################################################
##  Aggregating county data by racehisp, year, county, age                      ##
##  Exclusions: multiple gestation; missing combgest; weight; bw < 500;         ##
##              combgest <21; GEOID not in us counties file                     ##
##################################################################################

df <- tbl(con, 'nchs_births_trunc') %>%
  mutate(stfips = substr(GEOID, 1, 2),
         vptb = ifelse(COMBGEST < 32, 1, 0),
         eptb = ifelse(COMBGEST < 34, 1, 0),
         lptb = ifelse(COMBGEST > 33 & COMBGEST < 37, 1, 0),
         ptb = ifelse(COMBGEST < 37, 1, 0),
         etb = ifelse(COMBGEST == 37 | COMBGEST == 38, 1, 0),
         term = ifelse(COMBGEST > 36 & COMBGEST < 42, 1, 0),
         HISPRACE = ifelse(HISPRACE < 6, 3,
                           ifelse(HISPRACE == 6, 1, 
                                  ifelse(HISPRACE == 7, 2, NA)))) %>%
  filter(!(stfips %in% c('03', '07', '60', '81', '64', '14', '66', '84', '86',
                         '67','89', '68', '71', '76', '69', '70', '95', '43',
                         '72', '74', '52', '78')),
         DPLURAL == 1,
         DBWT > 500,
         COMBGEST != 99,
         COMBGEST > 20,
         !is.na(HISPRACE)) %>%
  group_by(GEOID, DOB_YY, MAGER9, HISPRACE) %>%
  summarise(births = n(),
            vptb = sum(vptb),
            eptb = sum(eptb),
            lptb = sum(lptb),
            ptb = sum(ptb),
            etb = sum(etb),
            term = sum(term)) %>%
  collect() %>%
  ungroup() %>%
  mutate( GEOID = plyr::mapvalues(GEOID,
                  from = c('46113',
                           '02270',
                           '02280',
                           '51515',
                           '02201',
                           '02232'),
                  to = c('46102', 
                         '02158',
                         '02195',
                         '51019',
                         '02198',
                         '02230'))) %>%
  drop_na(GEOID) %>%
  group_by(GEOID, DOB_YY, MAGER9, HISPRACE) %>% # this second time is to incorporate duplicates from recode GEOID
  summarise(births = sum(births),
            vptb = sum(vptb),
            eptb = sum(eptb),
            lptb = sum(lptb),
            ptb = sum(ptb),
            etb = sum(etb),
            term = sum(term)) %>%
  arrange(GEOID, DOB_YY, HISPRACE, MAGER9) %>%
  ungroup() %>%
  data.frame()



df2 <- expand(df, GEOID, DOB_YY, MAGER9, HISPRACE) %>%
  left_join(df, by = c('DOB_YY', 'MAGER9', 'HISPRACE', 'GEOID' ))


## FIXING THE GEOFILE -- THIS NOW HAS COUNTIES ALIGNED WITH NCHS DATA ##
us2 <- us %>%
  filter(GEOID %in% unique(df$GEOID)) %>%
  arrange(GEOID) %>%
  rowid_to_column(var = 'region.ID')

## SAVE
st_write(us2, 'DATA/us_counties_2017.gpkg', delete_layer = T)
us <- st_read('DATA/us_counties_2017.gpkg')

# Final adjustments to data
df3 <- df2 %>%
  left_join(us, by = 'GEOID') %>%
  select(-geom) %>%
  mutate(impute_births = ifelse(is.na(births), 1, 0),
         impute_term = ifelse(is.na(term), 1, 0),
         births = ifelse(births == 0 | is.na(births), 1, births),
         term = ifelse(term == 0 | is.na(term), 1, term),
         MAGER9f = factor(MAGER9,
                          labels = c('under 15', '15-19 yo', '20-24 yo', '25-29 yo', 
                                    '30-34 yo', '35-39 yo', '40-44 yo', '45-49 yo',
                                    '50-54 yo')),
         HISPRACEf = factor(HISPRACE,
                            labels = c('NH White', 'NH Black', 
                            'Hispanic'))) %>%
  replace(., is.na(.), 0) %>%
  data.frame()

saveRDS(df3, 'DATA/nchs_births/R/Data/county-perinatal-2007-2017.rds')


## NOW CREATING AN INLA SPATIAL GRAPH using SPHERE OF INFLUENCE

in.graph1 <- us %>% 
  st_transform(102003) %>%
  as('Spatial') %>%
  coordinates()

# Create triangle neighbor object
us_tri <- tri2nb(in.graph1, row.names = us$GEOID)
us_soi <- graph2nb(soi.graph(us_tri, in.graph1), row.names = us$GEOID)

# create inla graph object
nb2INLA('DATA/nchs_births/R/Data/graphs/us-soi.adj', us_soi)
