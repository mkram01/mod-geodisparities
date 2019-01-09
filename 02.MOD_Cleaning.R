# March of Dimes Analysis #
# 02: Cleaning Script     #
# Kevin Weiss             #
# 01/07/2019              #

## Package setup -----------------
rm(list = ls())
# package_list <- c("sas7bdat", "haven", "Hmisc", "feather", "dplyr")
# for(package in package_list) {
#   library(package, lib.loc = , character.only = TRUE)
# }
library(sas7bdat)
library(haven)
library(Hmisc)
library(feather)
library(dplyr)
library(magrittr)

## Read in data ------------------
# years <- c(2007:2016)
# for (i in seq_along(years)) {
#   # List file
#   fn <- list.files("NCHS Birth Data/R", pattern = as.character(years[i]), full.names = TRUE)
#   # Read in files
#   readRDS(fn)
#   cat("*")
# }
births2007 <- readRDS("NCHS Birth Data/R/births2007.rda")
births2008 <- readRDS("NCHS Birth Data/R/births2008.rda")
births2009 <- readRDS("NCHS Birth Data/R/births2009.rda")
births2010 <- readRDS("NCHS Birth Data/R/births2010.rda")
births2011 <- readRDS("NCHS Birth Data/R/births2011.rda")
births2012 <- readRDS("NCHS Birth Data/R/births2012.rda")
births2013 <- readRDS("NCHS Birth Data/R/births2013.rda")
births2014 <- readRDS("NCHS Birth Data/R/births2014.rda")
births2015 <- readRDS("NCHS Birth Data/R/births2015.rda")
births2016 <- readRDS("NCHS Birth Data/R/births2016.rda")
# 
# Feather
# write_feather("feather/births2007")
# write_feather("feather/births2008")
# write_feather("feather/births2009")
# write_feather(births2010, "feather/births2010")
# write_feather(births2011, "feather/births2011")
# write_feather(births2012, "feather/births2012")
# write_feather(births2013, "feather/births2013")
# write_feather(births2014, "feather/births2014")
# write_feather(births2015, "feather/births2015")
# write_feather(births2016, "feather/births2016")

## Data Description -------------------------
## Based on 2007 data - some years and vars may require cleaning
## Some data may be present only for the 1989 certificate or the 2003 certificate
## Base = 36 variables
## lrecl: Value is either 1500 or 775
## REVISION: A (2003) or S (1989) birth certificate
## DOB_YY: Birth year - Identical within years
## DOB_MM: Birth month - 12 months
## OSTATE: Occurrence Postal State - 50 states
## OCNTYFIPS: County FIPS (3-digit) -- will need to add state FIPS for linking 
## MAGER: Maternal age, 12 - 50 years, 10-12 classified as 12 , 50 includes 50-64     
## MAGER9: Maternal age recode (9 category) - <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-64
## MBCNTRY: Maternal birth country (2 letter)
## UMBSTATE: Maternal Postal Birth State - 1 letter (Blank in 2007)
## mbstate: Maternal Postal Birth State (revised) - 2 letter
## MBSTATE_REC (3 category) - Unsure what these refer to ***********
## MRSTATE: Maternal Residence Postal State (includes non-states)
## MRCNTYFIPS: Maternal Residence County FIPS
## MBRACE: Maternal Bridged Race - 18 category, single race and bridged multiple races     
## MRACE: Maternal Race - 5 category + additional state-specific codes for API
## UMHISP: Maternal Hispanic Origin - Non-Hispanic, Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Unknown
## MRACEHISP: Maternal Race/Hispanic Origin - Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Non-Hispanic White, Non-Hispanic Black, Non-Hispanic Other Races, Unknown
## MAR_P: Paternity Acknowledged (Blank in 2007): Yes, No, Unknown, NA
## MAR: Maternal Marital Status - Narried, Unmarried, Unknown
## MEDUC: Maternal Education (9 category) <= 8th grade, 9-12, High School/GED, Some college, Associate, Bachelor, Master's, Doctorate, Unknown
## FAGECOMB: Paternal Combined Age (Revised) - 9 - 98, 99 = unknown
## UFAGECOMB: Paternal Combined Age (Revised) - 10 - 98, 99 = unknown
## FBRACE: Paternal Bridged Race - 18 category, single race and bridged multiple races
## FRACEREC: Paternal Race Recode - White, Black, AI/AN, A/PI, Unknown
## FRACE: Paternal Race - 5 category + additional state-specific codes for API
## UFHISP: Paternal Hispanic Origin - Non-Hispanic, Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Unknown
## FRACEHISP: Paternal Race/Hispanic Origin - Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Non-Hispanic White, Non-Hispanic Black, Non-Hispanic Other Races, Unknown
## LBO_REC: Live Birth Order Recode - 1-7, 8 or more, Unknown
## TBO_REC: Total Birth Order Recode - 1-7, 8 or more, Unknown
## UPREVIS: Number of prenatal visits - 0-49, 99 = unknown
## CIG_REC: Cigarette Recode - Yes, No, Unknown
## DPLURAL: Plurality, recoded: Single, Twin, Triplet, Quadruplet, Quintuplet or higher
## COMBGEST: Gestation - Detail in Weeks - 17-47, 99 = Unknown
## OBGEST_FLG: Clinical Estimate of Gestation Used Flag - 1= Clinical estimate used
## DBWT: Birth Weight - Detail in Grams
#
## 2007 has additional variables: XOSTATE, MDEDUC, ALCOHOL
## XOSTATE: Expanded Occurrence Postal State
## MDEDUC: Not in documentation, 18 category + 99
## ALCOHOL: Not in documentation, Blank in 2007
## 2008-2010 have 2 additional variables (38) - XOSTATE, MDEDUC, ALCOHOL, lost lrecl

## Subset data -------------------
## Provide list of variables across years
vars <- c()

## Restrictions (e.g. only singleton births)
## Exclude births to foreign residents (RESTATUS)
## Singleton births only (DPLURAL)


df <- births %>%
  filter(RESTATUS != 4) %>%
  filter(DPLURAL < 2)


## Data Cleaning -----------------



## Census Data --------------------



## Output final dataset -----------
