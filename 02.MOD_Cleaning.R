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
## Base = 36 variables
## lrecl
## REVISION: A (2003) or S (1989) birth certificate
## DOB_YY: Birth year
## DOB_MM: Birth month
## OSTATE: Occurrence Postal State   
## OCNTYFIPS: County FIPS (3-digit) -- will need to add state FIPS for linking 
## MAGER: Maternal age, 12 - 50 years, 10-12 classified as 12 , 50 includes 50-64     
## MAGER9: Maternal age recode: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50-64
## MBCNTRY: Maternal birth country (2 letter)
## UMBSTATE: Maternal Postal Birth State - 1 letter
## mbstate: Maternal Postal Birth State (revised) - 2 letter
## MBSTATE_REC
## MRSTATE
## MRCNTYFIPS
## MBRACE      
## MRACE
## UMHISP
## MRACEHISP
## MAR_P
## MAR
## MEDUC
## FAGECOMB
## UFAGECOMB
## FBRACE
## FRACEREC    
## FRACE
## UFHISP
## FRACEHISP
## LBO_REC
## TBO_REC
## UPREVIS
## CIG_REC
## DPLURAL
## COMBGEST
## OBGEST
## DBWT" 
# 
## 2007 has additional variables: XOSTATE, MDEDUC, ALCOHOL
## 2008-2010 have 2 additional variables (38) - XOSTATE, MDEDUC, ALCOHOL, lost lrecl
## Other comments:
## lrecl always has the same value across all rows in a dataset
## OSTATE (occurrence postal state) and XOSTATE (expanded occurence postal state) do not always have the same values

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
