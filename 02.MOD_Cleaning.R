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

## Define variables of interest -----------------------
vars <- c("DOB_YY", "REVISION", "MRSTATE", "MRCNTYFIPS", "MAGER9", 
          "DPLURAL", "COMBGEST", "OBGEST_FLG", "DBWT", "MAR", "MEDUC")

# Need to add race/ethnicity variables
# b.	MATERNAL RACE/ETHNICITY – primary interest is in non-Hispanic Black and non-Hispanic White, but secondary interest (and possible modeling) could include Hispanic, Asian/Pacific Islander, and American Indian/Alaska Native, recognizing that each group has relatively specific spatial locations where there are sufficient births
## MBRACE: Maternal Bridged Race - 18 category, single race and bridged multiple races    
## MRACE: Maternal Race - 5 category + additional state-specific codes for API
## UMHISP: Maternal Hispanic Origin - Non-Hispanic, Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Unknown
## MRACEHISP: Maternal Race/Hispanic Origin - Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Non-Hispanic White, Non-Hispanic Black, Non-Hispanic Other Races, Unknown


## Create combined file, subset variables ------------------
years <- c(2007:2016)
for (i in seq_along(years)) {
  
  # List files
  fn <- list.files("NCHS Birth Data/R", pattern = as.character(years[i]), full.names = TRUE)
  
  # Read in files
  birthsyear <- readRDS(fn)
  
  # Account for variables not present
  # OBGEST_FLG (Removed in 2014)
  if (is.null(birthsyear$OBGEST_FLG)) {
    birthsyear$OBGEST_FLG <- rep(NA, nrow(birthsyear))
  }
  
  # REVISION (Removed in 2014)
  if (is.null(birthsyear$REVISION)) {
    birthsyear$REVISION <- rep(NA, nrow(birthsyear))
  }
  
  # MRSTATE (Renamed MRSTATEPSTL in 2014)
  if (is.null(birthsyear$MRSTATE)) {
    birthsyear <- rename(birthsyear, MRSTATE = MRSTATEPSTL)
  }
  
  # MAR (Renamed DMAR in 2014)
  if (is.null(birthsyear$MAR)) {
    birthsyear <- rename(birthsyear, MAR = DMAR)
  }
  
  # Subset to vars of interest 
  birthsyear <- birthsyear[, vars] 
  
  #  Restrictions (e.g. only singleton births)
  birthsyear <- birthsyear %>%
    # i.	Any record with Birthweight < 500 grams
    filter(DBWT >= 500) %>%
    
    # ii.	Any records with Gestational Age < 20 weeks
    filter(COMBGEST >= 20) %>%
    
    # iii.	Any record missing County of Residence;
    filter(!(is.na(MRCNTYFIPS))) %>%
    
    # iv.	Any record missing Maternal Race/Ethnicity; 
    # filter(!(is.na(MRCNTYFIPS))) %>%
    
    # v.	Any record missing Plurality; 
    filter(!(is.na(DPLURAL))) %>%
    
    # vi.	Any record missing Gestational Age; 
    filter(!(is.na(COMBGEST)))
  
  
  if (i == 1) {
    births <- birthsyear
  } else {
    births <- rbind(births, birthsyear)
  }
  
  cat("*")
  
  if (i == length(years)) {
    # Output cleaned files
    
    outfile <- paste0("NCHS Birth Data/R/allbirths.rda")
    saveRDS(births, outfile)     
  }
  
}


## Data Cleaning and Variable Creation -----------------
# recode singleton births to singleton or multiple
#MULTBIRTH
# MRCNTYFIPS (combine with state to get 5-digit FIPS)
#COMBFIPS <- paste0(births$MRCNTYFIPS)
#MEDUC (3 cat - No HS/GED, GED but no college, some college or higher) - missingness for 1989
## MEDUC: Maternal Education (9 category) <= 8th grade, 9-12, High School/GED, Some college, Associate, Bachelor, Master's, Doctorate, Unknown
# i.	PRETERM BIRTH – these will be recoded into the following categories (note that not mutually exclusive categories, but common reporting categories):
#   1.	20-31 weeks (e.g. < 32) – Very preterm birth
# 2.	32-36 weeks – Moderately preterm birth
# 3.	34-36 weeks – Late preterm birth 
# 4.	
#PTB 20-36 weeks (e.g. <37) – Preterm birth
#
# Reclassify Race

## Census Data --------------------



## Output final dataset -----------
