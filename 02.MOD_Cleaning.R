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
library(cdlTools)

## Define variables of interest -----------------------
vars <- c("DOB_YY", "REVISION", "MRSTATE", "MRCNTYFIPS", "MAGER9", 
          "DPLURAL", "COMBGEST", "OBGEST_FLG", "DBWT", "MAR", "MEDUC")

# Need to add race/ethnicity variables
# b.	MATERNAL RACE/ETHNICITY – primary interest is in non-Hispanic Black and non-Hispanic White, but secondary interest (and possible modeling) could include Hispanic, Asian/Pacific Islander, and American Indian/Alaska Native, recognizing that each group has relatively specific spatial locations where there are sufficient births
## MBRACE: Maternal Bridged Race - 18 category, single race and bridged multiple races    
## MRACE: Maternal Race - 5 category + additional state-specific codes for API
## UMHISP: Maternal Hispanic Origin - Non-Hispanic, Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Unknown
## MRACEHISP: Maternal Race/Hispanic Origin - Mexican, Puerto Rican, Cuban, Central/South American, Other and Unknown Hispanic, Non-Hispanic White, Non-Hispanic Black, Non-Hispanic Other Races, Unknown

# Temporary files
# births2007 <- readRDS("NCHS Birth Data/R/births2007.rda")
# births2008 <- readRDS("NCHS Birth Data/R/births2008.rda")
# births2009 <- readRDS("NCHS Birth Data/R/births2009.rda")
# births2010 <- readRDS("NCHS Birth Data/R/births2010.rda")
# births2011 <- readRDS("NCHS Birth Data/R/births2011.rda")
# births2012 <- readRDS("NCHS Birth Data/R/births2012.rda")
# births2013 <- readRDS("NCHS Birth Data/R/births2013.rda")
# births2014 <- readRDS("NCHS Birth Data/R/births2014.rda")
# births2015 <- readRDS("NCHS Birth Data/R/births2015.rda")
# births2016 <- readRDS("NCHS Birth Data/R/births2016.rda")

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

## Analyze missingness of variables ------------
table(births$DOB_YY, useNA = "always")
table(births$DOB_YY, births$REVISION, useNA = "always")
table(births$DOB_YY, births$MRSTATE, useNA = "always")
table(births$DOB_YY, births$MRCNTYFIPS, useNA = "always")
table(births$DOB_YY, births$MAGER9, useNA = "always")
table(births$DOB_YY, births$DPLURAL, useNA = "always")
table(births$DOB_YY, births$COMBGEST, useNA = "always")
table(births$DOB_YY, births$OBGEST_FLG, useNA = "always")
table(births$DOB_YY, births$DBWT, useNA = "always")
table(births$DOB_YY, births$MAR, useNA = "always")
table(births$DOB_YY, births$MEDUC, useNA = "always")

## Data Cleaning and Variable Creation -----------------

## EXAMINE VALUES FIRST and DOCUMENTATION FIRST!!!!

# Recode singleton births to singleton or multiple
births$MULTBIRTH <- rep(NA, nrow(births))
births$MULTBIRTH[which(births$DPLURAL == 1)] <- "Singleton"
births$MULTBIRTH[which(births$DPLURAL > 1)] <- "Multiple"

# Create combined FIPS variable
# Transform MRSTATE (abbreviation to FIPS code)
births$MRSTATEFIPS <- as.character(fips(births$MRSTATE, to = "FIPS"))

# Add leading zeroes to one digit state FIPS
for (i in nrow(births)) {
  if (nchar(births$MRSTATEFIPS[i]) == 1) {
    births$MRSTATEFIPS[i] <- paste0("0", births$MRSTATEFIPS[i])
  }
}
# Combined FIPS variable
births$COMBFIPS <- paste0(births$MRSTATEFIPS, births$MRCNTYFIPS)

# Maternal Education (9 category to 3 category)

#MEDUC (3 cat - No HS/GED, GED but no college, some college or higher) - missingness for 1989
## MEDUC: Maternal Education (9 category) <= 8th grade, 9-12, High School/GED, Some college, Associate, Bachelor, Master's, Doctorate, Unknown


## PRETERM BIRTH – these will be recoded into the following categories (note that not mutually exclusive categories, but common reporting categories):
#   1.	20-31 weeks (e.g. < 32) – Very preterm birth
# 2.	32-36 weeks – Moderately preterm birth
# 3.	34-36 weeks – Late preterm birth 
# 4.	
#PTB 20-36 weeks (e.g. <37) – Preterm birth
# IF 22<GESTEST<37 THEN PTB=1; ELSE PTB=0;
# IF 22<GESTEST<32 THEN VPTB=1; ELSE VPTB=0;
# IF 32<=GESTEST<37 THEN MPTB=1; ELSE MPTB=0;
# IF GESTEST IN (32,33) THEN PTB32_33=1; ELSE PTB32_33=0;
# IF 34<=GESTEST<=36 THEN LPTB=1; ELSE LPTB=0;
# 
# Reclassify Race
# IF MRACE=6 THEN RACE=0; 			 *non hisp white;
# IF MRACE in (1,2,3,4,5) THEN RACE=1; *hispanic;
# IF MRACE=7 THEN RACE=2; 			 *non hisp black;
# IF MRACE=8 THEN RACE=3; 			 *non hisp other;
## Census Data --------------------



