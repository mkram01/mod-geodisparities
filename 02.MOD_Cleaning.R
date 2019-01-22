# March of Dimes Analysis #
# 02: Cleaning Script     #
# Kevin Weiss             #
# 01/07/2019              #

# Questions
# Subset to just US data (rather than PR, Guam, etc...)?
# 
# NEED to Complete
# Missingness by state and by year
# Census FIPS cleaning - consistency of codes

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
library(plyr)
library(magrittr)
library(cdlTools)

# Temporary by-year files
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

## Define variables of interest -----------------------
vars <- c("DOB_YY", "REVISION", "MRSTATE", "MRCNTYFIPS", "MAGER9", 
          "DPLURAL", "COMBGEST", "OBGEST_FLG", "DBWT", "MAR", "MEDUC",
          "RACEHISP_RECODE")

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

  # Race 2007-2013 (MHISP_R not present in 2014-2016)
  if (is.null(birthsyear$MHISP_R)) {
    
    # Account for different spelling in name for 2013
    if (is.null(birthsyear$MRACEHISP) & is.null(birthsyear$MHISP_R)) {
      birthsyear <- rename(birthsyear, MRACEHISP = MRAACEHISP)
    }
    
    # Revised Coding for MBRACE
    # 1 = White, 2 = Black, # 3 = AI/AN, 4 = A/PI
    # MRACE covers states reporting only single race
    # MBRACE covers states reporting multiple races
    # Together, MRACE + MBRACE cover all

    birthsyear$MBRACE[which(birthsyear$MBRACE %in% c(1, 21) | birthsyear$MRACE == 1)] <- 1 #White (single or multiple)
    birthsyear$MBRACE[which(birthsyear$MBRACE %in% c(2, 22) | birthsyear$MRACE == 2)] <- 2 #Black (single or multiple)
    birthsyear$MBRACE[which(birthsyear$MBRACE %in% c(3, 23) | birthsyear$MRACE == 3)] <- 3 #AI/AN (single or multiple)
    birthsyear$MBRACE[which(birthsyear$MBRACE %in% c(4:14, 24) | birthsyear$MRACE %in% c(4:7, 18, 28, 38, 48, 58))] <- 4 #A/PI (single or multiple)

    # Race/Hisp Recode
    # 1 = Hispanic, 2 = NHW, 3 = NHB, 4 = NHAI/AN, 5 = NHA/PI, 9 = UNK
    # Recommended recode (Adapted/Edited from 2014-2016 Natality Data Use File)
    # if mhisp_r = 1:5, then recode = Hisp
    # if mhisp_r = 0 and mbrace = 1, then NHW
    # if mhisp_r = 0 and mbrace = 2, then NHB
    # if mhisp_r = 0 and mbrace = 3:4, then NWOther (I split into our other categories)
    # if mhisp_r = 9 then unknown or not stated
    
    birthsyear$RACEHISP_RECODE <- rep(NA, nrow(birthsyear))
    birthsyear$RACEHISP_RECODE[which(birthsyear$UMHISP %in% c(1:5))] <- 1 #Hispanic
    birthsyear$RACEHISP_RECODE[which(birthsyear$UMHISP == 0 & birthsyear$MBRACE == 1)] <- 2 #Non-Hispanic White
    birthsyear$RACEHISP_RECODE[which(birthsyear$UMHISP == 0 & birthsyear$MBRACE == 2)] <- 3 #Non-Hispanic Black
    birthsyear$RACEHISP_RECODE[which(birthsyear$UMHISP == 0 & birthsyear$MBRACE == 3)] <- 4 #Non-Hispanic AI/AN
    birthsyear$RACEHISP_RECODE[which(birthsyear$UMHISP == 0 & birthsyear$MBRACE == 4)] <- 5 #Non-Hispanic A/PI
    birthsyear$RACEHISP_RECODE[which(birthsyear$UMHISP == 9)] <- 9 #Unknown/Not Stated
    
  }
    
  # Race 2014-2016 (MRACE not present in 2014-2016)
  if (is.null(birthsyear$MRACE)) {

    # Race/Hisp Recode
    # 1 = Hispanic, 2 = NHW, 3 = NHB, 4 = NHAI/AN, 5 = NHA/PI, 9 = UNK
    # Recommended recode (Adapted/edited from 2014-2016 Natality Data Use File)
    # if mhisp_r = 1:5, then recode = Hisp
    # if mhisp_r = 0 and mbrace = 1, then NHW
    # if mhisp_r = 0 and mbrace = 2, then NHB
    # if mhisp_r = 0 and mbrace = 3:4, then NWOther (I split into our other categories)
    # if mhisp_r = 9 then unknown or not stated
         
    birthsyear$RACEHISP_RECODE <- rep(NA, nrow(birthsyear))
    birthsyear$RACEHISP_RECODE[which(birthsyear$MHISP_R %in% c(1:5))] <- 1 #Hispanic
    birthsyear$RACEHISP_RECODE[which(birthsyear$MHISP_R == 0 & birthsyear$MBRACE == 1)] <- 2 #Non-Hispanic White
    birthsyear$RACEHISP_RECODE[which(birthsyear$MHISP_R == 0 & birthsyear$MBRACE == 2)] <- 3 #Non-Hispanic Black
    birthsyear$RACEHISP_RECODE[which(birthsyear$MHISP_R == 0 & birthsyear$MBRACE == 3)] <- 4 #Non-Hispanic AI/AN
    birthsyear$RACEHISP_RECODE[which(birthsyear$MHISP_R == 0 & birthsyear$MBRACE == 4)] <- 5 #Non-Hispanic A/PI
    birthsyear$RACEHISP_RECODE[which(birthsyear$MHISP_R == 9)] <- 9 #Unknown/Not Stated
    
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

## Analyze missingness of variables (alone and by year) ------------
table(births$DOB_YY, useNA = "always") # 0 missing
table(births$MRSTATE, useNA = 'always') # 0 missing
table(births$MAGER9, useNA = 'always') # 0 missing
table(births$DPLURAL, useNA = 'always') # 0 missing
table(births$MAR, useNA = 'always') # 0 missing
table(births$REVISION, useNA = 'always') #11,923,219 missing
table(births$MEDUC, useNA = 'always') # 7,259,558 missing
table(births$OBGEST_FLG, useNA = "always") #38,806,521 missing
table(births$RACEHISP_RECODE, useNA = 'always') #137,096 missing
table(is.na(births$COMBGEST)) # 0 missing
table(is.na(births$DBWT)) # 0 missing
table(is.na(births$MRCNTYFIPS)) # 0 missing

# Missigness by year
table(births$DOB_YY, is.na(births$REVISION)) #all values missing in 2014-2016, none missing before that
table(births$DOB_YY, is.na(births$MEDUC)) # Missing values in all years except 2019
#        FALSE    TRUE
# 2007 2384308 1932058
# 2008 2756756 1491044
# 2009 2812175 1318574
# 2010 3095921  904466
# 2011 3392755  561796
# 2012 3489774  464290
# 2013 3558324  375701
# 2014 3848781  142690
# 2015 3913161   68939
# 2016 3949648       0
table(births$DOB_YY, is.na(births$OBGEST_FLG)) # Missing values in all years (exclusively missing in 2012, 2015, 2016)
#        FALSE    TRUE
# 2007  250059 4066307
# 2008  264480 3983320
# 2009  253095 3877654
# 2010  239613 3760774
# 2011  229175 3725376
# 2012       0 3954064
# 2013  213613 3720412
# 2014  204605 3786866
# 2015       0 3982100
# 2016       0 3949648
table(births$DOB_YY, is.na(births$RACEHISP_RECODE)) # No values missing in 2007-2013
# 2007 4280757   35609
# 2008 4218938   28862
# 2009 4103481   27268
# 2010 3981201   19186
# 2011 3944566    9985
# 2012 3945121    8943
# 2013 3926782    7243
# 2014 3991471       0
# 2015 3982100       0
# 2016 3949648       0

# Missingness by revision
table(births$REVISION, is.na(births$MEDUC)) # All are missing on S birth certs, some missing revision values
#      FALSE     TRUE
# A 21490013        0
# S        0  7047929
table(births$REVISION, is.na(births$OBGEST_FLG)) # Missing on both birth certs
#      FALSE     TRUE
# A  1157668 20332345
# S   292367  6755562
table(births$REVISION, is.na(births$RACEHISP_RECODE)) # Missing only on S birth cert
#       FALSE     TRUE
# A 21490013        0
# S  6910833   137096# 
 
# Missingness by state and by year
table(births$MRSTATE, is.na(births$MEDUC))
table(births$MRSTATE, is.na(births$OBGEST_FLG))
table(births$MRSTATE, is.na(births$RACEHISP_RECODE))

## Data Cleaning and Variable Creation -----------------
## Recode singleton births to singleton or multiple
births$PLURALITY <- rep(NA, nrow(births))
births$PLURALITY[which(births$DPLURAL == 1)] <- "Singleton"
births$PLURALITY[which(births$DPLURAL > 1)] <- "Multiple"

## Create combined FIPS variable
# Transform MRSTATE (abbreviation to FIPS code)
births$MRSTATEFIPS <- rep(NA, nrow(births))
births$MRSTATEFIPS <- as.character(fips(births$MRSTATE, to = "FIPS"))

# Add leading zeroes to one digit state FIPS
for (i in nrow(births)) {
  if (nchar(births$MRSTATEFIPS[i]) == 1) {
    births$MRSTATEFIPS[i] <- paste0("0", births$MRSTATEFIPS[i])
  }
}
# Combined FIPS variable
births$COMBFIPS <- paste0(births$MRSTATEFIPS, births$MRCNTYFIPS)

## Maternal Education (9 category to 3 category)
# Recode Recode: 1 = No HS/GED, 2 = GED but no college 3 = some college or higher
births$MEDUC_R <- rep(NA, nrow(births))
births$MEDUC_R[which(births$MEDUC %in% c(1:2) )] <- 1
births$MEDUC_R[which(births$MEDUC == 3)] <- 2
births$MEDUC_R[which(births$MEDUC %in% c(4:8))] <- 3
ncol(births)
births <- births[, -"MEDUC"]
ncol(births)

## Pre-Term Birth
# Following Monica Recode
births$PTB <- rep(0, nrow(births))
births$VPTB <- rep(0, nrow(births))
births$MPTB <- rep(0, nrow(births))
births$LPTB <- rep(0, nrow(births))
births$PTB[which(births$COMBGEST < 37 & births$COMBGEST >= 20)] <- 1 #[20, 37)
births$VPTB[which(births$COMBGEST < 32 & births$COMBGEST >= 20)] <- 1 #[20, 32)
births$MPTB[which(births$COMBGEST < 37 & births$COMBGEST >= 32)] <- 1 #[32, 37)
births$LPTB[which(births$COMBGEST < 37 & births$COMBGEST >= 34)] <- 1 #[34, 37)

outfile <- paste0("NCHS Birth Data/R/allbirths_newvars.rda")
saveRDS(births, outfile)  

## Output aggregated datasets for analysis ------------------------------------------------
## Counts of each preterm birth outcome plus count of total birth in strata

## YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
df1 <- births %>%
  
  # Restrict to NH-Black and NH-White
  filter(RACEHISP_RECODE == 2 | 
           RACEHISP_RECODE == 3) %>%
  
  # Group by Year, County, Race
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE) %>%

  # Summarize preterm birth
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY))))
  
  # Restrict to singleton births
  filter(PLURALITY == "singleton") %>%
  
## YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
df2 <- births %>%
    
  # # Restrict to NH-Black and NH-White
  # filter(RACEHISP_RECODE == 2 | 
  #          RACEHISP_RECODE == 3) %>%
  #          
  # Restrict to singleton births
  filter(PLURALITY == "singleton") %>%
    
  # Group by Year, County, Race, Age Cat
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE, MAGER9) %>%
  
  # Summarize preterm birth  
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY))))   
  
## YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS 
df3 <- births %>%
  
  # # Restrict to NH-Black and NH-White
  # filter(RACEHISP_RECODE == 2 | 
  #          RACEHISP_RECODE == 3) %>%
  
  # Restrict to singleton births
  filter(PLURALITY == "singleton") %>%
  
  # Group by Year, County, Race, Age Cat, Marital, Education
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE, MAGER9, MAR, MEDUC_R) %>%
  
  # Summarize preterm birth  
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY)))) 

## Now re-run including multiple births
## YEAR x COUNTY x RACE restricted to NH-Black/NH-White
df4 <- births %>%
  
  # Restrict to NH-Black and NH-White
  filter(RACEHISP_RECODE == 2 | 
           RACEHISP_RECODE == 3) %>%
  
  # Group by Year, County, Race
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE) %>%
  
  # Summarize preterm birth   
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY)))) 

## YEAR x COUNTY x RACE x AGE
df5 <- births %>%
  
  # # Restrict to NH-Black and NH-White
  # filter(RACEHISP_RECODE == 2 | 
  #          RACEHISP_RECODE == 3) %>%
  
  # Group by Year, County, Race, Age Cat,
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE, MAGER9) %>%
  
  # Summarize preterm birth   
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY)))) 

## YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION
df6 <- births %>%
  
  # Restrict to NH-Black and NH-White
  # filter(RACEHISP_RECODE == 2 | 
  # RACEHISP_RECODE == 3) %>%
  
  # Group by Year, County, Race, Age Cat, Marital, Education
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE, MAGER9, MAR, MEDUC_R) %>%
  
  # Summarize preterm birth      
  summarise(vptb = sum(VPTB),
                  lptb = sum(LPTB),
                  mptb = sum(MPTB),
                  ptb = sum(PTB),
                  births = sum(!(is.na(DOB_YY)))) 

  
