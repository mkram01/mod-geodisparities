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
library(tidyr)
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
      # FALSE    TRUE
# AB      44       0
# AK   45952   67235
# AL  189698  414944
# AR  129219  261061
# AS      12       0
# AZ  260722  635090
# BC     178       0
# CA 5130628    2895
# CO  668802     339
# CT   41444  334417
# DC   72569   20256
# DE  112246     887
# FL 2210853    2809
# GA 1258633   95411
# GU      50       8
# HI   55295  132888
# IA  391365    2210
# ID  231638     999
# IL 1117985  521072
# IN  847038    1800
# KS  396678    3831
# KY  562093    2354
# LA  383441  253902
# MA  418781  315154
# MB      64       0
# MD  514922  226106
# ME   45361   84880
# MI 1057005  100248
# MN  408582  291645
# MO  534041  235127
# MP       7       0
# MS  162501  243261
# MT  110797   12299
# NB      35       0
# NC  749124  480917
# ND   99307     728
# NE  264155      87
# NH  123525    5061
# NJ  219850  846386
# NM  244546   30032
# NS       4       0
# NV  271566   95834
# NY 2273602  149081
# OH 1401070   11571
# OK  408114  126003
# ON     400       0
# OR  414219   48553
# PA 1421421   14548
# PR      99      20
# QC     102       0
# RI   22940   89148
# SC  580329    7892
# SD  120721     282
# SK      13       0
# TN  814161    2494
# TX 3940588    2873
# UT  411803  110535
# VA  495306  543085
# VI      93      12
# VT   60792     188
# WA  882266    1116
# WI  403065  282780
# WV   68123  138402
# WY   76205     552
# XX   74021    8001
# ZZ    1394     249
table(births$MRSTATE, is.na(births$OBGEST_FLG))
      # FALSE    TRUE
# AB       2      42
# AK    5045  108142
# AL   18855  585787
# AR   14574  375706
# AS       0      12
# AZ   11538  884274
# BC      29     149
# CA   95879 5037644
# CO   27998  641143
# CT    7528  368333
# DC    6280   86545
# DE    4649  108484
# FL  123524 2090138
# GA  217204 1136840
# GU       4      54
# HI   12650  175533
# IA    9636  383939
# ID    3647  228990
# IL   91882 1547175
# IN   12847  835991
# KS    9065  391444
# KY   29557  534890
# LA   17703  619640
# MA   20722  713213
# MB       2      62
# MD   29571  711457
# ME    3934  126307
# MI   42353 1114900
# MN   43351  656876
# MO   32522  736646
# MP       0       7
# MS   14198  391564
# MT    8502  114594
# NB       3      32
# NC   47249 1182792
# ND    7403   92632
# NE   14746  249496
# NH    9327  119259
# NJ   11530 1054706
# NM   13436  261142
# NS       1       3
# NV   24910  342490
# NY   56319 2366364
# OH  101592 1311049
# OK   18762  515355
# ON      41     359
# OR   18886  443886
# PA  123147 1312822
# PR      10     109
# QC       6      96
# RI    8426  103662
# SC   19965  568256
# SD    3459  117544
# SK       0      13
# TN   48259  768396
# TX   60840 3882621
# UT   16958  505380
# VA   11937 1026454
# VI       5     100
# VT    4330   56650
# WA   68212  815170
# WI   38711  647134
# WV    5043  201482
# WY    4129   72628
# XX    1626   80396
# ZZ     121    1522
table(births$MRSTATE, is.na(births$RACEHISP_RECODE))
# FALSE    TRUE
# AB      44       0
# AK  110036    3151
# AL  599955    4687
# AR  386325    3955
# AS      12       0
# AZ  877653   18159
# BC     178       0
# CA 5133448      75
# CO  669137       4
# CT  363180   12681
# DC   92570     255
# DE  113100      33
# FL 2213621      41
# GA 1351014    3030
# GU      58       0
# HI  188179       4
# IA  393567       8
# ID  232634       3
# IL 1633050    6007
# IN  848829       9
# KS  400446      63
# KY  564436      11
# LA  633468    3875
# MA  717613   16322
# MB      64       0
# MD  731171    9857
# ME  128974    1267
# MI 1157243      10
# MN  700208      19
# MO  766750    2418
# MP       7       0
# MS  403992    1770
# MT  123038      58
# NB      35       0
# NC 1216392   13649
# ND  100034       1
# NE  264241       1
# NH  128421     165
# NJ 1060835    5401
# NM  274214     364
# NS       4       0
# NV  363716    3684
# NY 2421056    1627
# OH 1412612      29
# OK  532966    1151
# ON     400       0
# OR  460695    2077
# PA 1435895      74
# PR     117       2
# QC     102       0
# RI  111186     902
# SC  588073     148
# SD  121003       0
# SK      13       0
# TN  816632      23
# TX 3943414      47
# UT  522334       4
# VA 1028894    9497
# VI     105       0
# VT   60979       1
# WA  883345      37
# WI  676049    9796
# WV  205944     581
# WY   76755       2
# XX   81968      54
# ZZ    1636       7

# By State and Year
df <- as.data.frame(table(births$MRSTATE, births$DOB_YY, is.na(births$MEDUC)))
df <- df[which(df$Var3 == TRUE & df$Freq > 0), ]
df <- spread(df, key = Var1, value = Freq)
write.csv(df, "NCHS Birth Data/R/MEDUC_missing.csv")

df2 <- as.data.frame(table(births$MRSTATE, births$DOB_YY, is.na(births$OBGEST_FLG)))
df2 <- df2[which(df2$Var3 == TRUE & df2$Freq > 0), ]
df2 <- spread(df2, key = Var1, value = Freq)
write.csv(df2, "NCHS Birth Data/R/OBGEST_missing.csv")

df3 <- as.data.frame(table(births$MRSTATE, births$DOB_YY, is.na(births$RACEHISP_RECODE)))
df3 <- df3[which(df3$Var3 == TRUE & df3$Freq > 0), ]
df3 <- spread(df3, key = Var1, value = Freq)
write.csv(df3, "NCHS Birth Data/R/RACEHISP_missing.csv")

df4 <- as.data.frame(table(births$MRSTATE, births$DOB_YY, births$REVISION))
df4 <- df[which(df$Var3 == TRUE & df$Freq > 0), ]
df4 <- spread(df, key = Var1, value = Freq)
write.csv(df, "NCHS Birth Data/R/MEDUC_missing.csv")

## Data Cleaning and Variable Creation -----------------
## Recode singleton births to singleton or multiple
births$PLURALITY <- rep(NA, nrow(births))
births$PLURALITY[which(births$DPLURAL == 1)] <- "Singleton"
births$PLURALITY[which(births$DPLURAL > 1)] <- "Multiple"

## Maternal Education (9 category to 3 category)
# Recode Recode: 1 = No HS/GED, 2 = GED but no college 3 = some college or higher
births$MEDUC_R <- rep(NA, nrow(births))
births$MEDUC_R[which(births$MEDUC %in% c(1:2) )] <- 1
births$MEDUC_R[which(births$MEDUC == 3)] <- 2
births$MEDUC_R[which(births$MEDUC %in% c(4:8))] <- 3

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

## Create combined FIPS variable
# Transform MRSTATE (abbreviation to FIPS code)
births$MRSTATEFIPS <- rep(NA, nrow(births))
births$MRSTATEFIPS <- as.character(fips(births$MRSTATE, to = "FIPS"))
# Note: this transformation may not work with non-US states
# Caution: 84,505 "NA" values

# Reset character to logical NA
births$MRSTATEFIPS[which(births$MRSTATEFIPS == "NA")] <- NA

# Reset long value to logical NA
births$MRSTATEFIPS[which(nchar(births$MRSTATEFIPS) > 2)] <- NA

# Add leading zeroes to one-digit state FIPS
births$MRSTATEFIPS[which(nchar(births$MRSTATEFIPS) == 1)] <- paste0("0", births$MRSTATEFIPS[which(nchar(births$MRSTATEFIPS) == 1)])

# Initialize combined variable
births$COMBFIPS <- rep(NA, nrow(births))

# Create combined FIPS variable
births$COMBFIPS[which(nchar(births$MRSTATEFIPS) == 2)] <- paste0(births$MRSTATEFIPS[which(nchar(births$MRSTATEFIPS) == 2)], 
                                                                 births$MRCNTYFIPS[which(nchar(births$MRSTATEFIPS) == 2)])

## Restrict to US

outfile <- paste0("NCHS Birth Data/R/allbirths_newvars.rda")
saveRDS(births, outfile)  

## Output aggregated datasets for analysis ------------------------------------------------
## Counts of each preterm birth outcome plus count of total birth in strata

births2 <- select(births, DOB_YY, RACEHISP_RECODE, COMBFIPS, PLURALITY, PTB, VPTB, MPTB, LPTB)

# Cut Non-US births?
# MRSTATE = AB, AS, BC, GU, PR, VI, CC, CU, MX, XX, ZZ, MB, NB, NF, NL, NT, NS, NU, ON, PE, QC, SK, YT, ?MP?

## YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
df1 <- births2 %>%
  
  # Restrict to NH-Black and NH-White
  filter(RACEHISP_RECODE == 2 | 
           RACEHISP_RECODE == 3) %>%
  
  # Restrict to singleton births
  filter(PLURALITY == "Singleton") %>%
  
  # Group by Year, County, Race
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE) %>%
  
  # Summarize preterm birth
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY))))
saveRDS(df1, "NCHS Birth Data/R/df1.rda")   
  
## YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
df2 <- births %>%
    
  # # Restrict to NH-Black and NH-White
  # filter(RACEHISP_RECODE == 2 | 
  #          RACEHISP_RECODE == 3) %>%
  #          
  # Restrict to singleton births
  filter(PLURALITY == "Singleton") %>%
    
  # Group by Year, County, Race, Age Cat
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE, MAGER9) %>%
  
  # Summarize preterm birth  
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY))))   
saveRDS(df2, "NCHS Birth Data/R/df2.rda")

## YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS 
df3 <- births %>%
  
  # # Restrict to NH-Black and NH-White
  # filter(RACEHISP_RECODE == 2 | 
  #          RACEHISP_RECODE == 3) %>%
  
  # Restrict to singleton births
  filter(PLURALITY == "Singleton") %>%
  
  # Group by Year, County, Race, Age Cat, Marital, Education
  group_by(DOB_YY, COMBFIPS, RACEHISP_RECODE, MAGER9, MAR, MEDUC_R) %>%
  
  # Summarize preterm birth  
  summarise(vptb = sum(VPTB),
            lptb = sum(LPTB),
            mptb = sum(MPTB),
            ptb = sum(PTB),
            births = sum(!(is.na(DOB_YY)))) 
saveRDS(df3, "NCHS Birth Data/R/df3.rda")

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
saveRDS(df4, "NCHS Birth Data/R/df4.rda")

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
saveRDS(df5, "NCHS Birth Data/R/df5.rda")

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
saveRDS(df6, "NCHS Birth Data/R/df6.rda")
  
