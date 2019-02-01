# March of Dimes Analysis #
# 01: Analysis Script     #
# Kevin Weiss             #
# 01/31/2019              #

## Package and Data setup -----------------
library(sas7bdat)
library(haven)
library(Hmisc)
library(feather)
library(dplyr)
library(plyr)
library(tidyr)
library(magrittr)
# install.packages("INLA", repos = c(getOption("repos"), 
#                                    INLA = "https://inla.r-inla-download.org/R/stable"), 
#                  dep = TRUE)
library(INLA)



# Read in Summary data files from SAS
model1 <- readRDS(file = "DATA/nchs_births/R/data/model1.rda")
model2 <- readRDS(file = "DATA/nchs_births/R/data/model2.rda")
model3 <- readRDS(file = "DATA/nchs_births/R/data/model3.rda")
model4 <- readRDS(file = "DATA/nchs_births/R/data/model4.rda")
model5 <- readRDS(file = "DATA/nchs_births/R/data/model5.rda")
model6 <- readRDS(file = "DATA/nchs_births/R/data/model6.rda")

# Data Description

# Model 1: YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
# Model 2: YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
# Model 3: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS 
# Model 4: YEAR x COUNTY x RACE restricted to NH-Black/NH-White (including multiple births)
# Model 5: YEAR x COUNTY x RACE x AGE (including multiple births)
# Model 6: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION (including multiple births)




# Read in Config file
config <- load_config(repo = repo,
                      indicator_group = indicator_group,
                      indicator = indicator)
