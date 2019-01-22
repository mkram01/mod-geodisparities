# March of Dimes Analysis # 
# 03: Analysis Script     #
# Kevin Weiss             #
# 01/07/2019              #

## Package setup -----------------
library(sas7bdat)
library(haven)
library(Hmisc)
library(feather)
library(dplyr)
library(plyr)
library(magrittr)
library(cdlTools)

# Read in Config file
config <- load_config(repo = repo,
                      indicator_group = indicator_group,
                      indicator = indicator)
