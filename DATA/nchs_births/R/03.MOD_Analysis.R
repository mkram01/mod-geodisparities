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
library(tidyr)
library(magrittr)
library(cdlTools)

model1 <- readRDS(file = "DATA/nchs_births/R/model1.rda")
model2 <- readRDS(file = "DATA/nchs_births/R/model2.rda")
model3 <- readRDS(file = "DATA/nchs_births/R/model3.rda")
model4 <- readRDS(file = "DATA/nchs_births/R/model4.rda")
model5 <- readRDS(file = "DATA/nchs_births/R/model5.rda")
model6 <- readRDS(file = "DATA/nchs_births/R/model6.rda")

# Read in Config file
config <- load_config(repo = repo,
                      indicator_group = indicator_group,
                      indicator = indicator)
