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
# 
# Feather
write_feather("feather/births2007")
write_feather("feather/births2008")
write_feather("feather/births2009")
write_feather(births2010, "feather/births2010")
write_feather(births2011, "feather/births2011")
write_feather(births2012, "feather/births2012")
write_feather(births2013, "feather/births2013")
write_feather(births2014, "feather/births2014")
write_feather(births2015, "feather/births2015")
write_feather(births2016, "feather/births2016")

## Subset data -------------------
## Restrictions (e.g. only singleton births)
## Exclude births to foreign residents (RESTATUS)
## Singleton births only (DPLURAL)
vars <- c()

df <- births %>%
  filter(RESTATUS != 4) %>%
  filter(DPLURAL < 2)


## Data Cleaning -----------------



## Census Data --------------------



## Output final dataset -----------
