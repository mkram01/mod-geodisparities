# March of Dimes Analysis #
# 01: Reading Script     #
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
library(plyr)
library(tidyr)
library(magrittr)
library(cdlTools)

## Read in data ------------------
years <- c(2007:2016)
for (i in seq_along(years)) {
  # List file
  fn <- list.files("NCHS Birth Data/SAS Data", pattern = as.character(years[i]), full.names = TRUE)
  # Read in file
  birth_year <- haven::read_sas(fn)
  year <- years[i]
  births <- birth_year
  
  # For combined dataset:
  # If first year, then create dataset, else append
  # if (i == 1) {
  #   births <- birth_year
  # } else {
  #   births <- rbind(births, birth_year)
  # }
  
  outfile <- paste0("NCHS Birth Data/R/births", year,".rda")
  saveRDS(births, file = outfile)
  
  cat("*")
}  

## Output by-year datasets  -----------
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
