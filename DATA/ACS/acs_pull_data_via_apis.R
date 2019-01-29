############################################
# code author: erin stearns
# script objective: obtain ACS 5-year estimates for 2007-2016 via the US Census Bureau's APIs
# date: 22 january 2019
###########################################

# To run the code below be sure to sign up for an API key here(https://api.census.gov/data/key_signup.html)
#   Then, if you’re on a non-shared computer, add your Census API key to your .Renviron profile and 
#         call it CENSUS_KEY. censusapi will use it by default without any extra work on your part. 
#   If on a shared computer, within R, run:
    # # Add key to .Renviron
    # Sys.setenv(CENSUS_KEY=YOURKEYHERE)
    # # Reload .Renviron
    # readRenviron("~/.Renviron")
    # # Check to see that the expected key is output in your R console
    # Sys.getenv("CENSUS_KEY")
# Good reference: https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
pacman::p_load(censusapi, data.table)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

######################################################################################################
# -------------------------------------- find appropriate APIs ------------------------------------- #
######################################################################################################
#To see a current table of every available endpoint, run listCensusApis:
apis <- listCensusApis()
View(apis)

#checkout variables inside api of interest - acs5; 2005-2009 5-year ACS 
acs_vars <- listCensusMetadata(name = "acs5",
                               vintage = 2009,
                               type = "variables")

######################################################################################################
# -------------------------------------- ACS5 2009 ------------------------------------- #
######################################################################################################
#race/ethnicity
acs_race_eth <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME","B02001_001E", #total pop
                                 "B02001_002E", #white pop alone
                                 "B02001_003E", #black pop alone 
                                 "B03001_003E" #hispanic or latino
                                 ), 
                        region = "county:*",
                        regionin = "state:02")



acs_income <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("NAME", "B19013_001E", "B19013_001EA", "B19013_001M", "B19013_001MA"), 
                        region = "tract:*",
                        regionin = "state:02")
