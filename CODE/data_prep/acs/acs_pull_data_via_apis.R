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
# -------------------------------------- ACS5 2009 ------------------------------------------------- #
######################################################################################################

# ------------------------------------- race/ethnicity --------------------------------------------- #
acs_race_eth <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME","B02001_001E", #total pop
                                 "B02001_002E", #white pop alone
                                 "B02001_003E", #black pop alone 
                                 "B03001_003E" #hispanic or latino
                                 ), 
                        region = "county:*",
                        regionin = "state:02")

#rename columns

#create table with the following cols:
##  ---- percent black
##  ---- percent white
##  ---- percent hispanic
##  ---- black-white ratio

# ------------------------------------- income, poverty, housing --------------------------------------------- #
acs_income <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME","B19013_001E", # Median household income in the past 12 months (in 2009 inflation-adjusted dollars), B19013. Median Household Income
                                 "B17001_001E", #Total:	B17001. Poverty Status in the past 12 Months by Sex by Age
                                 "B17012_001E", # Total:	B17012. POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF RELATED CHILDREN UNDER 18 YEARS
                                 "B00002_001E" #	Total	B00002. Unweighted Sample Housing Units
                                 ), 
                        region = "county:*",
                        regionin = "state:02")
acs_poverty <- 

acs_housing <- getCensus(name = "acs5",
                         vintage = 2009, 
                         vars = c("NAME",
                                  "B25001_001E", #Total	B25001. Housing Units
                                  "B25002_003E", #Vacant	B25002. Occupancy Status
                                  "B25003_002E", #Owner occupied	B25003. Tenure
                                  "B25003_003E", #Renter occupied	B25003. Tenure
                                  
                                  ), 
                         region = "tract:*",
                         regionin = "state:02")



acs_poverty_group <- getCensus(name = "acs/acs5",
                               vintage = 2016, 
                               vars = c("NAME", "group(B17020)"), 
                               region = "tract:*",
                               regionin = "state:02")
# List column names
colnames(acs_poverty_group)


acs_income2 <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME", "B19013_001E", "B19013_001EA", "B19013_001M", "B19013_001MA"), 
                        region = "tract:*",
                        regionin = "state:02")



acs_income <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("NAME", "B19013_001E", "B19013_001EA", "B19013_001M", "B19013_001MA"), 
                        region = "tract:*",
                        regionin = "state:02")
