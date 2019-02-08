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
pacman::p_load(censusapi, data.table, tidycensus, tidyr,tidyverse,dplyr)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

#define the survey to use
acs_survey <- "acs5"

######################################################################################################
# -------------------------------------- load config ----------------------------------------------- #
######################################################################################################
config <- fread(file = paste0(data_repo, "/acs/config_acs_api_pull.csv"), stringsAsFactors = F)

#will need to loop through by year and grouping
years <- unique(config$year)
i <- years[1]  

######################################################################################################
# -------------------------------------- 2010 county boundaries ------------------------------------ #
######################################################################################################
#get base pop & associated spatial data -- NOT CURRENTLY WORKING
acs_geo <- get_acs(geography = (unique(config$geo_level)),
                   variables = "B25001_001E",
                   year = 2010,
                   survey = "acs5",
                   geometry = TRUE
                   )

######################################################################################################
# -------------------------------------- function for extracting acs data using config  ------------ #
######################################################################################################
pull_acsdata <- function(group, yr){
  # Function for pulling ACS data via the US Census bureau API using a custom config file specifying necessary
  #           arguments.
  # Args:
  #   group   : string value corresponding to field in config entitled "grouping" -- which group do you want to extract data for?
  
  # ----------------------------------- subset -------------------------------------------------------
  #subset config to grouping vars
  config_sub <- config[grouping == (group) & year == (yr),] 
  
  # ----------------------------------- make key -----------------------------------------------------
  #reduce config to get acs name - field names key
  config_names <- config_sub[,c('mod_label','acs_name'),with=F]
  
  # ----------------------------------- grab dat data! -----------------------------------------------
  #get relevant data
  acs_data <- get_acs(geography = (unique(config_sub$geo_level)),
                      variables = as.vector(config_sub$acs_name),
                      year = (i),
                      survey = (acs_survey)
                      ) %>% 
    
    #join to config key to get readable var names
    left_join(config_names, by = c("variable"="acs_name"))
  
  return(acs_data)
  
}

## how to concatenate groups of acs data --
#   split-apply-combine strategy: already split, and will apply, now need to combine
#   1. could try to use an if statement, if(group == grps[1]){define acs_data for first time} else {extract data, then rowbind to acs_data}
#   2. could do a for loop and in that loop take care of the concatenation
#   3. could try an lapply using the get_acs function, then bind list elements

#option 3.
pull_acsdata <- function(groups, yrs){
  # Function for pulling ACS data via the US Census bureau API using a custom config file specifying necessary
  #           arguments.
  # Args:
  #   group   : string value corresponding to field in config entitled "grouping" -- which group do you want to extract data for?
  
  # ----------------------------------- subset -------------------------------------------------------
  #subset config to grouping vars
  config_sub <- config[grouping == (group) & year == (yr),] 
  
  # ----------------------------------- make key -----------------------------------------------------
  #reduce config to get acs name - field names key
  config_names <- config_sub[,c('mod_label','acs_name'),with=F]
  
  # ----------------------------------- grab dat data! -----------------------------------------------
  #get relevant data
  acs_data <- get_acs(geography = (unique(config_sub$geo_level)),
                      variables = as.vector(config_sub$acs_name),
                      year = (i),
                      survey = (acs_survey)
  ) %>% 
    
    #join to config key to get readable var names
    left_join(config_names, by = c("variable"="acs_name"))
  
  return(acs_data)
  
}





######################################################################################################
# -------------------------------------- call funciton --------------------------------------------- #
######################################################################################################
#define a vector of groups -- used this in config, field entitled 'grouping'; will use this vector
#     to apply function over each element in vector
grps <- c("race_eth", "income", "poverty", "housing", "education", "employment")

#will need to apply over year too
years <- unique(config$year)
i <- years[1]  

######################################################################################################
# -------------------------------------- ACS5 2009 ------------------------------------------------- #
######################################################################################################


# ------------------------------------- race/ethnicity --------------------------------------------- #
group_sub = "race_eth"

#subset config to race eth vars
config_sub <- config[grouping == (group)] 

#reduce config to get acs name - field names key
config_names <- config_sub[,c('mod_label','acs_name'),with=F]

#will need to loop through by year and grouping
years <- unique(config_sub$year)
i <- years[1]  

#get relevant data
acs_data <- get_acs(geography = (unique(config_sub$geo_level)),
                    variables = as.vector(config_sub$acs_name),
                    year = (i),
                    survey = "acs5"
                    ) %>% 
  #join to config key to get readable var names
  left_join(config_names, by = c("variable"="acs_name"))
  
  
#join
acs_data2 <- left_join(acs_data, config_names, by = c("variable"="acs_name"))

#test on another grouping

#start making function -- bind all groupings together into one long table
acs_race_eth <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME","B02001_001E", #total pop
                                 "B02001_002E", #white pop alone
                                 "B02001_003E", #black pop alone 
                                 "B03001_003E" #hispanic or latino
                                 ), 
                        region = "county:*",
                        regionin = "state:02")

compare <- getCensus(name = "acs5",
                          vintage = 2009, 
                          vars = c("NAME","B02001_001E", #total pop
                                   "B01003_001E",
                                   "B00001_001E", #unweighted households
                                   "B25001_001E" #housing units
                          ),
                          region = "county:*",
                          regionin = "state:02")


B00001_001E


#rename columns

#create table with the following cols:
##  ---- percent black
##  ---- percent white
##  ---- percent hispanic
##  ---- black-white ratio

# ------------------------------------- income -------------------------------------------------------------- #
#grouping = "income"
acs_income <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME",
                                 "B19013_001E" # Median household income in the past 12 months (in 2009 inflation-adjusted dollars), B19013. Median Household Income
                                 ), 
                        region = "county:*",
                        regionin = "state:02")

# ------------------------------------- poverty ------------------------------------------------------------- #
#grouping = "poverty"
acs_poverty <- getCensus(name = "acs5",
                         vintage = 2009, 
                         vars = c("NAME",
                                  "B25001_001E", #	Housing Units
                                  "B17001_001E", #Total:	B17001. Poverty Status in the past 12 Months by Sex by Age
                                  "B17012_001E" # Total:	B17012. POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF RELATED CHILDREN UNDER 18 YEARS
                                  
                         ), 
                         region = "county:*",
                         regionin = "state:02")

#calculate rate (divide by pop total)

# ------------------------------------- housing ------------------------------------------------------------- #
#grouping = "housing"
acs_tenure <- getCensus(name = "acs5",
                         vintage = 2009, 
                         vars = c("NAME",
                                  "B25003_001E", #Tenure total -- use as denominator
                                  "B25002_003E", #Vacant	B25002. Occupancy Status
                                  "B25003_002E", #Owner occupied	B25003. Tenure
                                  "B25003_003E" #Renter occupied	B25003. Tenure
                                  
                                  
                                  ), 
                         region = "tract:*",
                         regionin = "state:02")

#calculate tenure as proportion owners vs renters

acs_stability <- getCensus(name = "acs5",
                        vintage = 2009, 
                        vars = c("NAME",
                                 "B25038_001E",# B25038. Tenure by Year Householder Moved into Unit
                                 "B25038_003E", #	Owner occupied:!!Moved in 2005 or later
                                 "B25038_010E" #Renter occupied:!!Moved in 2005 or later
                        ), 
                        region = "tract:*",
                        regionin = "state:02")

#calculate stability:
#Proportion of population that has resided in the area for previous 1-5 years
#sum owner and renter occupied : moved in 2005orlater; divide sum by total
# ------------------------------------- education ----------------------------------------------------------- #
#grouping = "education"



# ------------------------------------- employment ---------------------------------------------------------- #
try <- getCensus(name = "acs5",
                 vintage = 2009, 
                 table = "S2301",
                 region = "tract:*",
                 regionin = "state:02")

try2 <- get_acs(geography = "county",
                table = "S2301",
                year = 2009,
                survey = "acs5",
                state = "GA")


#SCRAP
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
