############################################
# code author: erin stearns
# script objective: prep new app inputs - contextual vars & model outcomes
# date: 3 november 2019
###########################################

rm(list = ls())

#NTS:
#   - do i need to even load the basegeo file?


# Summary of what this script does to prepare the data for being used by the app:
#    1. Create state GEOID
#    2.
#    3.
#    4.
#    5.
#    6.
#    7.

# Summary of what this script does to prepare the metadata for being used by the app:
#    1.
#    2.
#    3.
#    4.
#    5.
#    6.
#    7.

# Ways ACS and model data differ
#    1. ACS 'NAME' field includes county name, the word 'county', and state name while model data only has the county name, sans the word 'county'
#    2.
#    3.
#    4.
#    5.
#    6.
#    7.
######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse, geojsonsf, geojsonio, tidyr, plotly)

#set mapbox token if needed - https://docs.mapbox.com/help/glossary/access-token/
#Sys.setenv("MAPBOX_TOKEN" = "insert token here")

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
#setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

#contextual data & associated data dictionary
contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-data-4December2019.rds")
contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata-11feb2020.rds")

#perinatal model data & associated data dictionary
model_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-data-11feb2020.rds")
model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata-11feb2020.rds")

#matrix of variables x years with data source year as cell value -- to be used to dynamically plug year of data source into captioning
mapyears_data <- paste0(data_repo, "/app_inputs/pre_processed_inputs/map-years-4December2019.rds")

#base spatial layer -------------------------------------------------------------------------------------------------NTS:: why do i need this in here?
base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")

######################################################################################################
# -------------------------------------- functions ------------------------------------------------- #
######################################################################################################
null_nan_miss <- function(dt){
  # objective: function that prints out null, nan and missing vals for value of interest
  # inputs:
  #  REQUIRED:
  #       dt: data table -- expecting wide format
  # output:    report of null, nan and missing values

  # Assess nulls
  nas.vals <- dt[, lapply(.SD, function(x) length(x[is.na(x)])), .SD]
  nas.vals.long <- suppressWarnings(as.data.table(melt(nas.vals,
                                                       variable.name = "fieldname",
                                                       value.name = ("null_ct")))
  )

  # Assess nans
  nan.vals <- dt[, lapply(.SD, function(x) length(x[is.na(x)])), .SD]
  nan.vals.long <- suppressWarnings(as.data.table(melt(nan.vals,
                                                       variable.name = "fieldname",
                                                       value.name = ("nan_ct")))
  )

  # Assess missingness
  miss.vals <- dt[, lapply(.SD, function(x) length(x[unlist(x == "")])), .SD]
  miss.vals.long <- suppressWarnings(as.data.table(melt(miss.vals,
                                                        variable.name = "fieldname",
                                                        value.name = ("miss_ct")))
  )

  #rowbind together & output as one summary
  summary <- merge(nas.vals.long, nan.vals.long)
  summary <- merge(summary, miss.vals.long)

  #create new field to indicate if nas/nans/missings same count across
  summary[, unequal.counts:=0]
  summary[(null_ct != nan_ct | null_ct != miss_ct | nan_ct != miss_ct), unequal.counts := 1]

}

check_delaware <- function(data){
  
}
######################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
#load contextual data
acs <- readRDS(contextual_data)
#load meta data
cdd <- readRDS(contextual_dd)

#load perinatal outcome model data
mod <- readRDS(model_data)
#load meta data
mdd <- readRDS(model_dd)

#load matrix of vars with data source year
mapyears <- as.data.table(readRDS(mapyears_data))

#load state fips codes to get state name in model data --------------------------------------------------------------NTS:: use of this file might be part of issue
base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")
states <- fread(paste0(data_repo,"/app_inputs/pre_processed_inputs/state_fips_codes.csv"), stringsAsFactors = F,
                colClasses = list(character=c("state_name", "fips_code")))

#load 2017 US counties spatial layer
basegeo <- st_read(base)

######################################################################################################
# -------------------------------------- 1. create state geoid --------------------------------------- #
######################################################################################################
#1. create state geoid
acs$state_geoid <- substr(acs$GEOID, 0,2)
mod$state_geoid <- substr(mod$GEOID, 0,2)
basegeo$state_geoid <- substr(basegeo$GEOID, 0,2)

######################################################################################################
# -------------------------------------- validate input data --------------------------------------- #
######################################################################################################
#verify same states covered 
if ((length(unique(acs$state_geoid)) == length(unique(mod$state_geoid)))){
  message("Your ACS and model data have the same number of states")
} else {
  message("Eeeks. Your ACS and model data DO NOT have the same number of states")
}
acs.states <- unique(acs$state_geoid)
mod.states <- unique(mod$state_geoid)
acs.overlap <- length(acs.states[acs.states %in% mod.states])
mod.overlap <- length(mod.states[mod.states %in% acs.states])
if ((length(unique(acs$state_geoid)) == acs.overlap) & (length(unique(mod$state_geoid)) == mod.overlap)){
  message("Your ACS and model data cover the same states")
} else {
  message("Eeks. Your ACS and model data DO NOT cover the same states")
}

#verify same counties covered 
if ((length(unique(acs$GEOID)) == length(unique(mod$GEOID)))){
  message("Your ACS and model data have the same number of counties")
} else {
  message("Eeks. Your ACS and model data DO NOT have the same number of counties")
}
acs.counties <- unique(acs$GEOID)
mod.counties <- unique(mod$GEOID)
acs.county.overlap <- length(acs.counties[acs.counties %in% mod.counties])
mod.county.overlap <- length(mod.counties[mod.counties %in% acs.counties])
if ((length(unique(acs$GEOID)) == acs.county.overlap) & (length(unique(mod$GEOID)) == mod.county.overlap)){
  message("Your ACS and model data cover the same counties")
} else {
  message("Eeeks. Your ACS and model data DO NOT cover the same counties")
}





plot_ly(basegeo)
plot_geo(basegeo)
plot_mapbox(basegeo)
plot_mapbox(acs)

base.geot <- st_transform(basegeo, crs = 4326)
plot_mapbox(base.geot)

######################################################################################################
# -------------------------------------- basic data cleanup ---------------------------------------- #
######################################################################################################

#get rid of trailing whitespace in var display names
cdd$display_name <- trimws(cdd$display_name)
mdd$display_name <- trimws(mdd$display_name)
