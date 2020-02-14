############################################
# code author: erin stearns
# script objective: prep new app inputs - contextual vars & model outcomes
# date: 3 november 2019
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse, geojsonsf, geojsonio, tidyr)

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
# -------------------------------------- data validation: 1 ---------------------------------------- #
######################################################################################################
# check for nulls/nans/missing
acs.nullnanmiss <- null_nan_miss(as.data.table(acs))


# plot
# able to successfully plot with leaflet
acs6 <- st_transform(acs5, crs = 4326)

pal <- colorBin("YlOrRd", domain = acs6$`% Poverty`, bins = 5)
leaflet(acs6) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(`% Poverty`),
    weight = 2, 
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7
  )
