############################################
# code author: erin stearns
# script objective: checking data validity of MoD geodisparities app inputs
# date: 13 february 2020
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
message(paste0("You have specified ", data_repo, " as the location of your data."))

#contextual data & associated data dictionary
contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-data-4December2019.rds")
#contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata.rds")
contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata-4December2019.rds")

#perinatal model data & associated data dictionary
model_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-data-2november2019.rds")
#model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata.rds")
model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata.csv")

#matrix of variables x years with data source year as cell value -- to be used to dynamically plug year of data source into captioning
mapyears_data <- paste0(data_repo, "/app_inputs/pre_processed_inputs/map-years-4December2019.rds")

#base spatial layer
base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")

#post-processed data (what gets fed into app)
appdata.path <- paste0(data_repo,"/app_inputs/all-data-23december2019.rds")

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
mdd <- fread(model_dd, stringsAsFactors = F)

#load matrix ov vars with data source year
mapyears <- as.data.table(readRDS(mapyears_data))

#load state fips codes to get state name in model data
states <- fread(paste0(data_repo,"/app_inputs/pre_processed_inputs/state_fips_codes.csv"), stringsAsFactors = F,
                colClasses = list(character=c("state_name", "fips_code")))

#load 2017 US counties spatial layer
basegeo <- st_read(base)
#create state code field
basegeo$state_fips <- substr(basegeo$GEOID, 0, 2)

# load post-processed data (what gets fed into app)
appdata <- readRDS(appdata.path)

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
# -------------------------------------- validate state names -------------------------------------- #
######################################################################################################
state.names <- unique(appdata$state_name)
print(state.names)

#check out rows where is.na(state_name)
nastate <- as.data.table(appdata[is.na(appdata$state_name),])
nrow(nastate) #on 2/13: 2403 rows

# any other fields NULL or missing? -- ignore results for geom field, it is present.
whatshappening <- null_nan_miss(nastate) #on 2/13 -- consistently missing modeled data

#create new field for state FIPS code to figure out where
nastate[,state_fips:=substr(nastate$GEOID, 0, 2)]
unique(nastate$state_fips) #-- not just one state
length(unique(nastate$state_fips)) #41 states

######################################################################################################
# -------------------------------------- investigate delaware issue -------------------------------- #
######################################################################################################
# how many of rows from nastate above belong to delaware? state fips: 10
nrow(nastate[state_fips == "10",]) #12 rows

### how many census tracts total in Delaware
nrow(basegeo[basegeo$state_fips == "10",]) #3??? this is the problem

### getting delaware fips from base geo layer
de_basegeo <- (basegeo[basegeo$state_fips == "10","GEOID"]) # only 3
de_basegeo$geom <- NULL

#### check if these in main dataset
de_main <- appdata[appdata$GEOID %in% de_basegeo$GEOID,]
nrow(de_main) #36 rows of overlap
View(de_main)

#### look at all delaware data in app input
de.appdata <- appdata[appdata$state_name == "Delaware",] #2427 rows - why/how is it subsetting if the state_name field is NA
length(unique(de.appdata$GEOID)) #3 census tracts
#check nas/nans/missingness
de.appdata.dt <- as.data.table(de.appdata)
delawherrre <- null_nan_miss(de.appdata.dt)
print(delawherrre) #missing data for almost all rows - similar and sometimes exact same number as NA state names (2403) hmm
nrow(de.appdata.dt)

### how many census tracts do the individual pre-processed inputs have?
#create state code
acs$state_fips <- substr(acs$GEOID, 0,2)
mod$state_fips <- substr(mod$GEOID, 0,2)

#delaware
de.acs <- acs[acs$state_fips == "10",]
length(unique(de.acs$GEOID))
de.mod <- mod[mod$state_fips == "10",]
print(paste0("ACS/Contextual data has :", length(unique(de.acs$GEOID)), " Delaware census tracts and model/perinatal outcome data has: ", length(unique(de.mod$GEOID))))
