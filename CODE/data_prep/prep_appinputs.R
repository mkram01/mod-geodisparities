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
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
#setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

#TO-DO -- define app input data for preparation to be pulled into app
#contextual data & associated data dictionary
contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-data-4november2019.rds")
contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata.rds")

#perinatal model data & associated data dictionary
model_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-data-2november2019.rds")
model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata.rds")

#base spatial layer
#base <- paste0(data_repo,"/app_inputs/re_processed_inputs/us_counties_2017.gpkg")

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

#load state fips codes to get state name in model data
states <- fread(paste0(data_repo,"/app_inputs/pre_processed_inputs/state_fips_codes.csv"), stringsAsFactors = F,
                colClasses = list(character=c("state_name", "fips_code")))

#load 2017 US counties spatial layer
#basegeo <- st_read(base)

######################################################################################################
# -------------------------------------- basic data cleanup ---------------------------------------- #
######################################################################################################
#remove rows with NA 'NAME' values

# change certain fields into particular classes
#context_spatial$`Maternity care access (2016)` <- as.character(context_spatial$`Maternity care access (2016)`)

######################################################################################################
# -------------------------------------- Rename data fields to their human-readable versions ------- #
######################################################################################################
#create vector of field names
acsoriginal <- names(acs)
modoriginal <- names(mod)

#enumerate field names vector and place in dt
acsnamesdt <- as.data.table(acsoriginal)
acsnamesdt <- acsnamesdt[,id:=row.names(acsnamesdt)]

modnamesdt <- as.data.table(modoriginal)
modnamesdt <- modnamesdt[,id:=row.names(modnamesdt)]

#join to meta data dictionary
acsnamesdt2 <- as.data.table(left_join(acsnamesdt, cdd, by=c("acsoriginal" = "varname")))
modnamesdt2 <- as.data.table(left_join(modnamesdt, mdd, by=c("modoriginal" = "varname")))

#create single new field for names
acsnamesdt2 <- acsnamesdt2[,newname:=display_name]
acsnamesdt2 <- acsnamesdt2[is.na(acsnamesdt2$display_name),newname:=acsoriginal]

modnamesdt2 <- modnamesdt2[,newname:=display_name]
modnamesdt2 <- modnamesdt2[is.na(modnamesdt2$display_name),newname:=modoriginal]

#get new vectors using 'newname' field
acsnewnames <- acsnamesdt2[["newname"]]
modnewnames <- modnamesdt2[["newname"]]

#rename vars 
acs2 <- setnames(acs, acsoriginal, acsnewnames)
mod2 <- setnames(mod, modoriginal, modnewnames)

######################################################################################################
# -------------------------------------- create county and state name fields ----------------------- #
######################################################################################################
#copy
acs3 <- copy(acs2)
#create county name field
acs3$county_name <- gsub(" .*$", "", acs3$NAME)
#create state name field
acs3$state_name <- gsub('.*,\\s*', '', acs3$NAME)

#copy
mod3 <- copy(mod2)
#create county field in model data
mod3$county_name <- mod3$NAME
#create state fips
mod3$state_fips <- substr(mod3$GEOID, start = 1, stop = 2)
#join to state fips key
mod4 <- left_join(mod3, states, by = c("state_fips" = "fips_code"))

#drop puerto rico
acs4 <- (acs3[acs3$state_name != "Puerto Rico",])
mod5 <- (mod4[mod4$state_name != "Puerto Rico",])

#rework cols to keep
acs5 <- acs4[,c(1:2, 53, 54, 4:52)] #GEOID, year, county_name, state_name, ..., geom
mod6 <- mod5[,c(1, 3, 45, 47, 4:44)] #GEOID, year, county_name, state_name, ..., geom

######################################################################################################
# ------------------------------------------ simplify geometry ------------------------------------- #
######################################################################################################
#transform to Albers Equal Area projection
aeap <- "+proj=aea +lat_1=29.83333333333334 +lat_2=45.83333333333334 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#transform to projected CRS and try simplifying
acs6 <- st_transform(acs5, aeap)
mod7 <- st_transform(mod6, aeap)

#geometry simplification
acs_smpl <- st_simplify(acs6, dTolerance = 0.1, preserveTopology = T)
mod_smpl <- st_simplify(mod7, dTolerance = 0.1, preserveTopology = T)

######################################################################################################
# ------------------------------------------ set CRS ----------------------------------------------- #
######################################################################################################
#transform to wgs84
acs_fin <- st_transform(acs_smpl, crs = 4326)
mod_fin <- st_transform(mod_smpl, crs = 4326)

#test
#plot(st_geometry(acs_fin[acs_fin$state_name == "Maine" & acs_fin$year=="2008",])) 
#plot(st_geometry(mod_fin[mod_fin$state_name == "Georgia" & mod_fin$year=="2008",]))

######################################################################################################
# -------------------------------------- save spatial data objects ------------------------------- #
######################################################################################################
saveRDS(acs_fin, file = paste0(data_repo,"/app_inputs/contextual-data-5november2019.rds"))
saveRDS(mod_fin, file = paste0(data_repo,"/app_inputs/perinatal-data-3november2019.rds"))


######################################################################################################
# ---------------------------------------------- SCRAP --------------------------------------------- #
######################################################################################################
# ------ make sure geographies & years overlap -- 11/3 same length ------
#acsgeos <- unique(acs2$GEOID) 
#modgeos <- unique(mod2$GEOID)
#mod2 <- mod2[,mod_name:=NAME]
#join data by geoid & year to eliminate mismatches & subset data accordingly
#acsgeoyr <- acs2[,c("GEOID", "year", "NAME")]
#modgeoyr <- mod2[,c("GEOID", "year", "mod_name")]
#join
# acsmod <- left_join(acsgeoyr, modgeoyr)
# #remove all rows where county_name is null
# acsmod2 <- acsmod[!is.na(acsmod$mod_name),]
# #create a keep dt to join back to each and filter
# keeprows <- acsmod2[,c("GEOID", "year")]
# keeprows$keep <- 1
# #join back to each table
# acsfiltered <- as.data.table(left_join(acs2, keeprows))
# acsfiltered2 <- acsfiltered[!is.na(acsfiltered$keep),]
# 
# modfiltered <- as.data.table(left_join(mod2, keeprows))
# modfiltered2 <- modfiltered[!is.na(modfiltered$keep),]
