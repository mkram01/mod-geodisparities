############################################
# code author: erin stearns
# script objective: prep new app inputs - contextual vars & model outcomes
# date: 29 october 2019
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
contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/mod-contextual-vars.rds")
contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-variable-dictionary.csv")

#perinatal model data & associated data dictionary
model_data <- paste0(data_repo,"/app_inputs/re_processed_inputs/ptb_vptb.rds")
model_dd <- paste0(data_repo,"/app_inputs/re_processed_inputs/ptb_vptb-dictionary.csv")

#base spatial layer
base <- paste0(data_repo,"/app_inputs/re_processed_inputs/us_counties_2017.gpkg")


#temporary metadata for previous data
context_metadata <- paste0(data_repo,"/app_inputs/pre_processed_inputs/context_vars_palettemapper.csv")
mod_metadata <- paste0(data_repo,"/app_inputs/pre_processed_inputs/model_vars_palettemapper.csv")

######################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
#load contextual data
acs <- readRDS(contextual_data)
#load data dictionary
cdd <- fread(contextual_dd, stringsAsFactors = F)

#load perinatal outcome model data
mod <- readRDS(model_data)
#load data dictionary
mdd <- fread(model_dd, stringsAsFactors = F)

#load state fips codes to get state name in model data
states <- fread(paste0(data_repo,"/app_inputs/state_fips_codes.csv"), stringsAsFactors = F,
                colClasses = list(character=c("state_name", "fips_code")))

#load 2017 US counties spatial layer
basegeo <- st_read(base)

#read in metadata
context_meta <- fread(context_metadata, stringsAsFactors = F)
mod_meta <- fread(mod_metadata, stringsAsFactors = F)

######################################################################################################
# -------------------------------------- save meta data objects ----------------------------------- #
######################################################################################################
saveRDS(context_meta, file = paste0(data_repo,"/app_inputs/context_metadata.Rds"))
saveRDS(mod_meta, file = paste0(data_repo,"/app_inputs/mod_metadata.Rds"))

######################################################################################################
# -------------------------------------- wrangle data ---------------------------------------------- #
######################################################################################################
#convert both aspatial datasets to data table objects
acs <- as.data.table(acs)
mod <- as.data.table(mod)

#create vector of field names
acsoriginal <- names(acs)
modoriginal <- names(mod)

#enumerate field names vector and place in dt
acsnamesdt <- as.data.table(acsoriginal)
acsnamesdt <- acsnamesdt[,id:=row.names(acsnamesdt)]

modnamesdt <- as.data.table(modoriginal)
modnamesdt <- modnamesdt[,id:=row.names(modnamesdt)]

#join to data dictionary
acsnamesdt2 <- left_join(acsnamesdt, cdd, by=c("acsoriginal" = "Variable"))
modnamesdt2 <- left_join(modnamesdt, mdd, by=c("modoriginal" = "Variable"))

#get new vectors using 'display_name' field
acsnewnames <- acsnamesdt2[["display_name"]]
modnewnames <- modnamesdt2[["display_name"]]

#rename vars 
acs2 <- setnames(acs, acsoriginal, acsnewnames)
mod2 <- setnames(mod, modoriginal, modnewnames)

# ------ make sure geographies & years overlap -- not the same length ------
acsgeos <- unique(acs2$GEOID) 
modgeos <- unique(mod2$GEOID)

mod2 <- mod2[,mod_name:=NAME]

#join data by geoid & year to eliminate mismatches & subset data accordingly
acsgeoyr <- acs2[,c("GEOID", "year", "NAME")]
modgeoyr <- mod2[,c("GEOID", "year", "mod_name")]

#join
acsmod <- left_join(acsgeoyr, modgeoyr)
#remove all rows where county_name is null
acsmod2 <- acsmod[!is.na(acsmod$mod_name),]
#create a keep dt to join back to each and filter
keeprows <- acsmod2[,c("GEOID", "year")]
keeprows$keep <- 1
#join back to each table
acsfiltered <- as.data.table(left_join(acs2, keeprows))
acsfiltered2 <- acsfiltered[!is.na(acsfiltered$keep),]

modfiltered <- as.data.table(left_join(mod2, keeprows))
modfiltered2 <- modfiltered[!is.na(modfiltered$keep),]

# ---------- create necessary fields
# --- create county and state fields in contextual data
acs3 <- acsfiltered2[,county_name:=gsub(" .*$", "", NAME)]
acs3 <- acs3[,state_name:=gsub('.*,\\s*', '', NAME)]

#create county field in model data
mod3 <- modfiltered2[,county_name:=NAME]
#create state fips
mod3 <- mod3[,state_fips:=substr(GEOID, start = 1, stop = 2)]
#join to state fips key
mod4 <- left_join(mod3, states, by = c("state_fips" = "fips_code"))

#drop state_fips field from model data
#mod4 <- mod3[,-c("state_fips")]

#drop puerto rico
acs4 <- acs3[acs3$state_name != "Puerto Rico",]
mod5 <- mod4[mod4$state_name != "Puerto Rico",]

#rework cols to keep
acs5 <- acs4[,c(1:3, 54, 53, 4:51)]
mod6 <- mod5[,c(1,3,2,24, 22, 4:19)]

######################################################################################################
# -------------------------------------- wrangle spatial data -------------------------------------- #
######################################################################################################
#transform to Albers Equal Area projection
aeap <- "+proj=aea +lat_1=29.83333333333334 +lat_2=45.83333333333334 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#transform to projected CRS and try simplifying
basegeo2 <- st_transform(basegeo, aeap)

#geometry simplification of spatial layer
basegeo_smpl <- st_simplify(basegeo2, dTolerance = 0.1, preserveTopology = T)

#transform spatial data to wgs84
basegeo_smpl2 <- st_transform(basegeo_smpl, crs = 4326)

#test
#plot(st_geometry(basegeo_smpl2[basegeo_smpl2$NAME == "Bullock",]))

######################################################################################################
# -------------------------------------- create spatial data objects ------------------------------- #
######################################################################################################
#contextual data
context_spatial <- left_join(acs5, basegeo_smpl2, by = c("GEOID"="GEOID"))
#coerce into an sf object
context_spatial <- st_as_sf(context_spatial)

#change a few names
setnames(context_spatial, c("NAME.x", "NAME.y", "geom"), c("dataname","NAME", "geometry"))
st_geometry(context_spatial) <- "geometry"

#modeled data
mod_spatial <- left_join(mod6, basegeo_smpl2, by = c("GEOID"="GEOID"))
#coerce into an sf object
mod_spatial <- st_as_sf(mod_spatial)
#change a few names
setnames(mod_spatial, c("NAME.x", "NAME.y", "geom"), c("dataname","NAME", "geometry"))
st_geometry(mod_spatial) <- "geometry"


# change certain fields into particular classes
context_spatial$`Maternity care access (2016)` <- as.character(context_spatial$`Maternity care access (2016)`)


#test
#plot(st_geometry(context_spatial[context_spatial$state_name == "Maine" & context_spatial$year=="2008",])) 
#plot(st_geometry(mod_spatial[mod_spatial$state_name == "Georgia" & mod_spatial$year=="2008",]))

######################################################################################################
# -------------------------------------- save spatial data objects ------------------------------- #
######################################################################################################
saveRDS(context_spatial, file = paste0(data_repo,"/app_inputs/sf_contextual_data.Rds"))
saveRDS(mod_spatial, file = paste0(data_repo,"/app_inputs/sf_model_data.Rds"))
