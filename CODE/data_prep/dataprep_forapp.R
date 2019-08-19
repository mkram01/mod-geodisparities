############################################
# code author: erin stearns
# script objective: create spatially referenced dataset of acs indicators
# date: 19 august 2019
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,feather,sf,dplyr,rgeos, rmapshaper, tmap)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

####################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
#ran object simplification in DATA/acs/prep_for_app, will bring that in once relevant
acs_smpl <- readRDS(paste0(data_repo,"/acs/simpl_acs_spatial.Rds"))
mod_smpl <- readRDS(paste0(data_repo,"/model_output/simpl_eb_spatial.Rds"))

#transform spatial data to wgs84
acs_smpl_v <- st_transform(acs_smpl, crs = 4326)
mod_smpl_v <- st_transform(mod_smpl, crs = 4326)

######################################################################################################
# -------------------------------------- rename field vars ----------------------------------------- #
######################################################################################################
#new model var names
newmodnames <- c("GEOID", "year",'White PTB/1000', 'Black PTB/1000', 
                 'Black - White Rate Ratio', 'Black-White Rate Difference', 
                 "state_name", "geometry")

#new acs var names
newacsnames <- c("GEOID","state_name","state_code", "year", "source", 
                 "NAME", "Black - White Ratio", 
                 "Education - College or higher", "Education - High School or GED", "Education - Less than High School",
                 "Median Income", 
                 "Percent Black", "Percent Hispanic", "Percent White",
                 "Female Poverty Rate", "Household Poverty Rate",
                 "Housing Stability", "Housing Tenure", 
                 "variable", "estimate", "moe", "geometry"
)


#rename model vars
setnames(mod_smpl_v, names(mod_smpl_v), newmodnames)

#rename acs names
setnames(acs_smpl_v, names(acs_smpl_v), newacsnames)


######################################################################################################
# -------------------------------------- join ------------------------------------------------------ #
######################################################################################################
#subset acs data to same time range as modeled data range
acs_sub <- acs_smpl_v[acs_smpl_v$year == 2012 | acs_smpl_v$year == 2016, ]

#join modeled & input data into one dataframe
alldata <- st_join(acs_sub, mod_smpl_v)

# -------------------------- NEED TO COME BACK TO THIS ----------

#need to transform model data to a dataframe
mod_asp <- copy(mod_smpl_v)
mod_asp$geometry <- NULL

#merge aspatial model data to spatial acs
allasp <- merge(acs_sub, mod_asp) # TO-DO: need to figure out why renaming fields weirdly

#test if worked
tm_shape(allasp) +
  tm_fill() + 
  tm_borders()

#rename vars that got wonky
newnames <- c("GEOID","state_name","state_code", "year", "source", 
              "NAME", "Black - White Ratio", 
              "Education - College or higher", "Education - High School or GED", "Education - Less than High School",
              "Median Income", 
              "Percent Black", "Percent Hispanic", "Percent White",
              "Female Poverty Rate", "Household Poverty Rate",
              "Housing Stability", "Housing Tenure", 
              "variable", "estimate", "moe", 
              'White PTB/1000', 'Black PTB/1000', 
              'Black - White Rate Ratio', 'Black-White Rate Difference', 
              "geometry"
              )

setnames(allasp, names(allasp), newnames)

######################################################################################################
# -------------------------------------- save ------------------------------------------------------ #
######################################################################################################
#save full dataset
saveRDS(allasp, file=paste0(data_repo, "/app_inputs/alldata.Rds"))


#douglas-peuker
#saveRDS(smpl_acs_fin, file=paste0(data_repo, "/acs/sf_acs5_2007_2017_w2010counties.Rds"))
#visvalingam's
saveRDS(acs_sub, file=paste0(data_repo, "/acs/sf_acs5_2007_2017_w2010counties_v.Rds"))

#save spatial model data - douglas-peuker 
#saveRDS(smpl_mod_fin, file=paste0(data_repo, "/model_output/eb_spatial.Rds"))
#save spatial model data - visvalingam's 
saveRDS(mod_smpl_v, file=paste0(data_repo, "/model_output/eb_spatial_v.Rds"))