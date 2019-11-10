############################################
# code author: erin stearns
# script objective: prep app inputs - contextual vars & model outcomes
# date: 24 october 2019
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,sf,dplyr,rgeos, tmap)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

######################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
#ran object simplification in DATA/acs/prep_for_app, will bring that in once relevant
acs <- readRDS(paste0(repo,"/CODE/shiny/GeoDisparities/data/sf_acs5_2007_2017_w2010counties_v.Rds"))
mod <- readRDS(paste0(repo,"/CODE/shiny/GeoDisparities/data/eb_spatial_v.Rds"))

#transform spatial data to wgs84 -- already have the correct projection
# acs_smpl_v <- st_transform(acs_smpl, crs = 4326)
# mod_smpl_v <- st_transform(mod_smpl, crs = 4326)

#drop puerto rico
acs2 <- acs[acs$state_name != "Puerto Rico",]
mod2 <- mod[mod$state_name != "Puerto Rico",]

######################################################################################################
# -------------------------------------- align data ------------------------------------------------ #
######################################################################################################
#make sure same time periods covered
if (length(unique(acs2$year)) != length(unique(mod2$year))) {
  message("You do not have matching time periods between the ACS and modeled datasets.")
}
#mod only has 2012 & 2016 currently, so subsetting ACS accordingly
acs3 <- acs2[acs2$year %in% c(2012, 2016),]

#make sure same geographies covered
if (length(unique(acs3$GEOID)) != length(unique(mod2$GEOID))) {
  message("You do not have matching geographies covered between the ACS and modeled datasets.")
}

#how many don't match?
acsfips <- unique(acs3$GEOID)
modfips <- unique(mod2$GEOID)
nomatch_mod <- modfips[!modfips %in% acsfips] #4
nomatch_acs <- acsfips[!acsfips %in% modfips] #2

match_mod <- modfips[modfips %in% acsfips]
match_acs <- acsfips[acsfips %in% modfips]
#check
length(match_mod[!match_mod %in% match_acs])
length(match_acs[!match_acs %in% match_mod])

#subset accordingly
mod3 <- mod2[mod2$GEOID %in% match_mod,]
acs4 <- acs3[acs3$GEOID %in% match_acs,]

#scan for NAs
scan_for_nullvals <- function(dt, var){
  nullvalcount <- (sum(is.na((dt)[(var)])))[1] 
  message(paste0((nullvalcount), " null values for: ", var))
}

#NAs in mod3
for (i in names(mod3)){
  scan_for_nullvals(mod3, i)
}

#NAs in acs4 -- appears there are 5 rows missing geometry vals -- need to investigate this later
for (i in names(acs4)){
  scan_for_nullvals(acs4, i)
}

######################################################################################################
# -------------------------------------- save objects ---------------------------------------------- #
######################################################################################################
#save acs data
saveRDS(acs4, file=paste0(repo,"/CODE/shiny/GeoDisparities/data/sf_acs.Rds"))

#save mod data
saveRDS(mod3, file=paste0(repo,"/CODE/shiny/GeoDisparities/data/sf_moddata.Rds"))

