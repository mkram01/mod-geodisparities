############################################
# code author: erin stearns
# script objective: create spatially referenced dataset of acs indicators
# date: 20 february 2019
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,feather,sf,dplyr)

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
#county spatial data from 2010
counties <- readRDS(paste0(data_repo,"/acs/2010_county_shp/acs_2010_counties.Rds"))

#acs data
aspdata <- as.data.table(read_feather(paste0(data_repo,"/acs/acs5_2007_2017_rtg.feather")))

#empirical bayes output from kevin
data <- readRDS(paste0(data_repo,"/model_output/EBestrates.rda"))


######################################################################################################
# -------------------------------------- wrangle acs data------------------------------------------- #
######################################################################################################
#drop grouping col, will cause issues when casting wide otherwise
aspdata <- aspdata[,-c("grouping"),with=F]

# cast wide on vars, long on years
asp_wide <- dcast.data.table(aspdata, GEOID + year + source + NAME ~ mod_label, value.var = "estimate")

#create state col & remove whitespace
asp_wide[,state_name:=trimws(sapply(strsplit(NAME,","), "[", 2))]

#create state code col
asp_wide[,state_code:=trimws(substr(GEOID, start = 1, stop = 2))]

#rearrange cols
asp_wide <- asp_wide[,c(1,17,18,2:16)]

#one row has NA for state_name and it should be DC
asp_wide[is.na(state_name), state_name:="District of Columbia"]

#saveRDS(asp_wide, file=paste0(data_repo, "/acs/acs5_2007_2017_fin.Rds"))

######################################################################################################
# -------------------------------------- wrangle empirical bayse data------------------------------- #
######################################################################################################
data$FIPS <- as.character(data$FIPS)
data$Year <- as.numeric(as.character(data$Year))
data$`White PTB/1000` <- as.numeric(as.character(data$`White PTB/1000`))
data$`Black PTB/1000` <- as.numeric(as.character(data$`Black PTB/1000`))
data$`Black - White Rate Ratio` <- as.numeric(as.character(data$`Black - White Rate Ratio`))
data$`Black-White Rate Difference` <- as.numeric(as.character(data$`Black-White Rate Difference`))

#join to aspatial data on FIPS/GEIOD & year to get state_name
interim_data <- as.data.table(merge(data, asp_wide, by.x = c("FIPS", "Year"), by.y = c("GEOID", "year"), all.x = T))

#nas remain in state_name, thus will use first 2 digits to get state name
state_codes <- unique(asp_wide[,c('state_name', 'state_code')])

#create state code col in interim data
interim_data[,state_code:=trimws(substr(FIPS, start = 1, stop = 2))]

#join to key from aspatial data
interim2 <- as.data.table(merge(interim_data, state_codes, by.x = "state_code", by.y = "state_code"), all.x=T)

#subset to just cols needed
mod_data <- interim2[,c(names(data), 'state_name.y'),with=F]

#need to wrap back ticks around cols with spaces
# new_names <- paste0("`",names(mod_data),"`")
# new_names[1] <- 'FIPS'
# new_names[2] <- 'Year'
# new_names[7] <- 'state_name'

#ticks arent working, removing all spaces and replacing with _
new_names <- c('FIPS','Year', 
               'White_PTB_per_1000', 'Black_PTB_per_1000',
               'Black_White_Rate_Ratio', "Black_White_Rate_Difference",
               "state_name")

setnames(mod_data, names(mod_data), new_names)

#save
#saveRDS(mod_data, file=paste0(data_repo, "/model_output/eb_rtg.Rds"))

######################################################################################################
# -------------------------------------- create spatial dataset------------------------------------- #
######################################################################################################
#join on GEOID
acs_spatial <- left_join(asp_wide, counties)

#coerce new obj into an sf object
acs_spatial <- st_as_sf(acs_spatial)

#create spatial model output dataset
mod_spatial <- left_join(mod_data, counties, by = c("FIPS" = "GEOID"))

#drop to only needed cols
mod_spatial <- mod_spatial[,c('FIPS','Year', 
                              'White_PTB_per_1000', 'Black_PTB_per_1000',
                              'Black_White_Rate_Ratio', "Black_White_Rate_Difference",
                              "state_name", "geometry"),drop = FALSE]

#coerce new obj into an sf object
mod_spatial <- st_as_sf(mod_spatial)

#transform spatial data to wgs84
acs_spatial <- st_transform(acs_spatial, crs = 4326)
mod_spatial <- st_transform(mod_spatial, crs = 4326)

# for visualizing in app, need to simplify geometries -- doesn't look right
smpl_acs_spatial <- st_simplify(acs_spatial, dTolerance = 0.1)
smpl_mod_spatial <- st_simplify(mod_spatial, dTolerance = 0.1)
# 
# plot(st_geometry(smpl_acs_spatial[smpl_acs_spatial$state_name == "Georgia",]), col = "grey")
# plot(st_geometry(acs_spatial[acs_spatial$state_name == "Georgia",]), add = TRUE, col = "transparent", border = "firebrick")

#calculate centroids -- encountering error here, apparently some geometries may be empty
acs_pts <- st_point_on_surface(acs_spatial)
acs_pts <- st_centroid(acs_spatial)

######################################################################################################
# -------------------------------------- save ------------------------------------------------------ #
######################################################################################################
saveRDS(smpl_acs_spatial, file=paste0(data_repo, "/acs/sf_acs5_2007_2017_w2010counties.Rds"))
#save spatial model data
saveRDS(smpl_mod_spatial, file=paste0(data_repo, "/model_output/eb_spatial.Rds"))



#for running final app script locally:
#for running app locally, start here
setwd("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/mod_geo_mockup/")


