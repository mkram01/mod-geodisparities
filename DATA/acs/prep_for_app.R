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
pacman::p_load(data.table,feather,sf,dplyr,rgeos,rmapshaper)

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

#try simplifying geometry with rmapshaper
# acs_smpl <- rmapshaper::ms_simplify(acs_spatial, keep = 0.01, keep_shapes = TRUE)
# mod_smpl <- rmapshaper::ms_simplify(mod_spatial, keep = 0.01, keep_shapes = TRUE)
# #save
# saveRDS(acs_smpl, file = "C:/Users/erinste/Desktop/test/simpl_acs_spatial.Rds")
# saveRDS(mod_smpl, file = "C:/Users/erinste/Desktop/test/simpl_eb_spatial.Rds")

#having issues running this on ubuntu machine, so ran elsewhere, loading simplified objects back in
acs_smpl <- readRDS(paste0(data_repo,"/acs/simpl_acs_spatial.Rds"))
mod_smpl <- readRDS(paste0(data_repo,"/model_output/simpl_eb_spatial.Rds"))

#transform spatial data to wgs84
acs_smpl_v <- st_transform(acs_smpl, crs = 4326)
mod_smpl_v <- st_transform(mod_smpl, crs = 4326)


# for visualizing in app, need to simplify geometries -- doesn't look right --- need to project first
smpl_acs_spatial <- st_simplify(acs_spatial, dTolerance = 0.1, preserveTopology = T)
smpl_mod_spatial <- st_simplify(mod_spatial, dTolerance = 0.1, preserveTopology = T)

#plot
#base gray is douglas-peuker method
plot(st_geometry(smpl_acs_spatial[smpl_acs_spatial$state_name == "Georgia",]), col = "grey") 
#visvalingam's algorithm
plot(st_geometry(acs_smpl[acs_smpl$state_name == "Georgia",]), col = "transparent", border="blue", add = T)
#true base geometry
plot(st_geometry(acs_spatial[acs_spatial$state_name == "Georgia",]), add = TRUE, col = "transparent", border = "firebrick")

#transform to Albers Equal Area projection
aeap <- "+proj=aea +lat_1=29.83333333333334 +lat_2=45.83333333333334 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#transform to projected CRS and try simplifying
new_acs <- st_transform(acs_spatial, aeap)
new_mod <- st_transform(mod_spatial, aeap)

#simplify
smpl_acs_spatial2 <- st_simplify(new_acs, dTolerance = 0.1, preserveTopology = T)
smpl_mod_spatial2 <- st_simplify(new_mod, dTolerance = 0.1, preserveTopology = T)

#base gray is douglas-peuker method -- worked much better with projected data
plot(st_geometry(smpl_acs_spatial2[smpl_acs_spatial2$state_name == "Georgia",]), col = "grey") 

#testing if transformed back to a non-projected crs
smpl_acs_fin <- st_transform(smpl_acs_spatial2, crs = 4326)
smpl_mod_fin <- st_transform(smpl_mod_spatial2, crs = 4326)

#worked
plot(st_geometry(acs_test[acs_test$state_name == "Georgia",]), col = "grey") 

#transform spatial data to wgs84
#acs_spatial <- st_transform(acs_spatial, crs = 4326)
#mod_spatial <- st_transform(mod_spatial, crs = 4326)


#calculate centroids -- encountering error here, apparently some geometries may be empty
acs_pts <- st_point_on_surface(acs_spatial)
acs_pts <- st_centroid(acs_spatial)

######################################################################################################
# -------------------------------------- save ------------------------------------------------------ #
######################################################################################################
#douglas-peuker
saveRDS(smpl_acs_fin, file=paste0(data_repo, "/acs/sf_acs5_2007_2017_w2010counties.Rds"))
#visvalingam's
saveRDS(acs_smpl_v, file=paste0(data_repo, "/acs/sf_acs5_2007_2017_w2010counties_v.Rds"))

#save spatial model data - douglas-peuker 
saveRDS(smpl_mod_fin, file=paste0(data_repo, "/model_output/eb_spatial.Rds"))
#save spatial model data - visvalingam's 
saveRDS(mod_smpl_v, file=paste0(data_repo, "/model_output/eb_spatial_v.Rds"))



#for running final app script locally:
#for running app locally, start here
setwd("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/mod_geo_mockup/")


