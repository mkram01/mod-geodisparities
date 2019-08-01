##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Pre-create model input objects
# Date: 7.30.2019
#############################################

rm(list = ls())

######################################################################################################
# ---------------------------------- Set up -------------------------------------------------------- #
######################################################################################################
# load packages
x <- c("data.table", "tidyverse", "sf", "sp","spdep","plyr", "dplyr")

#installing any packages not installed already
new.packages <- x[!(x %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#loading all package libraries
lapply(x, require, character.only = TRUE)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')
#set your modeler name (should be yours!)
modeler <- Sys.getenv('mod_modeler')

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

# load config -- need some args here to properly subset the data
source('CODE/model/model_prep/load_config.R')

# format config args
source('CODE/model/model_prep/format_config_args.R')

# load predefined objects
source('CODE/model/model_prep/predefined_key.R')

#source create data prep functions
source("CODE/model/data_prep/data_prep_fxns.R")



######################################################################################################
# ---------------------------------- Create summarized dataset ------------------------------------- #
######################################################################################################
# ---- load and summarize ----
smry_data <- aspatial_smry(input_data = paste0(data_repo, '/nchs_births/R/Data/model1.rda'))




#compare to: ref_tbl
ref_tbl <- copy(smry_data)






######################################################################################################
# ---------------------------------- Load spatial data --------------------------------------------- #
######################################################################################################
message("From create_data.R script: Loading spatial data and prepping for model.")

# ---- Prep spatial data for region only ----
#creating save file name based on census division defined in config
cty_sf_name <- paste0(str_sub(geography), '_county.gpkg')

#Load spatial data directly or create based upon file existence and/or config preference
if (file.exists(paste0(data_repo, '/spatial/', cty_sf_name)) & create_sf_obj == FALSE){
  message(paste0("From create_data.R script: The spatial data for your geography already exists and you have elected not to recreate it. Loading it now!"))
  spatdata_sf <- st_read(paste0(data_repo, '/spatial/', cty_sf_name))
} else {
  message(paste0("From create_data.R script: Either the spatial data for your geography does not exist or you have elected to recreate it. Creating it now!"))
  #Read in national county shapefile and save in MOD folder as `.gpkg`.
  spatdata_sf <- st_read(paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp')) %>%
    filter(STATEFP %in% (geo_fips)) %>%
    st_transform((crs_proj))
  st_write(spatdata_sf, paste0(data_repo, '/spatial/', cty_sf_name), delete_dsn = T)
}

#Prep spatial data
# The data was created as an `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.
spatdata_sp <- spatdata_sf %>%
  dplyr::inner_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(vptb = sum(vptb),
                   ptb = sum(ptb),
                   births = sum(births),
                   rawvptb = vptb / births * 1000,
                   rawptb = ptb / births * 1000) %>%
  as('Spatial')

# Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
spatdata_sp$ID <- seq_len(nrow(spatdata_sp))

# Create long version (e.g. repeated rows for each year within county) as an
# `sf` object useful for facet printing of year x race.
spatdata_sf <- spatdata_sp %>%
  st_as_sf() %>%
  dplyr::select(GEOID, ID) %>%
  dplyr::right_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  dplyr::mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
                ID2 = ID, # ID and ID2 will be for f() in INLA
                year_c = dob_yy - (year_start))  %>% # scale year using start year in config so intercept interpretable
  dplyr::arrange(ID)

#######################################################################################################
# ---------------------------------- Load adjacency matrix ------------------------------------------ #
######################################################################################################

#name the adjacency file seeking
adjfilename <- paste0("knn_", geography, "_", k_numneighbors, '.adj')

#Conditionally load or create KNN
if (file.exists(paste0(data_repo, '/spatial/', adjfilename)) & create_knn_obj == FALSE){
  message(paste0("From load_data.R script: The KNN adjacency matrix for your geography already exists and you have elected not to recreate it. Loading it now!"))
  #load adjacency matrix
  model_knn <- inla.read.graph(paste0(data_repo, '/spatial/', adjfilename))
} else {
  message(paste0("From load_data.R script: Either the KNN adjacency matrix for your geography does not exist or you have elected to recreate it. Creating it now!"))
  # Create knn neighbor object
  model_knn <- spatdata_sp %>%
    coordinates() %>%  # get centroids
    knearneigh(k = (k_numneighbors)) %>% # calculate the k nearest neighbors 
    knn2nb(sym = T)  # knn neighbor object
  # Write an INLA adjacency file
  nb2INLA(paste0(data_repo, '/spatial/', adjfilename), model_knn)
}


message("From load_data.R script: Finished loading and prepping data!")
