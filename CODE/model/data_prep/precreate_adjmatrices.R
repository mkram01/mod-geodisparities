##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Pre-create adjacency matrices
# Date: 7.30.2019
#############################################

rm(list = ls())

######################################################################################################
# ---------------------------------- To-do! -------------------------------------------------------- #
######################################################################################################
# --- Arguments to define:

#  What method will you use for calculating spatial weights?
sp_weights_method <- "knn"

#  If KNN, How many neighbors?
k_numneighbors <- 6

#  do you want to re-create the base sf object if it doesn't exist? true/false (may want to do this if using new base county spatial layer)
create_sf_obj <- FALSE

#  what crs would you like?
crs_proj <- 4269

#  geography -- a character vector containing matching values to pre-defined values, see config_README for more info
#            -- this vector is what will be looped over to batch-create adjacency matrices
geography <- c("south","west","midwest","northeast",
               "newengland","midatlantic","southatlantic","esc","wsc","enc","wnc","pac","mountain",
               "all")

# If you would like to run using args you have in your config, un-comment the following (most helpful if only trying to create one object)
# # load config -- need some args here to properly subset the data
# source('CODE/model/model_prep/load_config.R')
# # format config args
# source('CODE/model/model_prep/format_config_args.R')

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

#define path to output folder for saving objects
outpath <- paste0(data_repo, "/model_input/adjacency_matrices/")

#Loop each geography to produce all objects
for (g in geography){
  # load predefined objects to define geography fips key
  source('CODE/model/model_prep/predefined_key.R')
  
  ######################################################################################################
  # ---------------------------------- load base spatial layer --------------------------------------- #
  ######################################################################################################
  # ---- Prep spatial data for region only ----
  #creating save file name based on census division defined in config
  cty_sf_name <- paste0(str_sub(g), '_county.gpkg')
  
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
  
  #converting GEOID to character for ordering
  spatdata_sf$GEOID <- as.character(spatdata_sf$GEOID)
  
  #transform to sp object
  spatdata_sp <- spatdata_sf %>%
    dplyr::arrange(GEOID) %>%
    as('Spatial')
  
  
  #old code
  #Prep spatial data
  # The data was created as an `sf` object which is useful for
  # *long* format (e.g. multiple years), but also want an `sp` object for creating
  # *neighbor* objects and simpler *wide* representations.
  # spatdata_sp <- spatdata_sf %>%
  #   dplyr::inner_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  #   dplyr::group_by(GEOID) %>%
  #   dplyr::summarise(vptb = sum(vptb),
  #                    ptb = sum(ptb),
  #                    births = sum(births),
  #                    rawvptb = vptb / births * 1000,
  #                    rawptb = ptb / births * 1000) %>%
  #   as('Spatial')
  
  # Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
  spatdata_sp$ID <- row.names(spatdata_sp)
  
  # Create a key mapping ID to FIPS
  spwts_key <- as.data.table(spatdata_sp) %>%
    dplyr::select("GEOID", "ID")
  
  ######################################################################################################
  # ---------------------------------- create adjacency matrix --------------------------------------- #
  ######################################################################################################
  #name the adjacency file seeking
  basefilename <- paste0("spwts_", g, "_",sp_weights_method, k_numneighbors)
  adjfilename <- paste0(basefilename,'.adj')
  
  #create object
  if(sp_weights_method == "knn"){
    model_spwts <- spatdata_sp %>%
      coordinates() %>%  # get centroids
      knearneigh(k = (k_numneighbors)) %>% # calculate the k nearest neighbors 
      knn2nb(sym = T)  # knn neighbor object
  } else {
    message("Erm..you have indicated a spatial weighting method not currently possible in this framework...")
  }
  
  
  # Write an INLA adjacency file
  nb2INLA(paste0(outpath, adjfilename), model_spwts)
  
  # Write key as csv too
  write.csv(spwts_key, paste0(outpath, basefilename, ".csv"), row.names = FALSE)
  
}


