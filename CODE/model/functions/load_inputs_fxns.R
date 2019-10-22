##############################################
# Code author: Erin Stearns
# Code objective: Functions for loading model inputs (e.g. config)
# Date: 5.7.2019
#############################################

#Message code:
message("From `load_inputs_fxns.R` script: Reading in custom functions for loading model inputs now!")

######################################################################################################
# ----------------------------------------------- load config function -------------------------------
######################################################################################################
load_config <- function(data_repo) {
  # Load parameters from config file into memory
  #   Arguments:
  #     repo            = Location where you've cloned the model repository
  #     disease         = name of disease being modeled
  config <- fread(paste0(data_repo, '/model_input/config.csv'), header=FALSE)
  for(param in config[, V1]) {
    assign(param, config[V1==param, V2], envir=globalenv())
  }
  return(config)  
}

######################################################################################################
# ---------------------------------- load spatial data for visualization --------------------------- #
######################################################################################################

load_spatialdata <- function(geography, baselayer = paste0(data_repo, '/model_input/base_geo/all_counties_2017.gpkg')){
  # Function for loading and prepping spatial data into modeling framework
  #   Input args:
  #           geography : what geography from predefined areas is this for? used to map to geo_fips in predefined_key.R
  #           baselayer : base county shapefile to use, default is set
  #   Output:
  
  cty_sf_name <- paste0(str_sub(geography), '_counties_2017.gpkg')
  
  #Load spatial data directly or create based upon file existence and/or config preference
  if (file.exists(paste0(data_repo, '/model_input/base_geo/', cty_sf_name)) & create_sf_obj == FALSE){
    message(paste0("From create_data.R script: The spatial data for your geography already exists and you have elected not to recreate it. Loading it now!"))
    spatdata_sf <- st_read(paste0(data_repo, '/model_input/base_geo/', cty_sf_name))
  } else {
    message(paste0("From create_data.R script: Either the spatial data for your geography does not exist or you have elected to recreate it. Creating it now!"))
    #Read in national county shapefile and save in MOD folder as `.gpkg`.
    spatdata_sf <- st_read(baselayer) %>%
      dplyr::filter(substr(GEOID, 1,2) %in% (geo_fips)) %>%
      st_transform((crs_proj))
    st_write(spatdata_sf, paste0(data_repo, '/model_input/base_geo/', cty_sf_name), delete_dsn = T)
  }
  
  #create ID
  spatdata_sf$ID <- seq_len(nrow(spatdata_sf))
  
  spatdata_sf <- spatdata_sf %>%
    dplyr::select(GEOID, ID) %>%
    inner_join(smry_data, by = c('GEOID' = 'GEOID')) %>%
    mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
           ID2 = ID) %>%  # ID and ID2 will be for f() in INLA
    dplyr::arrange(ID)
  
  
}



