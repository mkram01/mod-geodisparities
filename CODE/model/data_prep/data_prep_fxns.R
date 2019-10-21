##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: set of functions to create input data when does not exist already
# Date: 6.23.2019
#############################################

######################################################################################################
# ---------------------------------- summarizing aspatial dataset ---------------------------------- #
######################################################################################################

summarise_denominator <- function(x){
  #Function for defining how denominator var will be summarized
  # Input args:
  #     x: character var to apply function over
  # Output: value of formula applied to x
  
  tot <- sum(x) + 1
  return(tot)
}

summarise_aspatial <- function(input_data){
  #Function for summarizing aspatial dataset to be used as model input data
  # Input args:
  #     input_data: path to input dataset to use as start point
  # Output: summary tibble of data
  
    # ---- load and summarize ----
    smry_data <- readRDS((input_data)) %>% 
      #subsetting to pre-specified model race/ethnicity population defined in  model_prep/load_config.R & formatted in format_config_args.R script
      filter(HISPRACE %in% (race_eth),
             #subsetting to pre-specified model geography defined in model_prep/predefined_key.R
             substr(combfips,1,2) %in% (geo_fips), 
             #subsetting to pre-specified model year span defined in model_prep/load_config.R & formatted in format_config_args.R script)
             dob_yy %in% (year_span))
    
    #re-coding race/ethnicity to be binary if not false
    if (recode_binary != "nonbinary"){
      smry_data <- smry_data %>%
        dplyr::mutate((!!recode_binary) := ifelse(HISPRACE == (binary_code), 1, 0),
                      combfips = factor(combfips)) %>%
        dplyr::group_by_("dob_yy", "combfips", (recode_binary)) 
    }
    
    #Summarise data -- outcome
    smry_outcome <- smry_data %>%
      #summarize over vector of outcomes using summary function defined at top
      dplyr::summarise_at(outcome, sum)
    
    #Summarise data -- denominator
    smry_denom <- smry_data %>%
      #summarize over vector of outcomes using summary function defined at top
      dplyr::summarise_at(denominator, summarise_denominator)
    
    #Join into one summary data frame
    smry_data <- smry_outcome %>%
      inner_join(smry_denom)
  
}

######################################################################################################
# ---------------------------------- create adjacency matrix --------------------------------------- #
######################################################################################################
create_adjmatrix <- function(geography, outpath =  paste0(data_repo, "/model_input/adjacency_matrices/"),
                                        baselayer = paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp')){
  #Function for creating adjacency matrix
  # Input args:
  #           geography : what geography from predefined areas is this for? used to map to geo_fips in predefined_key.R
  #           outpath   : where would you like adjacency matrix outputs saved? default is set
  #           baselayer : base county shapefile to use, default is set
  #Output: an adjacency matrix for that geography specified
  
  
  # ---------------------------------- load base spatial layer --------------------------------------- 
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
    spatdata_sf <- st_read(baselayer) %>%
      filter(STATEFP %in% (geo_fips)) %>%
      st_transform((crs_proj))
    st_write(spatdata_sf, paste0(data_repo, '/spatial/', cty_sf_name), delete_dsn = T)
  }
  
  # ---------------------------------- wrangle spatial data--------------------------------------------
  #converting GEOID to character for ordering
  spatdata_sf$GEOID <- as.character(spatdata_sf$GEOID)
  
  #transform to sp object & ordering on FIPS
  spatdata_sp <- spatdata_sf %>%
    dplyr::arrange(GEOID) %>%
    as('Spatial')
  
  # ---------------------------------- create key for mapping data to INLA output object --------------
  # Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
  spatdata_sp$ID <- row.names(spatdata_sp)
  
  # Create a key mapping ID to FIPS
  spwts_key <- as.data.table(spatdata_sp) %>%
    dplyr::select("GEOID", "ID")
  # ---------------------------------- create adjacency matrix ----------------------------------------
  #create adjacency matrix based on spatial weighting method
  if(sp_weights_method == "knn"){
    model_spwts <- spatdata_sp %>%
      coordinates() %>%  # get centroids
      knearneigh(k = (k_numneighbors)) %>% # calculate the k nearest neighbors 
      knn2nb(sym = T)  # knn neighbor object
  } else {
    message("Erm..you have indicated a spatial weighting method not currently possible in this framework...")
  }
  
  # ---------------------------------- save object & key --------------------------------------------
  #name the adjacency file seeking
  if(sp_weights_method == "knn"){
    basefilename <- paste0("spwts_", geography, "_",sp_weights_method, k_numneighbors)
    adjfilename <- paste0(basefilename,'.adj')
  } else {
    basefilename <- paste0("spwts_", geography, "_",sp_weights_method)
    adjfilename <- paste0(basefilename,'.adj')
    
  }
  
  # Write an INLA adjacency file
  nb2INLA(paste0(outpath, adjfilename), model_spwts)
  
  # Write key as csv too
  write.csv(spwts_key, paste0(outpath, basefilename, ".csv"), row.names = FALSE)
  
}

######################################################################################################
# ---------------------------------- load spatial data for visualization --------------------------- #
######################################################################################################

load_spatialdata <- function(geography, baselayer = paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp')){
  # Function for loading and prepping spatial data into modeling framework
  #   Input args:
  #           geography : what geography from predefined areas is this for? used to map to geo_fips in predefined_key.R
  #           baselayer : base county shapefile to use, default is set
  #   Output:
  
  cty_sf_name <- paste0(str_sub(geography), '_county.gpkg')
  
  #Load spatial data directly or create based upon file existence and/or config preference
  if (file.exists(paste0(data_repo, '/spatial/', cty_sf_name)) & create_sf_obj == FALSE){
    message(paste0("From create_data.R script: The spatial data for your geography already exists and you have elected not to recreate it. Loading it now!"))
    spatdata_sf <- st_read(paste0(data_repo, '/spatial/', cty_sf_name))
  } else {
    message(paste0("From create_data.R script: Either the spatial data for your geography does not exist or you have elected to recreate it. Creating it now!"))
    #Read in national county shapefile and save in MOD folder as `.gpkg`.
    spatdata_sf <- st_read(baselayer) %>%
      dplyt::filter(STATEFP %in% (geo_fips)) %>%
      st_transform((crs_proj))
    st_write(spatdata_sf, paste0(data_repo, '/spatial/', cty_sf_name), delete_dsn = T)
  }
  
  #create ID
  spatdata_sf$ID <- seq_len(nrow(spatdata_sf))
  
  spatdata_sf <- spatdata_sf %>%
    dplyr::select(GEOID, ID) %>%
    inner_join(smry_data, by = c('GEOID' = 'combfips')) %>%
    mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
           ID2 = ID) %>%  # ID and ID2 will be for f() in INLA
    dplyr::arrange(ID)
  
  
}


