##############################################
# Code author: Erin Stearns
# Code objective: Functions for creating model inputs
# Date: 5.7.2019
#############################################

#Message code:
message("From `create_inputs_fxns.R`` script: Reading in custom functions for creating inputs now!")

######################################################################################################
# ---------------------------------- create adjacency matrix --------------------------------------- #
######################################################################################################
create_adjmatrix <- function(geography, outpath =  paste0(data_repo, "/model_input/adjacency_matrices/"),
                             baselayer = paste0(data_repo, '/model_input/base_geo/all_counties_2017.gpkg')){
  #Function for creating adjacency matrix
  # Input args:
  #           geography : what geography from predefined areas is this for? used to map to geo_fips in predefined_key.R
  #           outpath   : where would you like adjacency matrix outputs saved? default is set
  #           baselayer : base county shapefile to use, default is set
  #Output: an adjacency matrix for that geography specified
  
  
  # ---------------------------------- load base spatial layer --------------------------------------- 
  # ---- Prep spatial data for region only ----
  #creating save file name based on census division defined in config
  cty_sf_name <- paste0(str_sub(geography), '_counties_2017.gpkg')
  
  #Load spatial data directly or create based upon file existence and/or config preference
  if (file.exists(paste0(data_repo, '/model_input/base_geo/', cty_sf_name)) & create_sf_obj == FALSE){
    message(paste0("From create_inputs_fxns.R script: The spatial data for your geography already exists and you have elected not to recreate it. Loading it now!"))
    spatdata_sf <- st_read(paste0(data_repo, '/model_input/base_geo/', cty_sf_name))
  } else {
    message(paste0("From create_inputs_fxns.R script: Either the spatial data for your geography does not exist or you have elected to recreate it. Creating it now!"))
    #Read in national county shapefile and save in MOD folder as `.gpkg`.
    spatdata_sf <- st_read(baselayer) %>%
      dplyr::filter(substr(GEOID, 1,2) %in% (geo_fips)) %>%
      st_transform((crs_proj))
    st_write(spatdata_sf, paste0(data_repo, '/model_input/base_geo/', cty_sf_name), delete_dsn = T)
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
    message("You elected KNN as your spatial weighting method!")
    model_spwts <- spatdata_sp %>%
      coordinates() %>%  # get centroids
      knearneigh(k = (k_numneighbors)) %>% # calculate the k nearest neighbors 
      knn2nb(sym = T)  # knn neighbor object
  } else if (sp_weights_method == "soi") {
    message("You elected the sphere of influence as your spatial weighting method!")
    #create matrix of points
    pt_matrix <- coordinates(spatdata_sp)
    #create triangle neighbor object
    mod_tri <- tri2nb(pt_matrix, row.names = spatdata_sp$GEOID) #calculate neighbors
    #create spatial graph using sphere of influence
    model_spwts <- graph2nb(soi.graph(tri.nb=mod_tri, coords=pt_matrix), row.names = spatdata_sp$GEOID)
  }
  
  # ---------------------------------- save object & key --------------------------------------------
  #name the adjacency file seeking
  if(sp_weights_method == "knn"){
    basefilename <- paste0("spwts_", geography, "_",sp_weights_method, k_numneighbors)
    adjfilename <- paste0(basefilename,'.adj')
  } else if (sp_weights_method == "soi"){
    basefilename <- paste0(geography, "_",sp_weights_method)
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
