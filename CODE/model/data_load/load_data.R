##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: load data
# Date: 5.7.2019
#############################################

###################################3
# To-do items:
#   - Make code dynamic for specifying outcome var in the config (lines 36 & 37)
#         - any reformatting of the config arg should take place in 'format_config_args.R'
#   - Aspatial data contains years in a span as specified in the config, however when joining to the sp object spatial data, 
#         year is not taken into account. Either aspatial data needs to be reduced to one year or the spatial 
#         object needs to be joined to the aspatial data and year preserved
#   - create sp objects in advance
#   - output raw data map (need to finish model name composition)



######################################################################################################
# ---------------------------------- Create summarized dataset ------------------------------------- #
######################################################################################################
message("From load_data.R script: Loading aspatial data and prepping for model.")

# ---- load and summarize ----
smry_data <- readRDS(paste0(data_repo, '/nchs_births/R/Data/model1.rda')) %>%
  #subsetting to pre-specified model race/ethnicity population defined in  model_prep/load_config.R & formatted in format_config_args.R script
  filter(racehisp_recode %in% (race_eth),
         #subsetting to pre-specified model geography defined in model_prep/predefined_key.R
         substr(combfips,1,2) %in% (geo_fips), 
         #subsetting to pre-specified model year span defined in model_prep/load_config.R & formatted in format_config_args.R script)
         dob_yy %in% (year_span))

#re-coding race/ethnicity to be binary if not false
if (recode_binary != "Nonbinary"){
  smry_data <- smry_data %>%
    mutate((!!recode_binary) := ifelse(racehisp_recode == (binary_code), 1, 0),
           combfips = factor(combfips)) %>%
    group_by_("dob_yy", "combfips", (recode_binary)) 
}

#Summarise data
smry_data <- smry_data %>%
  summarise(vptb = sum(vptb) + 1, #Outcome var?
                       ptb = sum(ptb) + 1, #Outcome var?
                       births = sum(births) + 1)
  
######################################################################################################
# ---------------------------------- Load spatial data --------------------------------------------- #
######################################################################################################
message("From load_data.R script: Loading spatial data and prepping for model.")
# ---- Prep spatial data for region only ----
#creating save file name based on census division defined in config
cty_sf_name <- paste0(str_sub(geography), '_county.gpkg')
  
#Load spatial data directly or create based upon file existence and/or config preference
if (file.exists(paste0(data_repo, '/spatial/', cty_sf_name)) & create_sf_obj == FALSE){
  message(paste0("From load_data.R script: The spatial data for your geography already exists and you have elected not to recreate it. Loading it now!"))
  spatdata_sf <- st_read(paste0(data_repo, '/spatial/', cty_sf_name))
} else {
  message(paste0("From load_data.R script: Either the spatial data for your geography does not exist or you have elected to recreate it. Creating it now!"))
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
  inner_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
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
  select(GEOID, ID) %>%
  right_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
         ID2 = ID, # ID and ID2 will be for f() in INLA
         year_c = dob_yy - (year_start))  %>% # scale year using start year in config so intercept interpretable
  arrange(ID)

######################################################################################################
# ---------------------------------- Create Adjacency matrix --------------------------------------- #
######################################################################################################
## Create adjacency matrix using k-nearest neighbors if so specifie
## I'm going to use k-nearest neighbors, with $k=6$ as default. I will force symmetry so
## that if $a$ is neighbors with $b$, then $b$ must also be neighbors with $a$.

#name the adjacency file
adjfilename <- paste0(geography, "_knn", k_numneighbors, '.adj')

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

#View INLA object
#summary(model_knn)
#plot(model_knn, coords = coordinates(as(spatdata_sf, 'Spatial')))

# Summary of object
# * n is the size of the graph (e.g. the number of areal units)
# * nnbs are the number of neighbors for each node.
# The summary (names) tells us that there are either $4, 5, 6$ or $7$
# neighbors per node
# (count) says how many counties have each number of neighbors. __Note that
# the * symmetry = T above is what forced some counties to have > 4 neighbors__
# * ncc is about _connected components_ of the graph and this tells us how many are in the list (e.g. only 1)


# Create basemap of raw (pooled) rates
# These are rates pooled across all years and simply serve a reference for
# when we start getting modeled estimates.
# raw_outcome <- tm_shape(spatdata_sp) +
#   tm_fill('rawptb',
#           style = 'quantile') +
#   tm_borders()
# 
# #save
# save_tmap(raw_outcome, paste0())

# INLA Models
# Use regional object object as input data (`nrow(region)`
# rows = `length(unique(region$combfips))` # counties x
# `length(unique(region$dob_yy))` years x
# `length(unique(southatlantic$black))` race groups age categories).

message("From load_data.R script: Finished loading and prepping data!")
