##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: load data
# Date: 5.7.2019
#############################################

######################################################################################################
# ---------------------------------- Create summarized dataset ------------------------------------- #
######################################################################################################
message("From load_data.R script: Loading aspatial data and prepping for model.")

# ---- load and summarize ----
smry_data <- readRDS(paste0(data_repo, '/nchs_births/R/Data/model1.rda')) %>%
  #subsetting to pre-specified model race/ethnicity population defined in  model_prep/load_config.R & formatted in format_config_args.R script
  filter(racehisp_recode %in% (race_eth),
         #subsetting to pre-specified model geography defined in model_prep/predefined_key.R
         substr(combfips,1,2) %in% (geography), 
         #subsetting to pre-specified model year span defined in model_prep/load_config.R & formatted in format_config_args.R script)
         dob_yy %in% (year_span))

#re-coding race/ethnicity to be binary if not false
if (recode_binary != "FALSE"){
  #recode_binary <- recode_binary
  smry_data <- smry_data %>%
    mutate((!!recode_binary) := ifelse(racehisp_recode == (binary_code), 1, 0),
           combfips = factor(combfips)) %>%
    group_by(dob_yy, combfips, (recode_binary)) 
  
}

#Summarise data
smry_data <- smry_data %>%
  summarise(vptb = sum(vptb) + 1,
                       ptb = sum(ptb) + 1,
                       births = sum(births) + 1)
  

######################################################################################################
# ---------------------------------- Load spatial data --------------------------------------------- #
######################################################################################################

# ---- Prep spatial data for region only ----
#creating save file name based on census division defined in config
cty_sf_name <- paste0(str_sub(geography, start = 1, end = -5), '_county.gpkg')
  
#Read in national county shapefile and save in MOD folder as `.gpkg`.
cty_sf <- st_read(paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp')) %>%
  filter(STATEFP %in% (geography)) %>%
  st_transform(102003) # Albers Equal Area
st_write(cty_sf, paste0(data_repo, '/spatial/', cty_sf_name), delete_dsn = T)
# ------------------------------------------------------------------------------------- START HERE
# ---- Read in spatial data ---

# This is the data saved above. It is saved as `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.

southatlantic_sp <- st_read('../../data/spatial/southatlantic_county.gpkg') %>%
  inner_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
            ptb = sum(ptb),
            births = sum(births),
            rawvptb = vptb / births * 1000,
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

# Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
southatlantic_sp$ID <- seq_len(nrow(southatlantic_sp))

# Create long version (e.g. repeated rows for each year within county) as an
# `sf` object useful for facet printing of year x race.

southatlantic_sf <- southatlantic_sp %>%
  st_as_sf() %>%
  select(GEOID, ID) %>%
  right_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
         ID2 = ID, # ID and ID2 will be for f() in INLA
         year_c = dob_yy - 2007)  %>%# scale year so intercept interpretable
  arrange(ID)


## Create adjacency matrix using k-nearest neighbors
## I'm going to use k-nearest neighbors, with $k=6$. I will force symmetry so
## that if $a$ is neighbors with $b$, then $b$ must also be neighbors with $a$.

# Create knn neighbor object
southatlantic_knn <- southatlantic_sp %>%
  coordinates() %>%  # get centroids
  knearneigh(k = 6) %>% # calculate the 6 nearest neighbors
  knn2nb(sym = T)  # knn neighbor object

# Write an INLA adjacency file
nb2INLA('../../data/spatial/southatlantic_knn6.adj', southatlantic_knn)

# Look at KNN object
summary(southatlantic_knn)
#plot(southatlantic_knn, coords = coordinates(as(cty_sf, 'Spatial')))
#So counties have from 6 to 11 neighbors (>6 because of forced symmetry).

#View INLA object
g <- inla.read.graph('../../data/spatial/southatlantic_knn6.adj')
summary(g)
plot(g)

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
tm_shape(southatlantic_sp) +
  tm_fill('rawptb',
          style = 'quantile') +
  tm_borders()

# INLA Models
# Use regional object object as input data (`nrow(region)`
# rows = `length(unique(region$combfips))` # counties x
# `length(unique(region$dob_yy))` years x
# `length(unique(southatlantic$black))` race groups age categories).



# Check for common gaul lists so we can query premade simple polygons
if(identical(gaul_list, c(29, 42, 45, 47, 50, 66, 90, 94, 106, 105, 144, 155, 159, 181, 182, 214, 217, 221, 243))) {
  message("Your GAUL list matches WSSA, loading premade simple_polygon and assigning subset_shape to global environment...")
  load('/share/geospatial/simple_polygons/wssa.RData')
  assign("subset_shape", subset_shape, envir=globalenv())
  if(makeplots) plot(spoly_spdf)
  if(makeplots) plot(subset_shape, add = TRUE, border = grey(0.5))
  return(list(subset_shape=subset_shape,spoly_spdf=spoly_spdf))
}