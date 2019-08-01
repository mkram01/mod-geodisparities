##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: load data
# Date: 5.7.2019
#############################################

#source create data script containing data creation functions
source("CODE/model/data_prep/data_prep_fxns.R")

######################################################################################################
# ---------------------------------- Create summarized dataset ------------------------------------- #
######################################################################################################
message("From load_data.R script: Loading aspatial data and prepping for model.")

# ---- load and summarize ----
smry_data <- summarise_aspatial(input_data = paste0(data_repo, '/nchs_births/R/Data/model1.rda'))

#######################################################################################################
# ---------------------------------- Load adjacency matrix ------------------------------------------ #
#######################################################################################################
#define name of the adjacency file seeking
basefilename <- paste0("spwts_", geography, "_",sp_weights_method, k_numneighbors)
adjfilename <- paste0(basefilename,'.adj')

#Conditionally load or create spatial weights adjacency matrix
if (file.exists(paste0(data_repo, "/model_input/adjacency_matrices/", adjfilename)) & create_spwts == FALSE){
  message(paste0("From load_data.R script: The adjacency matrix for your geography already exists and you have elected not to recreate it. Loading it now!"))
  #load adjacency matrix
  model_spwts <- inla.read.graph(paste0(data_repo, "/model_input/adjacency_matrices/", adjfilename))
} else {
  message(paste0("From load_data.R script: Either the adjacency matrix for your geography does not exist or you have elected to recreate it. Creating it now!"))
  model_spwts <- create_adjmatrix(geography)
}

#Load key for mapping aspatial data to later outputs using id
spwts_key <- fread(file = paste0(data_repo, "/model_input/adjacency_matrices/", basefilename, ".csv"))
spwts_key$GEOID <- as.factor(spwts_key$GEOID)

#######################################################################################################
# ---------------------------------- Wrangling aspatial data ---------------------------------------- #
#######################################################################################################
#Join aspatial data to spatial weights key & order by key ID
smry_data <- smry_data %>%
  dplyr::left_join(spwts_key, by = c('combfips' = 'GEOID')) %>%
  dplyr::mutate(year_c = dob_yy - (year_start)) %>%  #scale year using start year in config so intercept interpretable
  dplyr:: arrange(ID, dob_yy) #order by ID from spatial weigts key to match to outputs
  

#######################################################################################################
# ---------------------------------- Load spatial data conditionally -------------------------------- #
#######################################################################################################
if (visualize == TRUE){
 message("From load_data.R: You have indicated that you want visualizations so loading in the spatial data now!")
  spatdata_sf <- load_spatialdata(geography)
}

message("From load_data.R script: Finished loading and prepping data!")
