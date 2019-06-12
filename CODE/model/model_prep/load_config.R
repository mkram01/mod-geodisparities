##############################################
# Code author: Erin Stearns
# Code objective: Functions for loading config and calling that function
# Date: 5.7.2019
#############################################

#Message code:
message("From load_config.R script: Loading config file now")

######################################################################################################
# ----------------------------------------------- config function ------------------------------------
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
# ----------------------------------------------- calling config function ----------------------------
######################################################################################################
# Read config file and save all parameters in memory
config <- load_config(data_repo = data_repo)

######################################################################################################
# ----------------------------------------------- setting config defaults ----------------------------
######################################################################################################
#Create defaults
#Message code:
message("From load_config.R script: Setting config defaults if any left empty")
######################################################################################################
# ----------------------------------------------- validating config args -----------------------------
######################################################################################################
#Validate that year_end comes after year_start

message("From load_config.R script: Validating config args")
