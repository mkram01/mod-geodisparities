##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Functions for loading config and calling that function
# Date: 5.7.2019
#############################################


# ----------------------------------------------- config function -------------------------------------------
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

# Read config file and save all parameters in memory
config <- load_config(data_repo = data_repo)

