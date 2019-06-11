##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Model launch script
# Date: 5.7.2019
#############################################

rm(list = ls())

######################################################################################################
# ---------------------------------- TO-DO ITEMS --------------------------------------------------- #
######################################################################################################
# Set environment variables for:
#   mod_repo: this is the code repo/location of your code pertaining to this model
#   mod_data: this is the file path to where you are storing the model data


######################################################################################################
# ---------------------------------- Set up -------------------------------------------------------- #
######################################################################################################
# load packages
x <- c("data.table", "tidyverse", "sf", "sp","spdep", "tmap", "INLA", "magrittr")
lapply(x, require, character.only = TRUE)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

# load central functions
source('CODE/central_functions/utility_fxns.R')

# load model prep functions
source('CODE/model/model_prep/prep_fxns.R')

# load config
source('CODE/model/model_prep/load_config.R')

# format config args
source('CODE/model/model_prep/format_config_args.R')

# load predefined objects
source('CODE/model/model_prep/predefined_key.R')





#load modeling functions
source("CODE/model/model_prep/model_functions.R")



######################################################################################################
# ---------------------------------- Data load ----------------------------------------------------- #
######################################################################################################
source("CODE/load_data.R")


######################################################################################################
# ---------------------------------- Create directory structure ------------------------------------ #
######################################################################################################
#Create model name (function found in 'prep_fxns.R')

#Create output folder directory if does not exist already
create_dirs(outdir = data_repo,
            model_type = model_type,
            family = family,
            outcome = outcome,
            model = model_name
            geography = geography
            )

######################################################################################################
# ---------------------------------- Run models ---------------------------------------------------- #
######################################################################################################

######################################################################################################
# ---------------------------------- Analyze outputs ----------------------------------------------- #
######################################################################################################
#visualizations

