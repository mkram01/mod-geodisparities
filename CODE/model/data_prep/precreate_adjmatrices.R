##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Pre-create adjacency matrices
# Date: 7.30.2019
#############################################

rm(list = ls())

######################################################################################################
# ---------------------------------- To-do! -------------------------------------------------------- #
######################################################################################################

# USER!! WARNING!

# The following base county shapefile being used is from 2016, so if you would like another year or version, you will need to change
#   your base shapefile being loaded to create the adjacency matrix by defining the 'baselayer' arg as the path to the new shapefile
#   you would like to use
#   Current default: baselayer = paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp')

# if having any FIPS matching issues, see model README documentation

# --- Arguments to define:

#  What method will you use for calculating spatial weights?
sp_weights_method <- "knn"

#  If KNN, How many neighbors?
k_numneighbors <- 6

#  do you want to re-create the base sf object if it doesn't exist? true/false (may want to do this if using new base county spatial layer)
create_sf_obj <- FALSE

#  what crs would you like?
crs_proj <- 4269

#  geo -- a character vector containing matching values to pre-defined values, see config_README for more info
#            -- this vector is what will be looped over to batch-create adjacency matrices
geo <- c("south","west","midwest","northeast",
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

#source data prep function script
source("CODE/model/data_prep/data_prep_fxns.R")

#define path to output folder for saving objects if want different from default (defined in function and commented out below)
#outpath <- paste0(data_repo, "/model_input/adjacency_matrices/")

######################################################################################################
# ---------------------------------- Create adjacency matrices iteratively ------------------------- #
######################################################################################################

for (g in geo){
  
  #define geography arg for other functions
  geography <- g
  
  # load predefined objects to define geography fips key
  source('CODE/model/model_prep/predefined_key.R')
  
  #create spatial weights matrix -- keeping outpath & baselayer defaults
  model_spwts <- create_adjmatrix(geography)
  
}



