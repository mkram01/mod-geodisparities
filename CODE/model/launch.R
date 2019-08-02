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
#   mod_modeler: this is a character string with your name for use in the model report
######################################################################################################
# ---------------------------------- Set up -------------------------------------------------------- #
######################################################################################################
# load packages
x <- c("data.table", "tidyverse", "sf", "sp","spdep", "tmap", "INLA", "magrittr", "tictoc",
       "plyr", "dplyr", "rmarkdown")
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

# load central functions
source('CODE/central_functions/utility_fxns.R')

#start timer for whole script
tic("Entire script")

# time stamp
run_date <- make_time_stamp()

# load model prep functions
source('CODE/model/model_prep/prep_fxns.R')

# load config
source('CODE/model/model_prep/load_config.R')

# format config args
source('CODE/model/model_prep/format_config_args.R')

# load predefined objects
source('CODE/model/model_prep/predefined_key.R')

#source data prep function script
source("CODE/model/data_prep/data_prep_fxns.R")

#load modeling functions
#source("CODE/model/model_prep/model_functions.R")

######################################################################################################
# ---------------------------------- Data load ----------------------------------------------------- #
######################################################################################################
tic("Data loading")
source("CODE/model/data_load/load_data.R")
toc(log = T) #end data loading timer
######################################################################################################
# ---------------------------------- Create directory structure ------------------------------------ #
######################################################################################################
tic("Naming model & creating directory.")
#Create model name (function found in 'prep_fxns.R')
modname <- create_modelname()

#Create output folder directory if does not exist already (function found in 'prep_fxns.R')
outpar <- paste0(data_repo, "/model_output")
create_dirs(outdir = outpar,
            model_type = model_type,
            family = family,
            outcome = outcome,
            geography = geography
            )
#defining where outputs should be written
outdir <- paste0(outpar, '/', model_type,'/',family,'/', outcome, '/',geography,"/output/")

toc(log = T) #end model naming and directory creation timer
######################################################################################################
# ---------------------------------- Run models ---------------------------------------------------- #
######################################################################################################
tic("Running model")
source("CODE/model/model_scripts/run_model.R")
toc(log = T) #end model run timer

######################################################################################################
# ---------------------------------- Analyze outputs ----------------------------------------------- #
######################################################################################################
#process model outputs for analysis
tic("Processing model outputs")
source("CODE/model/model_scripts/process_model.R")
toc(log = T)

#visualizations
if(visualize == TRUE){
  tic("Creating visualizations and final report")
  rmarkdown::render("CODE/model/visualization/model_report.Rmd", output_dir = outdir, output_file = paste0(modname, "_report.html"))
  toc(log = T) #end visualizations timer
}

######################################################################################################
# ---------------------------------- Finalize timer functions -------------------------------------- #
######################################################################################################
toc(log = T) #end master timer

#Format timer
ticlog <- tic.log(format = F)
df_timer <- generate_time_log(ticlog)

# Pull run time for this run
run_time_all <- df_timer[step == "Entire script", time]

## Write to a run summary csv file in the output directory -- fix this
output_file <- paste0(outdir, modname, "_run_summary.csv")
write.csv(df_timer, output_file)

