############################################
# code author: erin stearns
# script objective: remove 2 problem vars from PRI app data - RUCA & maternity care access
# date: 17 march 2020
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse, geojsonsf, geojsonio, tidyr, plotly, broom)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
#setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

#latest app data
alldata <- readRDS(paste0(data_repo,"/app_inputs/all-data-20feb20.rds"))

#contextual data dictionary
contextual_dd <- readRDS(paste0(data_repo,"/app_inputs/contextual-metadata-20feb20.rds"))

######################################################################################################
# -------------------------------------- remove RUCA & maternity care access ----------------------- #
######################################################################################################
#remove from main dataset
alldata_sans <- alldata %>%
  select(-c("Maternity care access (2016)", "Rural-Urban"))


#remove from contextual data dictionary
contextual_dd_sans <- contextual_dd[!display_name %in% c("Maternity care access (2016)","Rural-Urban"),]

######################################################################################################
# -------------------------------------- save ------------------------------------------------------ #
######################################################################################################
#save app input dataset
saveRDS(alldata_sans, file = paste0(data_repo,"/app_inputs/forPRIapp_noRUCAnoMatCareAccess/all-data-17mar20.rds"))

#save meta data
saveRDS(contextual_dd_sans, file = paste0(data_repo,"/app_inputs/forPRIapp_noRUCAnoMatCareAccess/contextual-metadata-17mar20.rds"))
