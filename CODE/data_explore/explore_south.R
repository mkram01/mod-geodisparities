##############################################
# Code author: erin r stearns
# Code objective: explore sample MoD data
# Date: 1.8.2019
#############################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################

pacman::p_load(rsconnect, dplyr, data.table, leaflet, sf, ggplot2)

# set code repo
repo <- Sys.getenv('mod_repo')

#setting working directory
setwd(repo)

######################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
south <- st_read(paste0(repo, 'DATA/south.gpkg')) %>%
  mutate(LABEL = paste(NAME, STATE_NAME, sep = ', '))


#subset to only vars to visualize in mock up right now
south2 <- south[,c(1:10,15,19,21,23,147:150),with=F]

#transform all predictors to numeric
south2$PctNoHS <- as.numeric(south2$PctNoHS)
south2$DEN_BL <- as.numeric(south2$DEN_BL)
south2$DEN_WH <- as.numeric(south2$DEN_WH)
south2$DEN_HI <- as.numeric(south2$DEN_HI)
south2$BW_RR_15 <- as.numeric(south2$BW_RR_15)
south2$BW_RD_15 <- as.numeric(south2$BW_RD_15)


st_write(south2, paste0(repo, 'CODE/shiny/mock_up/south_subset.gpkg'))

st_write(south2, '/home/e/Documents/git_repos/learn_all_the_things/R/shiny_mod_dash/data/south_subset.gpkg')


