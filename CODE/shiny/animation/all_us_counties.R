############################################
# code author: erin stearns
# script objective: create animations to use in Shiny app (non-interactive)
# date: 15 May 2019
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table, tidycensus, tidyr, tidyverse,dplyr, feather, sf, tmap)
