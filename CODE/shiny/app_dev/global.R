##############################################
# Code author: erin r stearns
# Code objective: create MoD shiny mock up - global script
# Date: 1.8.2019
#############################################

#####################################################
# --------------- TO DO ITEMS -----------------------
# - Make auto-subsetting in response to map panning
# - Make map plot 2 vals in -- base poly color and then centroid graduated symbols
# - add Github link if open source
# ---------------------------------------------------


######################################################################################################
# -------------------------------------- set up ---------------------------------------------------- #
######################################################################################################
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(rgdal)
library(sf)
library(ggplot2)
library(ggvis)
library(shinydashboard)
library(dplyr)
library(fontawesome)
require(raster)
require(gstat)

# -------------------------------------- load data ------------------------------------------------- 
#load spatial data
geodata <- readRDS('mod_geo_mockup/data/sf_acs5_2007_2017_w2010counties_v.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

#load aspatial data
adata <- readRDS('mod_geo_mockup/data/acs5_2007_2017_fin.Rds')

#model data
moddata <- readRDS('mod_geo_mockup/data/eb_rtg.Rds')

#spatial model data
geomod <- readRDS('mod_geo_mockup/data/eb_spatial_v.Rds')
#transform spatial data to wgs84
geomod <- st_transform(geomod, crs = 4326)

# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(adata$state_name))

#state choices for modeled data
mod_state_names <- as.character(unique(moddata$state_name))