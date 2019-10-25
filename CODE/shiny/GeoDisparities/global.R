##############################################
# Code author: erin r stearns
# Code objective: setting global args up for app
# Date: 1.8.2019
#############################################

# Developed with R version 3.4.4 (64-bit)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(rgdal)
library(sf)
library(ggplot2)
library(ggvis)
library(dplyr)
#library(fontawesome)
require(raster)
require(gstat)
library(stringr)
library(png)
library(shinyjs)
library(DT) #provides an R interface to the JavaScript library DataTables. R data objects (matrices or data frames) can be displayed as tables on HTML pages, and DataTables provides filtering, pagination, sorting, and many other features in the tables
library(rintrojs)
library(OneR)

# Calling carousel script -- not sure how this is being used
source("carouselPanel.R")

# Calling bivariate scatterplot function script
source("ggplotRegression.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

# -------------------------------------- load data ------------------------------------------------- 
#load spatial data
#geodata <- readRDS('data/sf_acs5_2007_2017_w2010counties_v.Rds')
geodata <- readRDS('data/alldata.Rds')

# NOTE: for Alabama talk swapping out 'newdata.Rds' for 'alldata.Rds'
#geodata <- readRDS('data/alldata.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

#state boundaries
state_bounds <- readRDS('data/stateboundaries.Rds')
#transform spatial data to wgs84
state_bounds <- st_transform(state_bounds, crs = 4326)

#aspatial data for plots
# adata <- copy(geodata)
# adata$geometry <- NULL

#load aspatial data
#adata <- readRDS('data/acs5_2007_2017_fin.Rds')

# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(geodata$state_name))
