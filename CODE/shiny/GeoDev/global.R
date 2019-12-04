##############################################
# Code author: erin r stearns
# Code objective: setting global args up for dev app
# Date: 1.8.2019
#############################################

# Developed with R version 3.6.1 (64-bit)
  
# --------------------------------------- set up environment ------------------------------------
# load packages
x <- c("shiny", "shinyjs", "rintrojs", "shinyWidgets",    #packages responsible to app performance
       "rgdal", "sf", "raster",                           #spatial data packages
       "gstat",                                           #spatial modeling packages
       "dplyr", "stringr", "OneR", "data.table",          #data handling and basic stats packages
       "leaflet", "leaflet.extras",                       
       "ggplot2", "ggvis",                                
       "plotly",
       "RColorBrewer",                                    # Color palette
       "png", "DT", "stats",                               # png for self-explanatory reasons, DT is an R interface to JS lib DataTables -- R data objects (matrices or data frames) can be displayed as tables on HTML pages, and DataTables provides filtering, pagination, sorting, and many other features in the tables
        "dplyr"
       )

#installing any packages not installed already
new.packages <- x[!(x %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#loading all package libraries
lapply(x, require, character.only = TRUE)

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
#load acs data -- NOTE: currently using this is as the reference dataset for shared functions with model data (e.g. state names vector below, year range for dashboard toggling, etc.)
#context_data <- readRDS('data/sf_acs.Rds')
#create county_data field
#context_data$county_name <- gsub(" .*$", "", context_data$NAME)

#load model data
#mod_data <- readRDS('data/sf_moddata.Rds')

#app data
appdata <- readRDS('data/all-data.rds')

#load in data classification keys for mapping colors & transformations to vars -- data.tables
context_meta <- readRDS('data/contextual-metadata.rds')
mod_meta <- readRDS('data/perinatal-metadata.rds')

#state boundaries
state_bounds <- readRDS('data/stateboundaries.Rds')

#transform state bounds data to wgs84
state_bounds <- st_transform(state_bounds, crs = 4326)


# -------------------------------------- app inputs defined ----------------------------------------
#state choices
state_names <- as.character(unique(appdata$state_name))

# --- variable groupings --- 
#contextual vars
context_groups <- unique(context_meta$category_name)

context_vars <- list()

for (i in 1:length(context_groups)){
  #get title of group -- without quotes
  groupname <- as.name(context_groups[i])
  #get vars for the group
  groupvars <- (unique(context_meta[context_meta$category_name == (groupname),]$display_name))
  
  #create list element and define by vars
  context_vars[[(groupname)]] = list()
  for (x in 1:length(groupvars)){
    context_vars[[(groupname)]][[x]] = groupvars[x]
  }
  
}

#model vars
mod_groups <- unique(mod_meta$category_name)

model_vars <- list()

for (i in 1:length(mod_groups)){
  #get title of group -- without quotes
  groupname <- as.name(mod_groups[i])
  #get vars for the group
  groupvars <- (unique(mod_meta[mod_meta$category_name == (groupname),]$display_name))
  
  #create list element and define by vars
  model_vars[[(groupname)]] = list()
  for (x in 1:length(groupvars)){
    model_vars[[(groupname)]][[x]] = groupvars[x]
  }
  
}

# --- creating vector of continuous variables
mod_continuousvars <<- mod_meta[plot_type %in% c("sequential", "divergent"),]$display_name
context_continuousvars <<- context_meta[plot_type %in% c("sequential", "divergent"),]$display_name
