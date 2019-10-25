##############################################
# Code author: Michael Kramer, Erin Stearns
# Code objective: Correct model input data FIPS code issues
# Date: 9.14.2019
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

######################################################################################################
# ---------------------------------- Set up environment -------------------------------------------- #
######################################################################################################
# load packages
pacman::p_load("data.table", "tidyverse", "sf", "sp","spdep","plyr", "dplyr")

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

######################################################################################################
# ---------------------------------- load data ----------------------------------------------------- #
######################################################################################################
orig <- st_read(paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp'))

model_input <- readRDS(paste0(data_repo, '/nchs_births/R/Data/model1.rda'))

######################################################################################################
# ---------------------------------- fix problem FIPS ---------------------------------------------- #
######################################################################################################
# 51515 -> 51019: Bedford City, Virginia (FIPS code = 51515).  Effective July 1, 2013, Bedford city, 
#          Virginia (51515), formerly an independent city, was added to Bedford County (51019.  
#          Beginning with the Vintage 2014 postcensal series, estimates for this county equivalent no 
#          longer appear on the bridged-race population files. Note that data for Bedford city still 
#          appear on NCHS birth and mortality files
#base shapefile (2016)
baseprob <- orig[orig$GEOID == "51019",]
baseprob2 <- orig[orig$GEOID == "51515",] #doesn't exist since shapefile post 2013

#input data
in_prob <- model_input[model_input$combfips == "51019",] #20 rows
in_prob2 <- model_input[model_input$combfips == "51515",] #20 rows

#get 2 related FIPS rows
fipsissue <- model_input[model_input$combfips %in% c("51019","51515"),]
noissue <- model_input[!model_input$combfips %in% c("51019","51515"),]

#redefine value of fips to all be 51019 then sum cols births, ptb, vptb, lptb & mptb
fipsissue2 <- as.data.table(copy(fipsissue))
fipsissue2$combfips <- "51019"
fipsdt <- as.data.frame(fipsissue2[, lapply(.SD, sum, na.rm=TRUE), by=c("dob_yy", "combfips", "racehisp_recode")])

#rejoin to noissue dataframe
alltogether <- rbind(noissue, fipsdt)

######################################################################################################
# ---------------------------------- save ---------------------------------------------------------- #
######################################################################################################
saveRDS(alltogether, file = paste0(data_repo, "/model_input/nchs_births/model1.rds"))
