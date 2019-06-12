##############################################
# Code author: erin r stearns
# Code objective: scrap paper for code experimentation/development
# Date: 5.26.2019
#############################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table, tidycensus, tidyr, tidyverse,dplyr, feather, sf, tmap, RColorBrewer,
               binr,OneR)


######################################################################################################
# -------------------------------------- load data ------------------------------------------------- 
######################################################################################################
#load spatial data
geodata <- readRDS('mod_geo_mockup/data/sf_acs5_2007_2017_w2010counties_v.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)


#prep subset for testing
#subset to contiguous US for visualization purposes
usa_contig <- geodata[!geodata$state_name %in% c("Alaska", "Hawaii", "Puerto Rico"),]
#bounding box for plotting
bb_contig <- st_bbox(usa_contig)

#spatial model data
geomod <- readRDS('mod_geo_mockup/data/eb_spatial_v.Rds')
#transform spatial data to wgs84
geomod <- st_transform(geomod, crs = 4326)
modtest <- copy(geomod)




######################################################################################################
# -------------------------------------- scrapping ------------------------------------------------- #
######################################################################################################

#creating quantiles by desired bin number
testbins <- bins.quantiles(usa_contig$blackwhite_ratio, 5, 5)

plot(usa_contig["blackwhite_ratio"])
plot()


usa_contig$quintiles <- bin(usa_contig$blackwhite_ratio, nbins = 5, method = "content")
usa_contig$quintiles <- bin(usa_contig$blackwhite_ratio, nbins = 10, method = "content")

plot(usa_contig["quintiles"])


modtest$quintiles <- bin(modtest$Black_PTB_per_1000, nbins = 5, method = "content")
plot(modtest["quintiles"])
