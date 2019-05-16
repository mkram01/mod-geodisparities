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
pacman::p_load(data.table, tidycensus, tidyr, tidyverse,dplyr, feather, sf, tmap, viridis, RColorBrewer)

######################################################################################################
# -------------------------------------- load data ------------------------------------------------- 
######################################################################################################
#load spatial data
geodata <- readRDS('mod_geo_mockup/data/sf_acs5_2007_2017_w2010counties_v.Rds')
#transform spatial data to wgs84
geodata <- st_transform(geodata, crs = 4326)

######################################################################################################
# -------------------------------------- make tmap --------------------------------------------------- 
######################################################################################################
# -- going to animate the black-white ratio
# rename
geo <- setnames(geodata, "blackwhite_ratio", "Black-White Ratio")

#explore var
summary(geo$`Black-White Ratio`)

#subset to contiguous US for visualization purposes
usa_contig <- geo[!geo$state_name %in% c("Alaska", "Hawaii", "Puerto Rico"),]
#bounding box for plotting
bb_contig <- st_bbox(usa_contig)
#take a look
plot(usa_contig["median_income"])

#create palette
div.pal = viridis::viridis(n = 8, direction = -1)
div.pal <- brewer.pal(9, "RdBu")

#create breaks
pb = c(0, .006, 0.02, 0.17, 1, 2, 4, 6, 7)

#create map
facet_anim <-  tm_shape(geo, bbox = bb_contig) +
  tm_polygons("Black-White Ratio", colorNA = NULL, palette = div.pal, breaks = pb) +
  tm_facets(free.scales.fill = FALSE, ncol = 1, nrow = 1, along = "year") +
  tm_shape(usa_contig) +
  tm_borders(lwd = 2) +
  tm_layout(legend.position = c("left", "bottom"))

#create animation
tmap_animation(tm = facet_anim, filename = "animation/blackwhiteratio.gif")
  