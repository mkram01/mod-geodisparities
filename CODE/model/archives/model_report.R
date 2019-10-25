##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: create model run report with graphical representations of inputs & model outputs
# Date: 6.23.2019
#############################################

###################################3
# To-do items:
#   - Make into a nice RMarkdown report

######################################################################################################
# ---------------------------------- Model input graphics ------------------------------------------ #
######################################################################################################
message("From model_report.R script: Summarizing INLA object.")

#View INLA object
summary(model_knn)
plot(model_knn, coords = coordinates(as(spatdata_sf, 'Spatial')))

# Summary of object
# * n is the size of the graph (e.g. the number of areal units)
# * nnbs are the number of neighbors for each node.
# The summary (names) tells us that there are either $4, 5, 6$ or $7$
# neighbors per node
# (count) says how many counties have each number of neighbors. __Note that
# the * symmetry = T above is what forced some counties to have > 4 neighbors__
# * ncc is about _connected components_ of the graph and this tells us how many are in the list (e.g. only 1)

message("From model_report.R script: Basemap of raw (pooled) rates.")
# Create basemap of raw (pooled) rates
# These are rates pooled across all years and simply serve a reference for
# when we start getting modeled estimates.
raw_outcome <- tm_shape(spatdata_sp) +
  tm_fill('rawptb',
          style = 'quantile') +
  tm_borders()
# 
# #save
# save_tmap(raw_outcome, paste0())

# INLA Models
# Use regional object object as input data (`nrow(region)`
# rows = `length(unique(region$combfips))` # counties x
# `length(unique(region$dob_yy))` years x
# `length(unique(southatlantic$black))` race groups age categories).

######################################################################################################
# --------------------------------- Model output graphics ------------------------------------------ #
######################################################################################################

#potenitally change layout so comparing raw and modeled rates

######################################################################################################
# ---------------------------------- render and save report ---------------------------------------- #
######################################################################################################

#render

#save


message("From model_report.R script: Finished model report -- see output folder!")
