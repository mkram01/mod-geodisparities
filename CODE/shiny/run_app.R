############################################
# code author: erin stearns
# script objective: profiling shiny app versions
# date: 23 april 2019
###########################################

rm(list = ls())

# Load library
library(profvis)
library(shiny)
library(htmlwidgets)

#for running GeoDisparities locally
runApp("GeoDisparities")


#for testing and development
#setwd("/home/e/Documents/git_repos/mod-geodisparities/CODE/shiny/GeoDisparities")
