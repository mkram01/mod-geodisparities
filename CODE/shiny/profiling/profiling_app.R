############################################
# code author: erin stearns
# script objective: profiling shiny app versions
# date: 23 april 2019
###########################################

rm(list = ls())

# Load library
library(profvis)
library(shiny)

#source utility functions
source('functions/utility_fxns.R')

#time stamp for labeling profivs output -- OPTION TO JUST FEED A STRING TO 'today' obj and create descriptive name
today <- make_time_stamp()

# Run profiler on shiny app with optional arg to save output
profvis({ runApp('app/app.R')},  
        prof_output = paste0('profiling/profvis_outdir/', today, "_app.Rprof"))


#save profvis output to html
save_profvis2html(input = paste0('profiling/profvis_outdir/', today, "_app.Rprof"),
                  outdir = 'profiling/profvis_html/')

