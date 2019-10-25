##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Placing all model code in one script for testing -- this one recreates spatial objects & matrices
# Date: 9.15.2019
#############################################


rm(list = ls())

######################################################################################################
# ----------------------------------Important notes ------------------------------------------------ #
# Previously, Kevin was able to run using sf objects as the input data, not aspatial data
#
# Most items are defined in the 'To-do!' section except model formulas and INLA model calls
#    - Hardcoded items: the .gpkg sf object loaded in 'load & wrangle spatial data' section
#                     - the shapefile being loaded in 'load & wrangle spatial data' section
#
# Things to try:
#   1. Simply put code all in this script and try to run using same objects as previously used in main architecture 
#                    - Got same error ("Error in inla(f2, family = (family), data = smry_data, offset = log(births),  : 
#                       In f(ID): 'covariate' must match 'values',  and both must either be 'numeric', or 'factor'/'character'.")
#   2. Try using path for adjacency matrix
#                    - M1 ran without error as has done previously
#                    - M2:Got same error from testing within architecture: 
#                          Error in if (n.from.graph <= 0) { : missing value where TRUE/FALSE needed
#                          In addition: Warning message: In inla.read.graph.ascii.internal(..., size.only = size.only) :
#   3. Try using an sf object for data instead of aspatial data
#                   - Noticed that Kevin created 3 different ID columns (ID, ID2, ID3)
#                   - M1 ran without error as has done previously
#                   - M2:  
#   4. Try recreating sf object from data and the adjacency matrix as Kevin did in his original experimental code
######################################################################################################


######################################################################################################
# ---------------------------------- Set up environment -------------------------------------------- #
######################################################################################################
# load packages
pacman::p_load("data.table", "tidyverse", "plyr", "dplyr", #data management packages
               "magrittr",                                 #piping/call sequencing
               "tictoc",                                   #timing functions
               "sf", "sp",                                 #spatial data management packages
               "spdep",                                    #creates spatial weights matrices
               "Rgraphviz",                                #visualizes graph onjects
               "INLA"                                      #modeling package
               )

# set code repo
repo <- Sys.getenv('mod_repo')

# set data repo
data_repo <- Sys.getenv('mod_data')

# load function to get time stamp & time log functions loaded
source('CODE/central_functions/utility_fxns.R')

#start timer for whole script
#tic("Entire script")

# time stamp
run_date <- make_time_stamp()

######################################################################################################
# ---------------------------------- To-do! -------------------------------------------------------- #
# Below are dynamic elements that will dictate how the model will run, where and for who
######################################################################################################
#define aspatial input data
input_data <-  paste0(data_repo, '/model_input/nchs_births/model1.rds')

#define name of pre-created adjacency matrix to use
adjfilename <- "spwts_southatlantic_knn6.adj"

#define name of pre-created adjacency matrix key to use
basefilename <- "spwts_southatlantic_knn6"

#define the geography to run model over
geography <- "southatlantic"

#what is the name of the outcome var you want to model (needs to correspond to a column title in aspatial dataset)
outcome <- "ptb"

#What variable/field is the denominator? Used to make rates (needs to correspond to a column title in aspatial dataset)
denominator <- "births"

#define which race/ethnicities to model
#    1 = Hispanic
#    2 = Non-Hispanic White
#    3 = Non-Hispanic Black
#    4 = Non-Hispanic Al/AN (American Indian, Alaska Native)
#    5 = Non-Hispanic A/PI (Asian/Pacific Islander)"
race_eth <- c(2,3)

#if you would like to recode race into binary variable, what do you want the new var called?
recode_binary <- "black"

#which race/ethnicity would you like to be coded as 1 (thus all else = 0)
binary_code <- 3 # Non-hispanic black

#define years to model over
year_start <- 2007
year_end <- 2016
year_span <- year_start:year_end

######################################################################################################
# ---------------------------------- functions ----------------------------------------------------- #
######################################################################################################
summarise_denominator <- function(x){
  #Function for defining how denominator var will be summarized
  # Input args:
  #     x: character var to apply function over
  # Output: value of formula applied to x
  
  tot <- sum(x) + 1
  return(tot)
}
######################################################################################################
# ---------------------------------- define geography ---------------------------------------------- #
######################################################################################################
# pulled from model_prep/predefined_key.R
suppressWarnings(
  if(geography == "southatlantic"){
    message("From predefined_key.R script: You have selected the South Altantic as your model geography. Assigning vector of FIPS codes now!")
    geo_fips <- c('10', '11', '12','13', '24', '37', '45', '51', '54')
  }
)
######################################################################################################
# ---------------------------------- load & wrangle aspatial data ---------------------------------- #
######################################################################################################
#aspatial births data
smry_data <- readRDS((input_data)) %>% 
  #subsetting to pre-specified model race/ethnicity population defined in  model_prep/load_config.R & formatted in format_config_args.R script
  filter(racehisp_recode %in% (race_eth),
         #subsetting to pre-specified model geography defined in model_prep/predefined_key.R
         substr(combfips,1,2) %in% (geo_fips), 
         #subsetting to pre-specified model year span defined in model_prep/load_config.R & formatted in format_config_args.R script)
         dob_yy %in% (year_span))

#re-coding race/ethnicity to be binary if not false
if (recode_binary != "nonbinary"){
  smry_data <- smry_data %>%
    dplyr::mutate((!!recode_binary) := ifelse(racehisp_recode == (binary_code), 1, 0),
                  combfips = factor(combfips)) %>%
    dplyr::group_by_("dob_yy", "combfips", (recode_binary)) 
}

#Summarise data -- outcome
smry_outcome <- smry_data %>%
  #summarize over vector of outcomes using summary function defined at top
  dplyr::summarise_at(outcome, sum)  # originally Kevin had this be sum(outcome) + 1, but I think we realized this was actually only to happen for the denominator to avoid errors, but just to note

#Summarise data -- denominator
smry_denom <- smry_data %>%
  #summarize over vector of outcomes using summary function defined at top
  dplyr::summarise_at(denominator, summarise_denominator)

#Join into one summary data frame
smry_data <- smry_outcome %>%
  inner_join(smry_denom)

#Load key for mapping aspatial data to later outputs using id -- Dropping this here
#spwts_key <- fread(file = paste0(data_repo, "/model_input/adjacency_matrices/", basefilename, ".csv"), colClasses = c("CHARACTER", "CHARACTER"))
#spwts_key$GEOID <- as.character(spwts_key$GEOID)

#scale year and order by ID and year
smry_data <- smry_data %>%
  #dplyr::left_join(spwts_key, by = c('combfips' = 'GEOID')) %>%
  dplyr::mutate(year_c = dob_yy - (year_start)) #%>%  #scale year using start year in config so intercept interpretable
  #dplyr:: arrange(ID) #removed the dob_yy arrange var

######################################################################################################
# ---------------------------------- load & wrangle spatial data ----------------------------------- #
######################################################################################################
## Read in spatial data
#Read in national county shapefile and save in MOD folder as `.gpkg`.
spatdata_sf <- st_read(paste0(data_repo, '/spatial/cb_2016_us_county_500k.shp')) %>%
  dplyr::filter(STATEFP %in% geo_fips) %>%
  st_transform(102003) # Albers Equal Area

# Saved as `sf` object which is useful for *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.
#spatdata_sp <- st_read(paste0(data_repo, '/spatial/southatlantic_county.gpkg')) %>% # this works too, and then you can comment out the sf object creation in code block above
spatdata_sp <- spatdata_sf %>%
  inner_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(ptb = sum(ptb),
            births = sum(births),
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

# Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
spatdata_sp$ID <- seq_len(nrow(spatdata_sp))

# Create long version (e.g. repeated rows for each year within county) as an
# `sf` object useful for facet printing of year x race.
spatdata_sf <- spatdata_sp %>%
  st_as_sf() %>%
  dplyr::select(GEOID, ID) %>%
  dplyr::right_join(smry_data, by = c('GEOID' = 'combfips')) %>%
  mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
         ID2 = ID) %>%  # ID and ID2 will be for f() in INLA
  dplyr::arrange(ID)


######################################################################################################
# ---------------------------------- load adjacency matrix ----------------------------------------- #
######################################################################################################
model_spwts <- inla.read.graph(paste0(data_repo, "/model_input/adjacency_matrices/", adjfilename))

######################################################################################################
# ---------------------------------- run models ---------------------------------------------------- #
######################################################################################################
#define family
family <- 'poisson'

# ------------------------------------- m1 ----------------------------------------- Works in both scripts
#define the model formula
f1 <- ptb ~ year_c + black + f(ID, model = "iid")

#inla model run
m1 <- inla(f1, family = (family),
           data = spatdata_sf,
           offset = log(births),
           control.predictor = list(link = 1,
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

# ------------------------------------- m2 -----------------------------------------
#define the model formula
f2 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = model_spwts, #"/CODE/model/spwts_all_knn6.adj", 
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.01)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.01))),
    scale.model = T)

#inla model run
m2 <- inla(f2, family = (family),
           data = spatdata_sf,
           offset = log(births),
           control.predictor = list(link = 1,
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

######################################################################################################
# ---------------------------------- process models ------------------------------------------------ #
######################################################################################################

# ------------------------------------- m1 -----------------------------------------
#formatting summary model output
m1_summ <- as.data.frame(m1$summary.fitted.values)

message("From process_model.R script: Joining the posterior of the fitted values to spatial data.")
#join to input data
alldata <- smry_data %>%
  bind_cols(m1_summ)

#create rate fields for poisson family models
alldata <- alldata %>%
  dplyr::mutate(
    raw_rate = (ptb/births),
    model_rate = (mean/births),
    rate_diff = (raw_rate - model_rate), #Deviation from truth
    model_lci = (`0.025quant`/births),
    model_uci = (`0.975quant`/births),
    cred_int = (model_uci - model_lci), #if this is larger than model_rate, then estimate unreliable
    unreliabele = if(cred_int > model_rate){1}else{0} #if credible interval greater than model rate, flag with a 1
  )

# ------------------------------------- m2 -----------------------------------------
m2_summ <- as.data.frame(m2$summary.fitted.values)

message("From process_model.R script: Joining the posterior of the fitted values to spatial data.")
#join to input data
alldata2 <- smry_data %>%
  bind_cols(m2_summ)

#create rate fields for poisson family models
alldata2 <- alldata2 %>%
  dplyr::mutate(
    raw_rate = (ptb/births),
    model_rate = (mean/births),
    rate_diff = (raw_rate - model_rate), #Deviation from truth
    model_lci = (`0.025quant`/births),
    model_uci = (`0.975quant`/births),
    cred_int = (model_uci - model_lci), #if this is larger than model_rate, then estimate unreliable
    unreliabele = if(cred_int > model_rate){1}else{0} #if credible interval greater than model rate, flag with a 1
  )

######################################################################################################
# ---------------------------------- Finalize timer functions -------------------------------------- #
######################################################################################################
# toc(log = T) #end master timer
# 
# #Format timer
# ticlog <- tic.log(format = F)
# df_timer <- generate_time_log(ticlog)

# Pull run time for this run
run_time_all <- df_timer[step == "Entire script", time]

