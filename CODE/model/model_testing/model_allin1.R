##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Placing all model code in one scritp for testing
# Date: 9.15.2019
#############################################


rm(list = ls())

######################################################################################################
# ---------------------------------- To-do! -------------------------------------------------------- #
######################################################################################################
#define the geography to run model over
geography <- "southatlantic"

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
tic("Entire script")

# time stamp
run_date <- make_time_stamp()

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
  dplyr::summarise_at(outcome, sum)

#Summarise data -- denominator
smry_denom <- smry_data %>%
  #summarize over vector of outcomes using summary function defined at top
  dplyr::summarise_at(denominator, summarise_denominator)

#Join into one summary data frame
smry_data <- smry_outcome %>%
  inner_join(smry_denom)

#


######################################################################################################
# ---------------------------------- wrangle aspatial data ----------------------------------------- #
######################################################################################################
southatlantic <- readRDS(paste0(data_repo, '/nchs_births/R/Data/model1.rda')) %>%
  filter(racehisp_recode %in% c(2,3),
         substr(combfips,1,2) %in% southatlanticfips,
         dob_yy %in% 2007:2016) %>%
  mutate(black = ifelse(racehisp_recode == 3, 1, 0),
         combfips = factor(combfips)) %>%
  group_by(dob_yy, combfips, black) %>%
  summarise(vptb = sum(vptb) + 1,
            ptb = sum(ptb) + 1,
            births = sum(births) + 1)