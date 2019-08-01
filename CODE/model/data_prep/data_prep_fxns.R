##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: set of functions to create input data when does not exist already
# Date: 6.23.2019
#############################################

######################################################################################################
# ---------------------------------- summarizing aspatial dataset ---------------------------------- #
######################################################################################################

sum_fxn <- function(x){
  #Function for defining how outcome vars will be summarized
  # Input args:
  #     x: vector or value to apply function over
  # Output: value of formula applied to x
  
  tot <- sum(x) + 1
  return(tot)
}

aspatial_smry <- function(input_data){
  #Function for summarizing aspatial dataset to be used as model input data
  # Input args:
  #     input_data: path to input dataset to use as start point
  # Output: summary tibble of data
  
    # ---- load and summarize ----
    smry_data <- readRDS((input_data)) %>% 
      #subsetting to pre-specified model race/ethnicity population defined in  model_prep/load_config.R & formatted in format_config_args.R script
      filter(racehisp_recode %in% (race_eth),
             #subsetting to pre-specified model geography defined in model_prep/predefined_key.R
             substr(combfips,1,2) %in% (geo_fips), 
             #subsetting to pre-specified model year span defined in model_prep/load_config.R & formatted in format_config_args.R script)
             dob_yy %in% (year_span))
    
    #re-coding race/ethnicity to be binary if not false
    if (recode_binary != "Nonbinary"){
      smry_data <- smry_data %>%
        dplyr::mutate((!!recode_binary) := ifelse(racehisp_recode == (binary_code), 1, 0),
                      combfips = factor(combfips)) %>%
        dplyr::group_by_("dob_yy", "combfips", (recode_binary)) 
    }
    
    #Summarise data
    smry_data<- smry_data %>%
      #summarize over vector of outcomes using summary function defined at top
      dplyr::summarise_at(outcome, sum_fxn)
  
}

######################################################################################################
# ---------------------------------- create adjacency matrix --------------------------------------- #
######################################################################################################
