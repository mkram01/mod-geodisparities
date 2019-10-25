##############################################
# Code author: Erin Stearns
# Code objective: Functions for wrangling model inputs 
# Date: 5.7.2019
#############################################

#Message code:
message("From `wrangle_inputs_fxns.R` script: Reading in custom functions for wrangling model inputs now!")

######################################################################################################
# ---------------------------------- summarizing aspatial dataset ---------------------------------- #
######################################################################################################
summarise_denominator <- function(x){
  #Function for defining how denominator var will be summarized
  # Input args:
  #     x: character var to apply function over
  # Output: value of formula applied to x
  
  tot <- sum(x) + 1
  return(tot)
}

summarise_aspatial <- function(input_data){
  #Function for summarizing aspatial dataset to be used as model input data
  # Input args:
  #     input_data: path to input dataset to use as start point
  # Output: summary tibble of data
  
  # ---- load and summarize ----
  smry_data <- readRDS((input_data)) %>% 
    #subsetting to pre-specified model race/ethnicity population defined in  model_prep/load_config.R & formatted in format_config_args.R script
    filter(HISPRACE %in% (race_eth),
           #subsetting to pre-specified model geography defined in model_prep/predefined_key.R
           substr(GEOID,1,2) %in% (geo_fips), 
           #subsetting to pre-specified model year span defined in model_prep/load_config.R & formatted in format_config_args.R script)
           DOB_YY %in% (year_span))
  
  #re-coding race/ethnicity to be binary if not false
  if (recode_binary != "nonbinary"){
    smry_data <- smry_data %>%
      dplyr::mutate((!!recode_binary) := ifelse(HISPRACE == (binary_code), 1, 0),
                    GEOID = factor(GEOID)) %>%
      dplyr::group_by_("DOB_YY", "GEOID", (recode_binary)) 
  }
  
  if (recode_binary == "nonbinary"){
    smry_data <- smry_data %>%
    dplyr::mutate(GEOID = factor(GEOID)) %>%
    dplyr::group_by_("DOB_YY", "GEOID", "HISPRACEf") 
    
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
  
}
