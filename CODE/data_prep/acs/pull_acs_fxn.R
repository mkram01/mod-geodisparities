############################################
# code author: erin stearns
# script objective: function for obtaining ACS via the US Census Bureau's APIs as dictated by a config file
# date: 22 january 2019
###########################################

######################################################################################################
# -------------------------------------- function for extracting acs data using config  ------------ #
######################################################################################################
pull_acsdata <- function(group, yr){
  # Function for pulling ACS data via the US Census bureau API using a custom config file specifying necessary
  #           arguments.
  # Args:
  #   group   : string value corresponding to field in config entitled "grouping" -- which group do you want to 
  #                   extract data for?
  #   yr      : string value corresponding to the year seeking data from
  
  # ----------------------------------- subset -------------------------------------------------------
  #subset config to grouping vars
  config_sub <- config[grouping == (group) & year == (yr),] 
  
  # ----------------------------------- make key -----------------------------------------------------
  #reduce config to get acs name - field names key
  config_names <- config_sub[,c('mod_label','grouping','acs_name'),with=F]
  
  # ----------------------------------- grab dat data! -----------------------------------------------
  #get relevant data
  acs_data <- get_acs(geography = (geo),
                      variables = as.vector(config_sub$acs_name),
                      year = (as.numeric(yr)),
                      survey = (acs_survey)
  ) %>% 
    
    #join to config key to get readable var names
    left_join(config_names, by = c("variable"="acs_name")) %>%
    
    #convert to data table
    as.data.table()
  
  #return(acs_data_coll)
  return(acs_data)
  
}