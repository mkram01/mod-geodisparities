##############################################
# Code author: Erin Stearns
# Code objective: Utility functions that can be used across scenarios
# Date: 5.7.2019
#############################################

## Functions contained in this script:
# -- time stamp
# -- timer functions


# ----------------------------------------------- time stamp function -------------------------------------------

make_time_stamp <- function(time_stamp = TRUE) {
  # Function to standardize time stamp format 
  #   Input args:
  #           time_stamp = TRUE or FALSE, defaults to TRUE
  #   Output: character string of date and time
  #           precise output: "<year>_<month>_<day>_<hour>_<minute>_<second>"
  
  if(time_stamp == TRUE){
    
    run_date <- gsub("-","_",Sys.time())
    run_date <- gsub(":","_",run_date)
    run_date <- gsub(" ","_",run_date)  
    
  } else {
    run_date <- 'scratch'
  }
  
  return(run_date)
  
}

# ----------------------------------------------- timer functions -------------------------------------------
generate_time_log <- function(ticlog) {
  
  # Set up packages
  require(magrittr)
  require(data.table)
  
  # Functions in functions
  strip_time <- function(x) {
    sec <- as.numeric(x$toc - x$tic)
    time <- format_time(sec)
    name <- x$msg
    
    df <- c(name, time) %>%
      t %>%
      as.data.table
    
    names(df) <- c("step", "time")
    
    return(df)
  }
  
  format_time <- function(run_time) {
    run_time <- round(as.numeric(run_time),0)
    
    hours <- run_time %/% 3600
    remainder <- run_time %% 3600
    minutes <- remainder %/% 60
    seconds <- remainder %% 60
    
    run_time <- paste0(hours, "h ", minutes, "m ", seconds, "s")
    return(run_time)
  }
  
  df_out <- lapply(ticlog, strip_time) %>% rbindlist
  
  return(df_out)
  
}