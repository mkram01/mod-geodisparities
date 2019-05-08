##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Functions for setting up a model run
# Date: 5.7.2019
#############################################

## Functions contained in this script:
# -- time stamp
# -- directory creation function
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


# ----------------------------------------------- directory creation function -------------------------------------------

create_dirs <- function(outdir, model_type, family, model, geography, save = TRUE) {
  ## Create directory structure
  #   Arguments:
  #     outdir          = Location where you want output to be written
  #     model         = name of model being run
  #     region        = name of region being modeled  
  dir.create(paste0(outdir, '/', model_type,'/',family,'/',model,'/',geography, '/Output/'))
  
  geography_out_dir <- paste0(outdir, '/', model_type,'/',family,'/',model,'/',geography, '/Output/')
  
  for(dir in c('output','model_image_history')) {
    dir.create(paste0(geography_out_dir,'/',dir), showWarnings = FALSE)
  }
  if(save == TRUE){
    save(config, file=paste0(geography_out_dir, '/model_image_history/', run_date, '_',model, '_', region, '.RData'))
  } 
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