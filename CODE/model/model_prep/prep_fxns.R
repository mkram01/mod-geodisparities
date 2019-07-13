##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Functions for setting up a model run
# Date: 5.7.2019
#############################################

## Functions contained in this script:
# -- create model name
# -- directory creation function

# ----------------------------------------------- model name creation function ----------------------------------------
create_modelname <- function(){
  ## Create model name specific to this run from predictors listed in config
  #   Arguments:
  #     
  
  #create model name
  model_name <- paste0(year_start, "_", year_end,"_", ran_slope, ran_int, run_date)

  }


# ----------------------------------------------- directory creation function -------------------------------------------

create_dirs <- function(outdir, model_type, family, outcome, geography, save = TRUE) {
  ## Create directory structure
  #   Arguments:
  #     outdir        = Location where you want output to be written
  #     model_type    = inla?
  #     family        = poisson?
  #     outcome       = outcome var
  #     save          = would you like to save the config as part of the model image history?
  suppressWarnings(
    dir.create(paste0(outdir, '/', model_type))
  )
  suppressWarnings(
    dir.create(paste0(outdir, '/', model_type,'/',family))
  )
  suppressWarnings(
    dir.create(paste0(outdir, '/', model_type,'/',family, '/', outcome))
  )
  
  suppressWarnings(
    dir.create(paste0(outdir, '/', model_type,'/',family, '/', outcome, '/', geography))
  )
  
  out_dir <- paste0(outdir, '/', model_type,'/',family,'/', outcome, '/',geography)
  
  for(dir in c('output','model_image_history')) {
    dir.create(paste0(out_dir,'/',dir), showWarnings = FALSE)
  }
  if(save == TRUE){
    save(config, file=paste0(out_dir, '/model_image_history/', run_date, '.RData'))
  } 
}

