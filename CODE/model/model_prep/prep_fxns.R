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
  model_name <- paste0(recode_binary, "_" )

  }


# ----------------------------------------------- directory creation function -------------------------------------------

create_dirs <- function(outdir, model_type, family, outcome, model, geography, save = TRUE) {
  ## Create directory structure
  #   Arguments:
  #     outdir        = Location where you want output to be written
  #     model_type    = inla?
  #     family        = poisson?
  #     outcome       = outcome var
  #     model         = name of model being run, created in function above
  #     geography     = name of geography being modeled 
  #     save          = would you like to save the config as part of the model image history?
  dir.create(paste0(outdir, '/', model_type,'/',family,'/', outcome, '/',model,'/',geography))
  
  out_dir <- paste0(outdir, '/', model_type,'/',family,'/', outcome, '/', model,'/',geography)
  
  for(dir in c('output','model_image_history')) {
    dir.create(paste0(out_dir,'/',dir), showWarnings = FALSE)
  }
  if(save == TRUE){
    save(config, file=paste0(out_dir, '/model_image_history/', run_date, '_',model, '_', region, '.RData'))
  } 
}

