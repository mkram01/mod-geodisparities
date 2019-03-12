
reg=as.character(commandArgs()[3])
age=as.numeric(commandArgs()[4])
run_date=as.character(commandArgs()[5])
test=as.character(commandArgs()[6])
holdout=as.character(commandArgs()[7])
indicator=as.character(commandArgs()[8])
indicator_group=as.character(commandArgs()[9])

pathaddin = paste0('_bin',age,'_',reg,'_',holdout)
message(pathaddin)
message(run_date)
message(test)

#load an image of the main environment
load(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/pre_run_tempimage_', run_date, pathaddin,'.RData'))
slots = as.numeric(slots)

#reload functions
setwd(repo)
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
package_lib <- paste0(root,'/temp/geospatial/packages') # Library for all MBG versioned packages. Ensures that none of this code is
#   dependent on the machine where the user runs the code.
.libPaths(package_lib)                                  # Ensures packages look for dependencies here when called with library(). Necessary for seeg libraries.
source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
source('mbg_central/post_estimation_functions.R')
source('mbg_central/gbd_functions.R')
source('mbg_central/shiny_functions.R')
source('mbg_central/stacking_functions.R')
source('mbg_central/categorical_variable_functions.R')
source('mbg_central/seegMBG_transform_functions.R')
source('mbg_central/validation_functions.R') 
#source(paste0(repo,'fever/function_tests/update_predict_mbg.R')) #an improved prediction function

# Custom for vaccines
library(tictoc, lib.loc = "/home/j/temp/jmosser/lib/")  # For timing 
#source('vaccine/functions/misc_vaccine_functions.R')    # For timing functions

tic("Entire script") # Start master timer

#call stacking functions

#functions come from the saved imaged
package_list <- c('rgeos', 'data.table','raster','rgdal','INLA','seegSDM','seegMBG','plyr','dplyr','foreign', 'magrittr')
for(package in package_list) {
  library(package, lib.loc = package_lib, character.only=TRUE)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~ Prep MBG inputs/Load Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cores_to_use <- round(slots*.5)
year_list <- eval(parse(text=year_list))

## Load simple polygon template to model over
gaul_list <- get_gaul_codes(reg)
suppressMessages(suppressWarnings(simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list,
                                                                             buffer = 0.4)))
subset_shape     <- simple_polygon_list[[1]]
simple_polygon   <- simple_polygon_list[[2]]

## Load list of raster inputs (pop and simple)
raster_list <- suppressMessages(suppressWarnings(build_simple_raster_pop(subset_shape)))
simple_raster <- raster_list[['simple_raster']]
pop_raster <- raster_list[['pop_raster']]

## Load input data based on stratification and holdout, OR pull in data as normal and run with the whole dataset if holdout == 0.
if(holdout!=0) {
  df <- as.data.table(stratum_qt[[paste0('region__',reg)]])
  oos_df <- df[fold == holdout, ]
  i <- length(unique(oos_df$year))
  periods <- data.frame(group = rep(1:i,5),years = rep(sort(unique(oos_df$year)),5))
  oos_df$period <- match(oos_df$year, periods$years) # add these to df
  df <- df[fold != holdout, ]
}
if(holdout==0) {
  df <- load_input_data(indicator = indicator,
                        simple = simple_polygon,
                        removeyemen = TRUE,
                        pathaddin = pathaddin,
                        years = 'annual',
                        withtag = TRUE,
                        datatag = datatag,
                        use_share = TRUE)
}
## Save distribution of data for this region
png(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date, '/', reg, '.png'))
hist(df[, get(indicator)])
dev.off()

## Define modeling space (right now, just in years)
period_map <- make_period_map(modeling_periods = c(min(year_list):max(year_list))) 

#make covariates conditional
cov_layers = NULL
gbd_cov_layers = NULL
mbg_cov_layers = NULL

# Pull all covariate bricks/layers
if(nchar(fixed_effects)> 0){
  selected_fixed_effects <- strsplit(fixed_effects," ")
  selected_fixed_effects <- selected_fixed_effects[[1]][selected_fixed_effects[[1]] != "+"]
  selected_measures <- strsplit(fixed_effects_measures," ")
  selected_measures <- selected_measures[[1]][selected_measures[[1]] != "+"]
  cov_layers <- load_and_crop_covariates_annual(covs = selected_fixed_effects,                
                                             measures = selected_measures,          
                                             simple_polygon = simple_polygon,
                                             start_year  = min(year_list),
                                             end_year    = max(year_list),
                                             interval_mo = 12,
                                             agebin=1)
}
if(nchar(gbd_fixed_effects)>0){
  selected_gbd_fixed_effects <- strsplit(gbd_fixed_effects," ")
  selected_gbd_fixed_effects <- selected_gbd_fixed_effects[[1]][selected_gbd_fixed_effects[[1]] != "+"]
  # Create list of gaul codes for region + any countries in the data that aren't in region (buffer zone)
  gbd_gaul_list <- unique(c(gaul_convert(unique(df[, country])), gaul_list))
  # Use a layer from the geospatial cov_layers list as a template (raster of simple_polygon) to rasterize GBD cov spdfs.
  gbd_cov_layers <- suppressMessages(suppressWarnings(load_gbd_covariates(gbd_fixed_effects = selected_gbd_fixed_effects, 
                                                                          year_ids = year_list, 
                                                                          gaul_list = gbd_gaul_list,
                                                                          template = cov_layers[[1]][[1]])))
}
if(nchar(mbg_fixed_effects)>0){
  mbg_cov_layers <- suppressMessages(suppressWarnings(load_mbg_covariates(mbg_fixed_effects = mbg_fixed_effects, 
                                                                          simple_polygon = simple_polygon)))
}

## Combine all the covariate layers we want to use and combine our fixed effects formula
#all_cov_layers <- c(cov_layers, gbd_cov_layers, mbg_cov_layers)

#update all cov layers with an indicator variable on country
all_cov_layers <- c(cov_layers, gbd_cov_layers)
source('mbg_central/stacking_functions.R')

if(use_child_country_fes == TRUE | use_inla_country_fes == TRUE){
  fe_gaul_list <- unique(c(gaul_convert(unique(df[, country])), gaul_list))  
  fe_template = cov_layers[[1]][[1]]
  suppressMessages(suppressWarnings(simple_polygon_list <- load_simple_polygon(gaul_list = fe_gaul_list,
                                                                               buffer = 0.4,
                                                                               subset_only = TRUE)))
  fe_subset_shape     <- simple_polygon_list[[1]]
  gaul_code <- rasterize(fe_subset_shape, fe_template, field = 'GAUL_CODE')
  gaul_code = setNames(gaul_code,'gaul_code')
  gaul_code = create_categorical_raster(gaul_code)
  
  #update covlayers and add country fixed effects to the
  all_cov_layers = update_cov_layers(all_cov_layers, gaul_code)
}

#sort out other categorical variables
# if(nchar(categorical_vars) > 0){
#   #convert categorical vars to a multi object vector
#   cat_vars = unlist(strsplit(categorical_vars, " ", fixed = T))
#   cat_ras = lapply(cat_vars, function(x) create_categorical_raster(all_cov_layers[[x]]))
#   all_cov_layers = update_cov_layers(unlist(cat_ras))
#   all_cov_layers = all_cov_layers[!names(all_cov_layers) %in% cat_vars] #remove the pre blocked out ones
# }

#add latitude and longitude as covariates
# lat_ras = setNames(init(all_cov_layers[[1]][[1]], 'y'), 'lat_ras')
# long_ras = setNames(init(all_cov_layers[[1]][[1]], 'x'), 'long_ras')
# space_interact = setNames((lat_ras + abs(cellStats(lat_ras, stat = 'min'))) * (long_ras + abs(cellStats(long_ras, stat = 'min'))), 'space_interact')
# 
# #update all_cov_layers
# all_cov_layers = c(unlist(all_cov_layers), lat_ras = lat_ras, long_ras = long_ras,space_interact = space_interact)

#regenerate all fixed effects equation from the cov layers
all_fixed_effects = paste(names(all_cov_layers), collapse = " + ")
if(use_child_country_fes==FALSE) all_fixed_effects = paste(names(all_cov_layers)[!grepl("gaul_code_", names(all_cov_layers))], collapse = " + ")

###STACK THE THINGS###

tic("Stacking - all") # Start stacking master timer

# Figure out which models we're going to use
child_model_names <- stacked_fixed_effects %>% 
                        gsub(" ", "", .) %>%
                        strsplit(., "+", fixed=T) %>%
                        unlist

the_covs = format_covariates(all_fixed_effects)

#copy the dataset to avoid unintended namespace conflicts
the_data = copy(df)

#shuffle the data into six folds
n_stack_folds <- 5
the_data = the_data[sample(nrow(the_data)),]
the_data[,fold_id := cut(seq(1,nrow(the_data)),breaks=as.numeric(n_stack_folds),labels=FALSE)] #make folds

#extract covariates to the points and subset data where its missing covariate values
cs_covs = extract_covariates(the_data, all_cov_layers, return_only_results = T, centre_scale = T, period_var = 'year', period_map = period_map)

the_data = cbind(the_data, cs_covs[[1]])
covs_cs_df = cs_covs[[2]]

the_data = na.omit(the_data, c(indicator, 'N', the_covs)) #this will drop rows with NA covariate values

#Fit a gam model
if ('gam' %in% child_model_names) {
  tic("Stacking - GAM")     # Start stacking timer - GAM
  gam = fit_gam_child_model(df = the_data, #data frame
                            model_name = 'gam', #identifier for the child model-- needs to be consistent through the steps
                            fold_id_col = 'fold_id',
                            covariates = all_fixed_effects, #rhs formula
                            additional_terms = 'year', #column(s) in df that should be included in the fit. Ideally, there is a raster companion to the column. These columns are not splined
                            weight_column = 'weight', #column in the data frame specifying weights
                            bam =F, #should mgcv::bam be called rather than gam?
                            spline_args = list(bs = 'ts', k = 3), #spline arguments to be applied to the columns specified by the covariates argument
                            auto_model_select =T, #should the function override bam calls in low data situations (boosts probability of convergence)
                            indicator = indicator, #outcome variable column
                            indicator_family = indicator_family, #family of the outcome. Binomial and Gaussian are implemented.
                            cores = 10) #number of compute cores available
  toc(log = T)              # End stacking timer - GAM
}

#Fit a GBM/BRT model
if ('gbm' %in% child_model_names) {
  tic("Stacking - GBM")     # Start stacking timer - GBM
  gbm = fit_gbm_child_model(df = the_data,
                            model_name = 'gbm',
                            fold_id_col = 'fold_id',
                            covariates = all_fixed_effects,
                            weight_column = 'weight',
                            tc = 3, #tree complexity, change back to 4 for real runs
                            lr = 0.005, #learning rate
                            bf = 0.75, #bag fraction
                            indicator = indicator,
                            indicator_family = indicator_family,
                            cores = cores_to_use)
  toc(log = T)              # End stacking timer - GBM
}

#fit some nets
#lasso
if ('lasso' %in% child_model_names) {
  tic("Stacking - lasso")        # Start stacking timer - lasso
  lasso = fit_glmnet_child_model(df = the_data,
                                 model_name = 'lasso',
                                 covariates =all_fixed_effects,
                                 fold_id_col = 'fold_id',
                                 additional_terms = NULL,
                                 indicator_family = indicator_family,
                                 indicator = indicator,
                                 cores = cores_to_use,
                                 alpha = 0,
                                 weight_column = 'weight')
  toc(log = T)                   # End stacking timer - lasso
}

#ridge
if ('ridge' %in% child_model_names) {
  tic("Stacking - ridge")        # Start stacking timer - ridge
  ridge = fit_glmnet_child_model(df = the_data,
                                 model_name = 'ridge',
                                 covariates = all_fixed_effects,
                                 fold_id_col = 'fold_id',
                                 additional_terms = NULL,
                                 indicator_family = indicator_family,
                                 indicator = indicator,
                                 cores = cores_to_use,
                                 alpha = 1,
                                 weight_column = 'weight')
  toc(log = T)                   # End stacking timer - ridge
}

#enet
if ('enet' %in% child_model_names) {
  tic("Stacking - enet")         # Start stacking timer - enet
  enet = fit_glmnet_child_model(df = the_data,
                                model_name = 'enet',
                                covariates = all_fixed_effects,
                                fold_id_col = 'fold_id',
                                additional_terms = NULL,
                                indicator_family = indicator_family,
                                indicator = indicator,
                                cores = cores_to_use,
                                alpha = .5,
                                weight_column = 'weight')
  toc(log = T)                   # End stacking timer - enet
}

#combine the children models
the_data = cbind(the_data, do.call(cbind, lapply(lapply(child_model_names, 'get'), function(x) x[[1]])))
child_model_objs = setNames(lapply(lapply(child_model_names, 'get'), function(x) x[[2]]), child_model_names)

#fit stacker
stacked_results = gam_stacker(the_data, #the dataset in data table format
                              model_names= child_model_names, #prefixes of the models to be stacked
                              indicator = indicator, #the indicator of analysis
                              indicator_family = indicator_family) #indicator family (e.g. binomial)

#return the stacked rasters
stacked_rasters = make_stack_rasters(covariate_layers = all_cov_layers, #raster layers and bricks
                                     period = min(period_map[, period_id]):max(period_map[, period_id]), #period of analysis, NULL returns all periods
                                     child_models = child_model_objs, #model objects fitted to full data
                                     stacker_model = stacked_results[[2]],
                                     indicator_family = indicator_family,
                                     return_children = T,
                                     centre_scale_df = covs_cs_df)

# if(as.logical(gpr_stack)){
#   all_fixed_effects = paste0(names(stacked_rasters[2:length(stacked_rasters)]), collapse = ' + ')
# } else{
#   all_fixed_effects = 'stacked_results'
# }
#all_fixed_effects = 'stacked_results'
#all_fixed_effects = paste('gbm','lasso','ridge','enet', names(gaul_code), sep=" + ")
#eval(parse(text=stacked_fixed_effects))

#all_fixed_effects = paste(stacked_fixed_effects, paste(names(gaul_code), collapse = " + "), sep=" + ")
all_fixed_effects <- stacked_fixed_effects
if(use_inla_country_fes) all_fixed_effects = paste(stacked_fixed_effects, paste(names(gaul_code)[2:length(names(gaul_code))], collapse = " + "), sep=" + ")


#copy things back over to df
df = copy(the_data)

#remove the covariate columns so that there are no name conflicts when they get added back in
df = df[,paste0(the_covs) := rep(NULL, length(the_covs))]

## Double-check that gaul codes get dropped before extracting in save_mbg_input()
df <- df[, grep('gaul_code_*', names(df), value = T) := rep(NULL, length(grep('gaul_code_*', names(df), value = T)))]

## Build spatial mesh over modeling area
mesh_s <- build_space_mesh(d = df,
                           simple = simple_polygon,
                           max_edge = mesh_s_max_edge,
                           mesh_offset = mesh_s_offset)

## Build temporal mesh (standard for now)
mesh_t <- build_time_mesh(periods=eval(parse(text=mesh_t_knots)))

#create a full raster list to carry though to the shiny/next steps
full_raster_list = c(unlist(stacked_rasters),unlist(all_cov_layers))
child_mod_ras = full_raster_list[child_model_names]

toc(log = T) # End stacking master timer

## Save all inputs for MBG model into correct location on /share
save_mbg_input(indicator = indicator, 
               indicator_group = indicator_group,
               df = df,
               simple_raster = simple_raster,
               mesh_s = mesh_s,
               mesh_t = mesh_t, 
               cov_list = full_raster_list,
               pathaddin = pathaddin,
               run_date = run_date,
               child_model_names = child_model_names,
               all_fixed_effects = all_fixed_effects,
               period_map = period_map) #specify by region

#reload data an prepare for MBG
load(paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, pathaddin, '.RData'))

## ~~~~~~~~~~~~~~~~~~~~~~~~ Run MBG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tic("MBG - all") # Start MBG master timer

#for stacking, overwrite the columns matching the model_names so that we can trick inla into being our stacker
df = df[,paste0(child_model_names) := lapply(child_model_names, function(x) get(paste0(x,'_cv_pred')))]

## Generate MBG formula for INLA call
if(use_inla_nugget == TRUE) df$IID.ID <- 1:nrow(df)
mbg_formula <- build_mbg_formula(fixed_effects = all_fixed_effects,
                                 add_nugget = use_inla_nugget)

## Create SPDE INLA stack
input_data <- build_mbg_data_stack(df = df,
                                   fixed_effects = all_fixed_effects,
                                   mesh_s = mesh_s,
                                   mesh_t = mesh_t)

#combine all the inputs
stacked_input <- input_data[[1]]
spde <- input_data[[2]]
cs_df <- input_data[[3]]

# Add nugget if toggled
if(use_inla_nugget==TRUE){
  stacked_input$effects$data$IID.ID <- 1:stacked_input$effects$nrow
  stacked_input$effects$ncol <- c(stacked_input$effects$ncol, 1)
  names(stacked_input$effects$ncol)[length(names(stacked_input$effects$ncol))] <- "IID.ID"
  stacked_input$effects$names[["IID.ID"]] <- "IID.ID"
}

#binomial data
## Generate other inputs necessary
outcome=df[[indicator]] # N+_i - event obs in cluster
N=df$N                  # N_i - total obs in cluster
weights=df$weight

#catch in case there is no weight column
if(is.null(weights)){
  weights = rep(1,nrow(df))
}

tic("MBG - fit model") # Start MBG - model fit timer

## Fit MBG model
source('mbg_central/mbg_functions.R')
model_fit <- fit_mbg(indicator_family = indicator_family,
                     stack.obs = stacked_input,
                     spde = spde,
                     cov = outcome,
                     N = N,
                     int_prior_mn = intercept_prior,
                     f_mbg = mbg_formula,
                     run_date = run_date,
                     keep_inla_files = keep_inla_files,
                     cores = cores_to_use,
                     wgts = weights)

toc(log = T) # End MBG - model fit timer

tic("MBG - predict model") # Start MBG - model predict timer

#predict some MBG
source('mbg_central/mbg_functions.R')

# # Run predict_mbg on chunks of 100 samples (to avoid memory issues)
#   max_chunk <- 100
#   samples <- as.numeric(samples)
#   
#   # Create vector of chunk sizes
#   chunks <- rep(max_chunk, samples %/% max_chunk)
#   if (samples %% max_chunk > 0) chunks <- c(chunks, samples %% max_chunk)
# 
#   predict_mbg <- lapply(chunks, function(samp) {
# 
#     chunk <- predict_mbg(res_fit       = model_fit,
#                          cs_df         = cs_df,
#                          mesh_s        = mesh_s,
#                          mesh_t        = mesh_t,
#                          cov_list      = full_raster_list,
#                          samples       = samp,
#                          simple_raster = simple_raster,
#                          transform     = transform) 
#     return(chunk[[3]])
# 
#     })
# 
#   predict_mbg <- do.call(cbind, predict_mbg)
  
  predict_mbg <- predict_mbg(res_fit       = model_fit,
                             cs_df         = cs_df,
                             mesh_s        = mesh_s,
                             mesh_t        = mesh_t,
                             cov_list      = cov_list,
                             samples       = samples,
                             simple_raster = simple_raster,
                             transform     = transform) 

toc(log = T) # Stop MBG - model predict timer

cell_pred <- predict_mbg[[3]]
rm(predict_mbg)

##################################################################################################
############################### SAVE AND CROSS-VAL ##### #########################################
##################################################################################################
## Save MBG outputs in standard outputs folder structure
save_mbg_preds(config     = config,
               time_stamp = time_stamp,
               run_date   = run_date,
               mean_ras   = NULL,
               sd_ras     = NULL,
               res_fit    = model_fit,
               cell_pred  = cell_pred,
               df         = df,
               pathaddin  = pathaddin)

