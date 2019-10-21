## Logit functions
logit <- function(x) {
  log(x/(1-x))
}
invlogit <- function(x) {
  exp(x)/(1+exp(x))
}

# save_mbg_input <- function(indicator, indicator_group, df, simple_raster, mesh_s, mesh_t, cov_list, pathaddin="") {
#
#   # Split list of covariates into a brick of non-time-varying covariates and separate bricks for each time-varying covariate.
#   covs <- stack()
#   for(i in 1:length(cov_list)) {
#     if(length(names(cov_list[[i]]))==1) { # One layer indicates non-time-varying
#       covs <- stack(cov_list[[i]], covs)
#     }
#     else { # More than one layer indicates time-varying
#         cov_name <- gsub(".1", "", names(cov_list[[i]])[1])
#         assign(paste0("tv_", cov_name), cov_list[[i]])
#       }
#     }
#   covs <- brick(covs)
#
#   if(dim(covs)[3]==0) TV_only = TRUE else TV_only = FALSE
#
#   # make periods for all surveys
#   i <- length(unique(df$year))
#   periods <- data.frame(group = rep(1:i,5),years = rep(sort(unique(df$year)),5)) # NOTE:  why is this repeating 5 times?
#   df$period <- match(df$year, periods$years) # add these to df
#
#   # ~~~~~~~~~~~~~~~
#   # sort covariates
#
#   # extract cluster covariates
#   df$longitude<-as.numeric(df$longitude)
#   df$latitude <-as.numeric(df$latitude )
#
#   # remove missing values
#   #df <- na.omit(df)
#
#   if(!TV_only){
#     # extract cluster level covariates
#     cluster_covs <- as.data.frame(extract(covs, df[,c('longitude', 'latitude'),with=F]))
#
#     # add to dataframe
#     df <- cbind(df, cluster_covs)
#   }
#
#   # add dummy column for time-varying covariates
#   TVnames = c()
#   for(l in ls()[grep('tv_',ls())]){
#     df[,substr(l,4,nchar(l))]=NA
#     TVnames[length(TVnames)+1]=substr(l,4,nchar(l))
#   }
#
#   # loop through time varying covariates to insert them
#   for (period in sort(unique(periods$group))) {
#     for(l in TVnames){
#
#       # find matching rows
#       l=paste0('tv_',l)
#       message(paste0(l,period))
#       idx_tv<- which(df$period == period)
#
#       # get raster
#       craster  <- get(l)[[period]]
#       crs(craster)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
#       # extract values
#       extr <- extract(craster, df[idx_tv, c('longitude', 'latitude'),with=F])
#
#       # add into df, on gam transformed scale (already in it)
#       df[[substr(l,4,nchar(l))]][idx_tv]=extr
#
#     }
#   }
#
#   # Save df with covariates merged, time-varying covariate bricks and single brick of all non-time-varying covariates, meshes, and an empty raster to project model.
#   # Along with the hyperparamters in the config file, these are all the inputs needed to run an MBG model.
#   to_save <- c("df", "simple_raster", ls()[grep('^tv_',ls())], "covs", "mesh_s", "mesh_t")
#   save(list = to_save, file = paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, pathaddin, '.RData'))
#   message(paste0('All inputs saved to /share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, pathaddin,'.RData'))
#
# }

save_mbg_input <- function(indicator = indicator, indicator_group = indicator_group, df = df , simple_raster = simple_raster, mesh_s = mesh_s,
                           mesh_t = mesh_t, cov_list = all_cov_layers, run_date = NULL, pathaddin="",
                           child_model_names = NULL, all_fixed_effects = NULL, period_map = NULL,centre_scale = T) {

  just_covs <- extract_covariates(df, cov_list, return_only_results = T, centre_scale = centre_scale, period_var = 'year', period_map = period_map)
  if(centre_scale==TRUE){
    just_covs <- just_covs$covs
  }
  just_covs <- just_covs[, year := NULL]
  just_covs <- just_covs[, period_id := NULL]
  df = cbind(df, just_covs)

  #create a period column
  if(is.null(period_map)) {
    period_map <- make_period_map(c(2000,2005,2010,2015))
  }
  df[, period := NULL]
  setnames(period_map, 'data_period', 'year')
  setnames(period_map, 'period_id', 'period')
  df = merge(df, period_map, by = 'year', sort =F)

  ## Now that we've extracted covariate values to our data in the buffer zone, clip cov list to simple_raster instead of simple_polygon
  ##    (simple_raster is area we actually want to model over)
  for(l in 1:length(cov_list)) {
    cov_list[[l]]  <- crop(cov_list[[l]], extent(simple_raster))
    cov_list[[l]]  <- setExtent(cov_list[[l]], simple_raster)
    cov_list[[l]]  <- mask(cov_list[[l]], simple_raster)
  }

  ## Save all inputs
  to_save <- c("df", "simple_raster", "mesh_s", "mesh_t", 'cov_list', 'child_model_names', 'all_fixed_effects','period_map')
  save(list = to_save, file = paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, pathaddin, '.RData'))
  message(paste0('All inputs saved to /share/geospatial/mbg/', indicator_group, '/', indicator, '/model_image_history/', run_date, pathaddin,'.RData'))

}

transform_theta <- function(theta) {(exp(theta)-1) / (1+exp(theta))}

test_rho_priors <- function(mean,sd) {
  prec <- (1/sd)^2
  message(paste0('theta1_prior_prec: ', round(prec, 2)))
  lower <- transform_theta(mean - (1.96*sd))
  upper <- transform_theta(mean + (1.96*sd))
  message(paste0('95% range in Rho prior: ', round(transform_theta(mean), 2), ' (', round(lower, 2), ' - ', round(upper, 2), ')'))
}

build_mbg_formula_with_priors <- function(fixed_effects,
                                          positive_constrained_variables=NULL,
                                          interact_with_year=NULL,
                                          int=TRUE,
                                          nullmodel=FALSE,
                                          add_nugget=FALSE,
                                          temporal_model_type="'ar1'",
                                          theta_prior_type="'loggamma'",
                                          theta_prior_mean=1,
                                          theta_prior_sd=0.00005,
                                          theta1_prior_type="'normal'",
                                          theta1_prior_mean=0,
                                          theta1_prior_sd=2.58
){

  # Set up model equation
  if(int==TRUE) intercept='+int' else intercept =''
  f_null  <- formula(paste0('covered~-1',intercept))

  if(!is.null(interact_with_year)){
    for(iwy in interact_with_year){
      fixed_effects <- gsub(iwy,paste0(iwy,'* factor(period)'),fixed_effects)
    }
    if(!is.null(positive_constrained_variables))
      stop("Cannot Both Constrain and Interact, one must be NULL.")
  }


  if(!is.null(positive_constrained_variables)){
    if(!all(positive_constrained_variables %in% strsplit(fixed_effects,' \\+ ')[[1]]))
      stop('Not all your positive_constrained_variables match fixed_effects')
    for(pcv in positive_constrained_variables){
      v <- sprintf("f(%s, model='clinear',range=c(0,Inf),initial=0)",pcv)
      fixed_effects <- gsub(pcv,v,fixed_effects)
    }
  }

  f_nugget <- ~ f(IID.ID, model='iid')

  test_rho_priors(theta1_prior_mean, theta1_prior_sd) ## Report how priors for theta1 (Rho) are being used in Rho space.
  theta1_prior_prec <- (1/theta1_prior_sd)^2 ## Transform SD in theta1 space to precision in theta1 space, which is what INLA uses for Rho.
  f_space <- as.formula(paste0('~f(space,
                                   model = spde,
                                   group = space.group,
                                   control.group = list(model = ', temporal_model_type, ", hyper = list(theta = list(prior = ", theta_prior_type,
                               ", param = c(", theta_prior_mean, ", ", theta_prior_sd, ")), theta1 = list(prior = ", theta1_prior_type,
                               ", param = c(", theta1_prior_mean, ", ", theta1_prior_prec, ")))))"))

  if(!nullmodel){
    f_lin <- reformulate(fixed_effects)
    f_mbg = f_null + f_lin + f_space
    if(add_nugget==TRUE) f_mbg = f_null + f_lin + f_space + f_nugget
  } else {
    f_mbg = f_null + f_space
  }
  message(f_mbg)
  return(f_mbg)
}

build_mbg_formula <- function(fixed_effects,
                              positive_constrained_variables=NULL,
                              interact_with_year=NULL,
                              int=TRUE,
                              nullmodel=FALSE,
                              add_nugget=FALSE) {

  # Set up model equation
  if(int==TRUE) intercept='+int' else intercept =''
  f_null  <- formula(paste0('covered~-1',intercept))

  if(!is.null(interact_with_year)){
    for(iwy in interact_with_year){
      fixed_effects <- gsub(iwy,paste0(iwy,'* factor(period)'),fixed_effects)
    }
    if(!is.null(positive_constrained_variables))
      stop("Cannot Both Constrain and Interact, one must be NULL.")
  }


  if(!is.null(positive_constrained_variables)){
    if(!all(positive_constrained_variables %in% strsplit(fixed_effects,' \\+ ')[[1]]))
      stop('Not all your positive_constrained_variables match fixed_effects')
    for(pcv in positive_constrained_variables){
      v <- sprintf("f(%s, model='clinear',range=c(0,Inf),initial=0)",pcv)
      fixed_effects <- gsub(pcv,v,fixed_effects)
    }
  }

  f_nugget <- ~ f(IID.ID, model='iid')

  f_space <- ~ f(
    space,
    model = spde,
    group = space.group,
    control.group = list(model = 'ar1'))

  if(!nullmodel){
    f_lin <- reformulate(fixed_effects)
    f_mbg = f_null + f_lin + f_space
    if(add_nugget==TRUE) f_mbg = f_null + f_lin + f_space + f_nugget
  } else {
    f_mbg = f_null + f_space
  }
  message(f_mbg)
  return(f_mbg)
}


# spde priors function
local.inla.spde2.matern.new = function(mesh, alpha=2, prior.pc.rho,
                                       prior.pc.sig)
{
  # Call inla.spde2.matern with range and standard deviationparametrization
  d = INLA:::inla.ifelse(inherits(mesh, "inla.mesh"), 2, 1)
  nu = alpha-d/2
  kappa0 = log(8*nu)/2
  tau0   = 0.5*(lgamma(nu)-lgamma(nu+d/2)-d/2*log(4*pi))-nu*kappa0
  spde   = inla.spde2.matern(mesh = mesh,
                             B.tau   = cbind(tau0,   nu,  -1),
                             B.kappa = cbind(kappa0, -1, 0))

  # Change prior information
  param = c(prior.pc.rho, prior.pc.sig)
  spde$f$hyper.default$theta1$prior = "pcspdega"
  spde$f$hyper.default$theta1$param = param
  spde$f$hyper.default$theta1$initial = log(prior.pc.rho[1])+1
  spde$f$hyper.default$theta2$initial = log(prior.pc.sig[1])-1

  # End and return
  return(invisible(spde))
}

build_mbg_data_stack <- function(df, fixed_effects, mesh_s, mesh_t,exclude_cs='',usematernnew=F,sig0=0.5,rho0=0.3) {

  # construct an SPDE model with a Matern kernel
  message('Building SPDE...')
  if(usematernnew){
    #rho0 is typical range, sig0 typical sd
    spde = local.inla.spde2.matern.new(mesh = mesh_s,
                                       prior.pc.rho = c(rho0, 0.5),
                                       prior.pc.sig = c(sig0, 0.5),
                                       alpha        = 2)
  } else {
    spde <- inla.spde2.matern(mesh = mesh_s,  alpha = 2)
  }

  # Projector Matrix
  A <- inla.spde.make.A(
    mesh = mesh_s,
    loc = as.matrix(df[, c('longitude', 'latitude'),with=F]),
    group = df$period,
    group.mesh = mesh_t
  )
  space = inla.spde.make.index("space",
                               n.spde = spde$n.spde,
                               n.group = mesh_t$m)


  #find cov indices
  if(fixed_effects!="NONE"){
    f_lin <- reformulate(fixed_effects)
    message('Indexing covariates...')
    covs_indices <- unique(c(match(all.vars(f_lin), colnames(df))))

    # make design matrix, center the scaling
    design_matrix <- data.frame(int = 1,
                                df[,covs_indices,with=F])


    cs_df <- getCentreScale(design_matrix, exclude = c('int',exclude_cs))


    design_matrix <- centreScale(design_matrix,
                                 df = cs_df)
  } else{
    design_matrix <- data.frame(int = rep(1,nrow(df)))
    cs_df <- getCentreScale(design_matrix, exclude = c('int','rates'))
  }

  # construct a 'stack' object for observed data
  cov=df[[indicator]] # N+_i
  N=df$N                 # N_i

  message('Stacking data...')
  stack.obs <- inla.stack(
    data = list(covered = cov),
    A = list(A, 1),
    effects = list(space,
                   design_matrix),
    tag = 'est'
  )

  return_list <- list(stack.obs, spde, cs_df)

  return(return_list)

}

fit_mbg <- function(indicator_family, stack.obs, spde, cov, N, int_prior_mn, int_prior_prec=1, f_mbg, run_date, keep_inla_files, cores,verbose_output=FALSE,wgts=0,intstrat='eb', fe_mean_prior = 0, fe_sd_prior = 1) {

  if(wgts[1]==0) wgts=rep(1,length(N))
  # Check if user has allocated less cores in their qsub than they specified in their config file
  cores_available <- Sys.getenv("NSLOTS")
  #if(cores_available!="" & cores_available < cores) stop(paste0("You've specified ", cores, " for INLA to use but you've only requested ", cores_available, " in your qsub"))
  #if(cores_available=="") message(paste0("It looks like you're running interactively, be sure your environment has the number of cores available that you've told INLA to use (", cores, ")"))

  ## Add fixed effect on GAUL_CODE if specified


  # ~~~~~~~~~~~~~~~~
  # fit the model
  # enable weights
  inla.setOption("enable.inla.argument.weights", TRUE)

  message('Fitting INLA model')

  # set a prior variance of 1.96 on the intercept as this is
  # roughly as flat as possible on logit scale without >1 inflection

  # code just to fit the model (not predict)
  inla_working_dir <- paste0('/snfs1/temp/geospatial/inla_intermediate/inla_', run_date)
  dir.create(inla_working_dir)
  if(indicator_family=='binomial') {
    system.time(
      res_fit <- inla(f_mbg,
                      data = inla.stack.data(stack.obs),
                      control.predictor = list(A = inla.stack.A(stack.obs),
                                               link = 1,
                                               compute = FALSE),
                      control.fixed = list(expand.factor.strategy = 'inla',
                                           prec.intercept = int_prior_prec,
                                           mean.intercept = int_prior_mn,
                                           mean = fe_mean_prior,
                                           prec = fe_sd_prior),
                      control.compute = list(dic = TRUE,
                                             cpo = TRUE,
                                             config = TRUE),
                      control.inla = list(int.strategy = intstrat, h = 1e-3, tolerance = 1e-6),
                      family = 'binomial',
                      num.threads = cores,
                      Ntrials = N,
                      verbose = verbose_output,
                      working.directory = inla_working_dir,
                      weights = wgts,
                      keep = TRUE)
    )
  }
  if(indicator_family=='gaussian') {
    system.time(
      res_fit <- inla(f_mbg,
                      data = inla.stack.data(stack.obs),
                      control.predictor = list(A = inla.stack.A(stack.obs),
                                               compute = FALSE),
                      control.fixed = list(expand.factor.strategy = 'inla',
                                           prec.intercept = 1,
                                           mean.intercept = int_prior_mn,
                                           mean = 0,
                                           prec = 2),
                      control.compute = list(dic = TRUE,
                                             cpo = TRUE,
                                             config = TRUE),
                      control.inla = list(int.strategy = intstrat, h = 1e-3, tolerance = 1e-6),
                      family = 'Gaussian',
                      num.threads = cores,
                      verbose = TRUE,
                      working.directory = inla_working_dir,
                      weights = wgts,
                      scale = N,
                      keep = TRUE)
    )
  }

  return(res_fit)

}

predict_mbg <- function(res_fit, cs_df, mesh_s, mesh_t, cov_list, samples, simple_raster, transform) {

  ## Now that we've extracted covariate values to our data in the buffer zone, clip cov list to simple_raster instead of simple_polygon
  ##    (simple_raster is area we actually want to model over)
  for(l in 1:length(cov_list)) {
    cov_list[[l]]  <- crop(cov_list[[l]], extent(simple_raster))
    cov_list[[l]]  <- setExtent(cov_list[[l]], simple_raster)
    cov_list[[l]]  <- mask(cov_list[[l]], simple_raster)
  }

  #updated by DCCASEY on 11-21-2016


  message('Making predictions')

  # number of samples
  n_draws <- samples

  # dummy raster
  template <- simple_raster

  cell_idx <- seegSDM:::notMissingIdx(template)

  # sample from posterior over latents
  suppressWarnings(draws <- inla.posterior.sample(n_draws, res_fit))

  # get parameter names
  par_names <- rownames(draws[[1]]$latent)

  # index to spatial field and linear coefficient samples
  s_idx <- grep('^space.*', par_names) ## sapce-time random effects
  l_idx <- match(sprintf('%s.1', res_fit$names.fixed), ## main effects
                 par_names)
  if(mean(is.na(l_idx)) == 1) l_idx <- match(sprintf('%s', res_fit$names.fixed), par_names) ## fix for different INLA version naming conventions

  # get samples as matrices
  pred_s <- sapply(draws, function (x)
    x$latent[s_idx])
  pred_l <- sapply(draws, function (x)
    x$latent[l_idx])
  if(length(l_idx)==1) pred_l=t(as.matrix(pred_l))
  rownames(pred_l) <- res_fit$names.fixed
  ## if we fit with a nugget, we also need to take draws of the nugget precision
  if(length(grep('^IID.ID.*', par_names)) > 0){
    pred_n <- sapply(draws, function(x)
      x$hyperpar[[4]]) ## this gets the precision for the nugget
  }else{
    pred_n <- NULL
  }

  # get coordinates of cells to predict to
  coords <- xyFromCell(template, seegSDM:::notMissingIdx(template))

  # ~~~~~~~~~~~~~~
  # project spatial effect

  # make predictions for all periods

  # get predictor matrix between spatial nodes and prediction locations
  #nperiod <- mesh_t$n
  nperiod <- max(mesh_t$loc)

  # replicate coordinates and years
  coords_periods <- do.call(rbind,
                            replicate(nperiod,
                                      coords,
                                      simplify = FALSE))

  groups_periods <- rep(1:nperiod,
                        each = nrow(coords))

  # Projector matrix
  A.pred <- inla.spde.make.A(
    mesh = mesh_s,
    loc = coords_periods,
    group = groups_periods,
    group.mesh = mesh_t
  )

  # get samples of s for all cells
  cell_s <- A.pred %*% pred_s
  cell_s <- as.matrix(cell_s)

  # remove out temporally varying covariates for now, will deal with them later
  #split the effects names by varying or time varying
  tvnames = pars =c()
  for(i in 1:length(cov_list)){
    if(dim(cov_list[[i]])[3]!=1) tvnames[length(tvnames)+1]= names(cov_list)[i]
    if(dim(cov_list[[i]])[3]==1) pars[length(pars)+1]= names(cov_list)[i]
  }

  #now split the covariates themselves (not just the names)
  # process covariates

  #start by ensuring the same mask and cell size
  cov_list = setNames(lapply(cov_list, function(x) mask(resample(x,template),template)),names(cov_list))

  #extract the covariates
  vals = data.table(do.call(cbind, lapply(cov_list, function(x) raster::extract(x,coords))))

  #create the int column
  vals[,int:= 1]

  #reshape long
  vals[,id:= 1:nrow(vals)] #create an id to ensure that melt doesn't sort things

  #convert the time varying names into meltable values
  tv_cov_colnames = grep(paste(tvnames, collapse = "|"), names(vals), value = T) #unlisted
  tv_cov_colist = lapply(tvnames, function(x) grep(x, names(vals), value = T))

  vals = melt(vals, id.vars = c('id','int',pars), measure = tv_cov_colist, value.name = tvnames, variable.factor =F)

  #fix the names
  #melt returns different values of variable based on if its reshaping 1 or 2+ columns.
  #enforce that it must end with the numeric after the period
  vals[,variable:= as.numeric(substring(variable,regexpr("(\\.[0-9]+)$", variable)[1]+1))]

  #Tests suggest this is not needed anymore/as a carry over. Keep just in case
  #if(length(tv_cov_colist)==1){
  #  setnames(vals,paste0(tvnames[[1]],'1'), tvnames)
  #}

  #keep only columns that were part of the regression
  vals = vals[,mget(rownames(pred_l))]

  #centreScale the values
  vals = centreScale(vals,cs_df) #happens for all periods at once

  #create the grid of fixed effects
  cell_l =as.matrix(vals) %*% pred_l

  ## add on nugget effect if applicable
  if(!is.null(pred_n)){
    cell_n <- sapply(pred_n, function(x){
      rnorm(n = nrow(cell_l), sd = 1 / sqrt(x), mean = 0)
    })
    cell_l <- cell_l + cell_n ## add on the nugget/noise to fixed effects field
  }

  # project model uncertainty from node-level sd
  cell_l_sd <-  apply(cell_l, 1, sd)

  node_s_sd <- apply(pred_s, 1, sd)
  cell_s_sd <- as.matrix(A.pred %*% node_s_sd)
  # combine to get cell-level sd

  # project to cells - ASSUMES INDEPENDENCE!
  cell_sd <- sqrt(cell_l_sd ^ 2 + cell_s_sd ^ 2)

  # ~~~~~~~~~~~~~~
  # combine and summarise them
  cell_all <- cell_l + cell_s

  # get predictive draws on probability scale
  if(transform=='inverse-logit') { cell_pred <- plogis(as.matrix(cell_all))
  } else cell_pred <- eval(parse(text=transform))

  # get prediction mean (integrated probability)
  pred_mean <- rowMeans(cell_pred)

  # create multi-band rasters for each of these metrics
  # each band giving the values for a different period
  mean_ras <- insertRaster(template,
                           matrix(pred_mean,
                                  ncol = nperiod))

  sd_ras <- insertRaster(template,
                         matrix(cell_sd,
                                ncol = nperiod))

  names(mean_ras) <-
    names(sd_ras) <-
    paste0('period_', 1:nperiod)

  return_list <- list(mean_ras, sd_ras, cell_pred)

  return(return_list)

}

save_mbg_preds <- function(config, time_stamp, run_date, mean_ras, sd_ras, res_fit, cell_pred, df ,pathaddin="") {

  if(time_stamp==TRUE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/', run_date)
  if(time_stamp==FALSE) output_dir <- paste0('/share/geospatial/mbg/', indicator_group, '/', indicator, '/output/scratch')
  dir.create(output_dir, showWarnings = FALSE)

  # Save log of config file
  write.csv(config, paste0(output_dir,'/config.csv'), row.names = FALSE)

  if (!is.null(mean_ras)) {
    writeRaster(
      mean_ras,
      file = (paste0(output_dir, '/', indicator,'_prediction_eb',pathaddin)),
      overwrite = TRUE
    )
  }

  if (!is.null(sd_ras)) {
    # latent sd
    writeRaster(
      sd_ras,
      file = (paste0(output_dir, '/', indicator,'_sd_eb',pathaddin)),
      overwrite = TRUE
    )
  }

  # save model
  save(res_fit,
       file = (paste0(output_dir, '/', indicator,'_model_eb',pathaddin,'.RData')))
  # save draws (with compression) to recombine later
  save(
    cell_pred,
    file = (paste0(output_dir, '/', indicator,'_cell_draws_eb',pathaddin,'.RData')),
    compress = TRUE
  )
  # save training data
  write.csv(
    df,
    file = (paste0(output_dir, '/', indicator,'_trainingdata',pathaddin)),
    row.names = FALSE
  )

  # Write a an empty file to indicate done with this parallel script
  write(NULL, file = paste0(output_dir, "/fin_", pathaddin))

}
