##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Processing INLA model outputs for visualization
# Date: 7.12.2019
#############################################

message("From process_model.R script: Extracting marginal posterior estimates")

#formatting summary model output
m1_summ <- as.data.frame(m1$summary.fitted.values)

message("From process_model.R script: Joining the posterior of the fitted values to spatial data.")
#join to input data
alldata <- smry_data %>%
  bind_cols(m1_summ)

#create rate fields for poisson family models
alldata <- alldata %>%
  dplyr::mutate(
    raw_rate = ((outcome)/(denominator)), #outcome/denominator
    model_rate = (mean/(denominator)),
    rate_diff = (raw_rate - model_rate), #Deviation from truth
    model_lci = (`0.025quant`/births),
    model_uci = (`0.975quant`/births),
    cred_int = (model_uci - model_lci), #if this is larger than model_rate, then estimate unreliable
    unreliabele = if(cred_int > model_rate){1}else{0} #if credible interval greater than model rate, flag with a 1
  )

# #extract marginal posterior estimates and join to sf object
# spatdata_sf$m1_iid_re <- unlist(lapply(m1$marginals.random$ID,
#                                        function(x) inla.emarginal(exp, x)))

# This is the posterior of the fitted values. Note that there are
# spatdata_sf$m1_fit 

#save outputs
write.csv(alldata, paste0(outdir, modname, "_fittedvals.csv"), row.names = FALSE)

#save model as rds file -- make this togglable in config
if(save_mod == TRUE){
  saveRDS(m1, file = paste0(outdir, modname, "_model.rds"))
}

# Old code -- delete when sure not going to use
# # Extracting marginal posterior estimates
# # This is the posterior of the (aspatial) random effect for county on the scale of risk ratio/SMR:
# spatdata_sp$m1_iid_re <- unlist(lapply(m1$marginals.random$ID,
#                                             function(x) inla.emarginal(exp, x)))
# 
# message("From process_model.R script: Joining the posterior of the fitted values to spatial data.")
# 
# #model object of fitted values
# head(m1$summary.fitted.values)
# #use mean to fill fitted value for visualization for counts
# #     - to get rates, join to smry_data and divide by total births

