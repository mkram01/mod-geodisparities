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

# Create indicator of whether this is binary black or factor HISPRACEf
race_group <- ifelse(recode_binary == 'nonbinary', 'HISPRACEf', 'black')

# Now create df indicating which county-groups have sparse data averaged over time period
suppress <- smry_data %>%
  dplyr::group_by_("GEOID", (race_group)) %>%
  dplyr::summarise(totbirths = sum(births),
                   suppress = ifelse(totbirths < 200, 1, 0)) %>%
  dplyr::select(-totbirths)


# Join suppress indicator in, and bind with inla fitted values
alldata <- smry_data %>%
  left_join(suppress, by = c("GEOID", (race_group))) %>%
  bind_cols(m1_summ)

#create rate fields for poisson family models
# Note changed use of environmental variables because error with previous
alldata <- alldata %>%
  dplyr::mutate(
    raw_rate := UQ(rlang::sym(outcome)) / UQ(rlang::sym(denominator)), #outcome/denominator - changed due to error
    model_rate := `0.5quant`/UQ(rlang::sym(denominator)),
    rate_resid = (raw_rate - model_rate), #Deviation from truth
    model_lci = (`0.025quant`/UQ(rlang::sym(denominator))),
    model_uci = (`0.975quant`/UQ(rlang::sym(denominator))),
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
  saveRDS(m1, file = paste0(outimage, modname, "_model.rds"))
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

