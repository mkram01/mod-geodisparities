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
    raw_rate = (ptb/births),
    model_rate = (mean/births),
    model_lci = (`0.025quant`/births),
    model_uci = (`0.975quant`/births),
    cred_int = (model_uci - model_lci) #if this is larger than model_rate, then estimate unreliable
  )

#width of credible interval greater than rate itself?
#numeric suppression by denominator


#save outputs
#save model as rds file -- make this togglable in config

#save output table as csv


# This is the posterior of the (aspatial) random effect for county on the scale of risk ratio/SMR:






# Extracting marginal posterior estimates
# This is the posterior of the (aspatial) random effect for county on the scale of risk ratio/SMR:
spatdata_sp$m1_iid_re <- unlist(lapply(m1$marginals.random$ID,
                                            function(x) inla.emarginal(exp, x)))

message("From process_model.R script: Joining the posterior of the fitted values to spatial data.")

#model object of fitted values
head(m1$summary.fitted.values)
#use mean to fill fitted value for visualization for counts
#     - to get rates, join to smry_data and divide by total births


#saving outputs for comparison
model_out <- as.data.table(m1$summary.fitted.values)
model_in <- as.data.table(smry_data)

write.csv(model_out, file = paste0(data_repo, "/model_output/model_compare/new_model_output.csv"))
write.csv(model_in, file = paste0(data_repo, "/model_output/model_compare/new_model_input.csv"))
