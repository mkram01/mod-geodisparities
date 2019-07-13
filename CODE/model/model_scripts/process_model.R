##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Processing INLA model outputs for visualization
# Date: 7.12.2019
#############################################

message("From process_model.R script: Extracting marginal posterior estimates")

# Extracting marginal posterior estimates
# This is the posterior of the (aspatial) random effect for county on the scale of risk ratio/SMR:
spatdata_sp$m1_iid_re <- unlist(lapply(m1$marginals.random$ID,
                                            function(x) inla.emarginal(exp, x)))

message("From process_model.R script: Joining the posterior of the fitted values to spatial data.")
# This is the posterior of the fitted values.
spatdata_sf$m1_fit <- unlist(lapply(m1$marginals.fitted.values,
                                         function(x) inla.emarginal(mean, x))) /spatdata_sf$births
