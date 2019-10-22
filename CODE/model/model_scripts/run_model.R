##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: INLA model call
# Date: 7.12.2019
#############################################

message("From run_model.R script: Running INLA model")

#define inla formula
f1 <- inla_formula

#inla model run
m1 <- inla(f1, family = (family),
           data = spatdata_sf,
           offset = log(births),
           control.predictor = list(link = 1,
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

message("From run_model.R script: INLA model run complete!")
