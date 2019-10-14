##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: INLA model call
# Date: 7.12.2019
#############################################

message("From run_model.R script: Running INLA model")

#define inla formula
f1 <- inla_formula

f1 <- ptb ~ year_c + black + 
  f(ID, model = 'bym', 
    graph = model_spwts, 
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.01)), 
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.01))), 
    scale.model = T) 

#attempts at getting the denominator arg to work in inla call:
# denom <- paste0('log(',as.name(denominator),')')
# offset_formula <- as.formula(denom)


#inla model run
m1 <- inla(f1, family = (family),
           data = smry_data,
           offset = log(births),
           control.predictor = list(link = 1,
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

message("From run_model.R script: INLA model run complete!")
