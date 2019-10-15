##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: INLA model call
# Date: 9.14.2019
#############################################

message("From run_model.R script: Running INLA model")

#define inla formula
#f1 <- inla_formula

#  ---- M1: Poisson Random intercept, iid ----
# Random intercept for county with independent (aspatial) prior.
#f1 <- ptb ~ year_c + black + f(ID, model = "iid")
smry_data$ID <- as.numeric(smry_data$ID)

f1 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = model_spwts, # "/CODE/model/spwts_all_knn6.adj",
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.01)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.01))),
    scale.model = T)
    #values = c("ID"))

#f1 <- ptb ~ year_c + black + f(ID, model = "bym", graph = "/CODE/model/spwts_all_knn6.adj")

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

#check out adjacency matrix
summary(model_spwts)
plot(model_spwts)

test <- inla.read.graph("/CODE/model/spwts_all_knn6.adj")
summary("/CODE/model/spwts_all_knn6.adj")
