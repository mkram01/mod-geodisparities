## INLA Model Function

## Setup and load packages ---------------------------------
library(knitr)
library(tidyverse) # data management
library(sf) # spatial manipulation
library(sp) # spatial manipulation (adjacency)
library(spdep) # adjacency
library(tmap)  # mapping
library(INLA)  # Mapping
library(magrittr) # Chaining
library(Rgraphviz) # plotting INLA adjacency graph objects

## Load prepped data
source("Prep Data.R")

# Create INLA model function
inlamodel <- function(formula, family, dataset, title) {
  
  # Read in function
  func <- formula
  
  # Run
  model <- inla(func, family = family,
             data = dataset,
             offset = log(births),
             control.predictor = list(link = 1,
                                      compute = T),
             control.compute = list(dic = TRUE,
                                    cpo = TRUE,
                                    waic = TRUE,
                                    config = T))
  
  # Produce model comparison table
  if (is.null(comptable)) {
    comptable <- as.data.frame(rbind(title, 
                                     model$dic$dic, 
                                     model$waic$waic, 
                                     sum(log(model$cpo$cpo))))
    colnames(comptable) <- c("Model", "DIC", "WAIC", "Sum(log(CPO))")
  } else {
    newmodel <- rbind(title,
                      model$dic$dic,
                      model$waic$waic,
                      sum(log(model$cpo$cpo)))
    comptable <- as.data.frame(rbind(comptable, newmodel))
  }
}

inlamodel(ptb ~ year_c + black + f(ID, model = 'iid'), 
          "poisson",
          newengland_sf,
          "")
## M1: Poisson Random intercept, iid -------------------------
# Random intercept for county with independent (aspatial) prior.
f1 <- ptb ~ year_c + black + f(ID, model = 'iid')
m1 <- inla(f1, family = 'poisson',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1,
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f2 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T)

m2 <- inla(f2, family = 'poisson',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f3 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T) +
  f(ID2, black, model = 'iid')

m3 <- inla(f3, family = 'poisson',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f4 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T) +
  f(ID2, black, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T)

m4 <- inla(f4, family = 'poisson',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f5 <- ptb ~ year_c + black + f(ID, model = 'iid')

m5 <- inla(f5, family = 'nbinomial',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1,
                                    compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f6 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T)

m6 <- inla(f6, family = 'nbinomial',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f7 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T) +
  f(ID2, black, model = 'iid')

m7 <- inla(f7, family = 'nbinomial',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))

f8 <- ptb ~ year_c + black +
  f(ID, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T) +
  f(ID2, black, model = 'bym',
    graph = '../../data/spatial/southatlantic_knn6.adj',
    hyper = list(prec.unstruct = list(prior = 'loggamma', param = c(1,0.001)),
                 prec.spatial = list(prior = 'loggamma', param = c(1,0.001))),
    scale.model = T)

m8 <- inla(f8, family = 'nbinomial',
           data = southatlantic_sf,
           offset = log(births),
           control.predictor = list(link = 1, compute = T),
           control.compute = list(dic = TRUE,
                                  cpo = TRUE,
                                  waic = TRUE,
                                  config = T))
