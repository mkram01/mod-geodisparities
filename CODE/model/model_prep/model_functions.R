##############################################
# Code author: Michael Kramer, Kevin Weiss, Erin Stearns
# Code objective: Model functions
# Date: 5.7.2019
#############################################


# Create INLA model function
inlamodel <- function(formula, family, dataset, title) {
  # Function to create INLA model function
  #   Input args:
  #           formula   = model formula, e.g. ptb ~ year_c + black + f(ID, model = 'iid')
  #           family    = 
  #           dataset   = model input data
  #           title     = title of model run
  #
  #   Output: character string of date and time
  #           precise output: "<year>_<month>_<day>_<hour>_<minute>_<second>"
  
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







