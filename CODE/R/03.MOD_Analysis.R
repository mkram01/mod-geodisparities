# March of Dimes Analysis #
# 03: Analysis Script     #
# Kevin Weiss             #
# 02/04/2019              #

## Package and data setup -----------------
rm(list = ls())

library(sp) # for spatial data manipulation
library(spdep) # for adjacency matrix creation
library(tigris) # for TIGRIS county shapefile
library(colorspace) # for HCL color palette
library(grid) # for spplot customization
library(lattice)
library(tidycensus)
library(ggplot2)
library(dplyr)
library(plyr)
library(sf)
library(mapview)
library(car)
library(maptools)
library(faraway)
library(INLA)
library(knitr)
library(tidyr) # for data manipulation
library(magrittr) # for chaining actions together

## Model Analysis --------------------

# Read in data files
model1 <- readRDS(file = "DATA/nchs_births/R/data/model1.rda")
# model2 <- readRDS(file = "DATA/nchs_births/R/data/model2.rda")
# model3 <- readRDS(file = "DATA/nchs_births/R/data/model3.rda")
# model4 <- readRDS(file = "DATA/nchs_births/R/data/model4.rda")
# model5 <- readRDS(file = "DATA/nchs_births/R/data/model5.rda")
# model6 <- readRDS(file = "DATA/nchs_births/R/data/model6.rda")

# Data Description
# Model 1: YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
# Model 2: YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
# Model 3: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS
# Model 4: YEAR x COUNTY x RACE restricted to NH-Black/NH-White (including multiple births)
# Model 5: YEAR x COUNTY x RACE x AGE (including multiple births)
# Model 6: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION (including multiple births)

# Model 0: Null model ----------------------------------------
null <- ptb ~ 1 # Fixed effects
fit0 <- inla(null, 
             family = "poisson", 
             data = model1, 
             verbose = TRUE,  
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
                                 #int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)
#27.436 seconds
# Model summary
summary(fit0)

# Look at DIC and WAIC
fit0$dic$dic # 415285.8
fit0$waic$waic #415634.3

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit0$marginals.fixed$`(Intercept)`)

# See fitted model values
hist(fit0$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit0$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit0$summary.fitted.values$mean))

# Spatial plot (need to join to spatial polygons data frame)
# model1$fittedptb <- fit0$summary.fitted.values$mean
# brks <- quantile(c(model1$ptb, model1$fittedptb), 
#                  p = c(0, .25, .5, .75, 1), na.rm = TRUE)
# brks
# 0%          25%          50%          75%         100% 
# 0.000000e+00 1.652566e-02 7.036080e-01 8.000000e+00 2.602000e+03 
# spplot(model1, c("ptb", "fittedptb"), 
#        col.regions=brewer.pal(n=5, "RdBu"), 
#        at = brks , 
#        main = "Raw and Fitted PTB Counts")

# Model 1: Add random observation-level effect ----------------------------------------
random <- ptb ~ 1 + # Fixed effects
  f(id, model = "iid") # Random effects
fit1 <- inla(random, 
             family = "poisson", 
             data = model1, 
             verbose = TRUE,  
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
                                # int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)
#135 seconds

# Model summary
summary(fit1)

# Look at DIC and WAIC
fit1$dic$dic #287180.8
fit1$waic$waic #284747.3

# Look at hyperparameters
1/fit1$summary.hyperpar$mean #0.06420099

# Look at precision of hyperparameters
m1 <- inla.tmarginal(
  function(x) 1/x,
  fit1$marginals.hyperpar$'Precision for id')

# Plot precision for hyperparameter for between-county variance
plot(m1, 
     type = "l", 
     main = c("Posterior distibution for between county variance"))

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit1$marginals.fixed$`(Intercept)`)
# inla.hpdmarginal(p = .95, fit1$marginals.random$id)

# See fitted model values
hist(fit1$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit1$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit1$summary.fitted.values$mean))

# Spatial plot (need to join to spatial polygons data frame)
# model1$fittedptb <- fit0$summary.fitted.values$mean
# brks <- quantile(c(model1$ptb, model1$fittedptb), 
#                  p = c(0, .25, .5, .75, 1), na.rm = TRUE)
# brks
# 0%          25%          50%          75%         100% 
# 0.000000e+00 1.652566e-02 7.036080e-01 8.000000e+00 2.602000e+03 
# spplot(model1, c("ptb", "fittedptb"), 
#        col.regions=brewer.pal(n=5, "RdBu"), 
#        at = brks , 
#        main = "Raw and Fitted PTB Counts")

# Model 2: BYM Model for space -----------
#  R-INLA Conditional Autoregressive (CAR) Besag–York–Mollié (BYM) Model
## See Chapter 6 (especially 6.1.2 for BYM) in the "Spatial and Spatio-Temporal Bayesian Models in R-INLA" by Blangiardo and Cameletti (2015)
# Section 6.3 is about ZIP models
# Specify spatial connectivity matrix
h <- inla.read.graph(filename = "DATA/nchs_births/R/Data/graphs/model1.adj")
bymspace <- ptb ~ 1 + # Fixed effects # Is the 1 needed?
  f(id, model = "bym", constr = TRUE, scale.model = TRUE, graph = h) # Spatial matrix
fit2 <- inla(bymspace, 
             family = "poisson", 
             data = model1, 
             verbose = TRUE,
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
             #int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)

# Model summary
summary(fit2)

# Look at DIC and WAIC
fit2$dic$dic #
fit2$waic$waic #

# Look at hyperparameters
1/fit2$summary.hyperpar$mean #

# Look at precision of hyperparameters
fit2_sp <- inla.tmarginal(
  function(x) (1/x),
  fit2$marginals.hyperpar$`Precision for id (spatial component)`)
fit2_iid <- inla.tmarginal(
  function(x) (1/x),
  fit2$marginals.hyperpar$`Precision for id (iid component)`)

# Plot precision for hyperparameters
plot(fit2_sp, 
     type = "l", 
     main = c("Posterior distibution for between Spatial MSA variance", 
              "- Multi-level model -"), 
     xlim = c(0, .015))
lines(fit2_iid, col = 2,lty = 2)
legend("topright", 
       legend = c("Spatial Variance", "IID Variance"), 
       col = c(1, 2), 
       lty = c(1, 2))

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit2$marginals.fixed$`(Intercept)`)
inla.hpdmarginal(.95, marginal = fit2_sp)
inla.hpdmarginal(.95, marginal = fit2_iid)

# See fitted model values
hist(fit2$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit2$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit2$summary.fitted.values$mean))

# Model 3: BYM Model for space and time -----------
#  R-INLA Conditional Autoregressive (CAR) Besag–York–Mollié (BYM) Model
## See Chapter 6 (especially 6.1.2 for BYM) in the "Spatial and Spatio-Temporal Bayesian Models in R-INLA" by Blangiardo and Cameletti (2015)
# Section 6.3 is about ZIP models
# Specify spatial connectivity matrix
h <- inla.read.graph(filename = "DATA/nchs_births/R/Data/graphs/model1.adj")
spacetime < -ptb ~ 1 + # Fixed effects
  f(id, model = "bym", constr = TRUE, scale.model = TRUE, graph = h) + # Spatial matrix
  f(dob_yy, model="rw1") + # Year random effect (type 1 random walk)
fit3 <- inla(formula = spacetime,
             family = "poisson", 
             data = model1, 
             verbose = TRUE,
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
             #int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)

# Look at DIC and WAIC
fit3$dic$dic #
fit3$waic$waic #

# Look at hyperparameters
1/fit3$summary.hyperpar$mean #

# Look at precision of hyperparameters
fit3_iid <- inla.tmarginal(
  function(x) (1/x),
  fit3$marginals.hyperpar$`Precision for id (iid component)`)
fit3_sp <- inla.tmarginal(
  function(x) (1/x),
  fit3$marginals.hyperpar$`Precision for id (spatial component)`)
fit3_year <- inla.tmarginal(
  function(x) (1/x),
  fit3$marginals.hyperpar$`Precision for dob_yy`)

# Plot precision for hyperparameters
plot(fit3_sp, 
     type = "l", 
     main = c("Posterior distibution for between Spatial MSA variance", 
              "- Multi-level model -"), 
     xlim = c(0, .015))
lines(fit3_iid, col = 2,lty = 2)
legend("topright", 
       legend = c("Spatial Variance", "IID Variance"), 
       col = c(1, 2), 
       lty = c(1, 2))

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit3$marginals.fixed$`(Intercept)`)
inla.hpdmarginal(.95, marginal = fit3_sp)
inla.hpdmarginal(.95, marginal = fit3_iid)
inla.hpdmarginal(.95, marginal = fit3_year)

# See fitted model values
hist(fit3$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit3$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit3$summary.fitted.values$mean))


# Model 4: Fixed effect covariates ----------------------------------------
covs ~ dob_yy + combfips + racehisp_recode # Fixed effects
fit4 <- inla(covs, 
             family = "poisson", 
             data = model1, 
             verbose = TRUE,  
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
             # int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)

# Model summary
summary(fit4)

# Look at DIC and WAIC
fit4$dic$dic #
fit4$waic$waic #

# Look at hyperparameters
1/fit4$summary.hyperpar$mean #

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit4$marginals.fixed$`(Intercept)`)
inla.hpdmarginal(p = .95, fit4$marginals.fixed$`(racehisp_recode)`)
inla.hpdmarginal(p = .95, fit4$marginals.fixed$`(dob_yy)`)
inla.hpdmarginal(p = .95, fit4$marginals.fixed$`(combfips)`)

# See fitted model values
hist(fit4$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit4$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit4$summary.fitted.values$mean))

# Spatial plot (need to join to spatial polygons data frame)
# model1$fittedptb <- fit0$summary.fitted.values$mean
# brks <- quantile(c(model1$ptb, model1$fittedptb), 
#                  p = c(0, .25, .5, .75, 1), na.rm = TRUE)
# brks
# 0%          25%          50%          75%         100% 
# 0.000000e+00 1.652566e-02 7.036080e-01 8.000000e+00 2.602000e+03 
# spplot(model1, c("ptb", "fittedptb"), 
#        col.regions=brewer.pal(n=5, "RdBu"), 
#        at = brks , 
#        main = "Raw and Fitted PTB Counts")

# Model 5: Covariates + random effects ----------------------------------------
covs2 ~ dob_yy + combfips + racehisp_recode + # Fixed effects
  f(id, model = "iid") # Random effects
fit5 <- inla(covs2, 
             family = "poisson", 
             data = model1, 
             verbose = TRUE,  
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
             # int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)
#135 seconds

# Model summary
summary(fit5)

# Look at DIC and WAIC
fit5$dic$dic #
fit5$waic$waic #

# Look at hyperparameters
1/fit5$summary.hyperpar$mean #

# Look at precision of hyperparameters
m5 <- inla.tmarginal(
  function(x) 1/x,
  fit1$marginals.hyperpar$'Precision for id')

# Plot precision for hyperparameter for between-county variance
plot(m5, 
     type = "l", 
     main = c("Posterior distibution for between county variance"))

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit5$marginals.fixed$`(Intercept)`)
inla.hpdmarginal(p = .95, fit5$marginals.fixed$`(racehisp_recode)`)
inla.hpdmarginal(p = .95, fit5$marginals.fixed$`(dob_yy)`)
inla.hpdmarginal(p = .95, fit5$marginals.fixed$`(combfips)`)

# See fitted model values
hist(fit1$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit5$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit5$summary.fitted.values$mean))

# Spatial plot (need to join to spatial polygons data frame)
# model1$fittedptb <- fit0$summary.fitted.values$mean
# brks <- quantile(c(model1$ptb, model1$fittedptb), 
#                  p = c(0, .25, .5, .75, 1), na.rm = TRUE)
# brks
# 0%          25%          50%          75%         100% 
# 0.000000e+00 1.652566e-02 7.036080e-01 8.000000e+00 2.602000e+03 
# spplot(model1, c("ptb", "fittedptb"), 
#        col.regions=brewer.pal(n=5, "RdBu"), 
#        at = brks , 
#        main = "Raw and Fitted PTB Counts")

# Model 6: BYM Model for space and time and space-time interaction (residual) -----------
#  R-INLA Conditional Autoregressive (CAR) Besag–York–Mollié (BYM) Model
## See Chapter 6 (especially 6.1.2 for BYM) in the "Spatial and Spatio-Temporal Bayesian Models in R-INLA" by Blangiardo and Cameletti (2015)
# Section 6.3 is about ZIP models
# Specify spatial connectivity matrix
h <- inla.read.graph(filename = "DATA/nchs_births/R/Data/graphs/model1.adj")
spacetimeresid < -ptb ~ 1 + # Fixed effects
  f(id, model = "bym", constr = TRUE, scale.model = TRUE, graph = h) + # Spatial matrix
  f(dob_yy, model="rw1") + # Year random effect (type 1 random walk)
  f(resid, model = "iid") # • Space-time interaction term (residual, iid)
fit6 <- inla(formula = spacetimeresid,
             family = "poisson", 
             data = model1, 
             verbose = TRUE,
             E = meanptb, # Needed?
             offset = log(births + 1), # Correct offset? Need + 1 to avoid error
             control.inla = list(strategy = 'gaussian'),#,
             #int.strategy = 'eb'), # Empirical Bayes
             control.compute = list(dic = TRUE,
                                    waic = TRUE),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)

# Look at DIC and WAIC
fit6$dic$dic #
fit6$waic$waic #

# Look at hyperparameters
1/fit6$summary.hyperpar$mean #

# Look at precision of hyperparameters
fit6_iid <- inla.tmarginal(
  function(x) (1/x),
  fit6$marginals.hyperpar$`Precision for id (iid component)`)
fit6_sp <- inla.tmarginal(
  function(x) (1/x),
  fit6$marginals.hyperpar$`Precision for id (spatial component)`)
fit6_year <- inla.tmarginal(
  function(x) (1/x),
  fit6$marginals.hyperpar$`Precision for dob_yy`)

# Plot precision for hyperparameters
plot(fit6_sp, 
     type = "l", 
     main = c("Posterior distibution for between Spatial MSA variance", 
              "- Multi-level model -"), 
     xlim = c(0, .015))
lines(fit6_iid, col = 2,lty = 2)
legend("topright", 
       legend = c("Spatial Variance", "IID Variance"), 
       col = c(1, 2), 
       lty = c(1, 2))

# Get a 95% credible interval for a regresion parameter:
inla.hpdmarginal(p = .95, fit6$marginals.fixed$`(Intercept)`)
inla.hpdmarginal(.95, marginal = fit6_sp)
inla.hpdmarginal(.95, marginal = fit6_iid)
inla.hpdmarginal(.95, marginal = fit6_year)

# See fitted model values
hist(fit6$summary.fitted.values$mean)

# Plot observed vs. fitted values
plot(x = fit6$summary.fitted.values$mean, 
     y = model1$ptb , 
     ylab = "Observed", 
     xlab = "Estimated/Fitted")
abline(lm(model1$ptb ~ fit6$summary.fitted.values$mean))

# Space-time mapping of the fitted values (NEEDS FURTHER EDITING WITH ACTUAL FILES)-------------
final.dat$fitted_m3 <- mod3$summary.fitted.values$mean
final.dat %>%
  filter(year %in% c(2000))%>%
  mutate(qrr = cut(fitted_m3, breaks = quantile(fitted_m3, p = seq(0,1,length.out = 8)))) %>%
  ggplot() + 
  geom_sf(aes(fill = qrr)) + 
  scale_colour_brewer(palette = "RdBu" ) + 
  scale_fill_brewer(palette = "RdBu", na.value = "grey") + 
  guides(fill = guide_legend(title = "Relative Risk Quartile")) + 
  ggtitle(label="Relative Risk Quartile - IID Model, 2000") + 
  coord_sf(crs = 102008)

final.dat %>%
  filter(year %in% c(2007))%>%
  mutate(qrr = cut(fitted_m3, breaks = quantile(fitted_m3, p = seq(0,1,length.out = 8)))) %>%
  ggplot() + 
  geom_sf(aes(fill = qrr)) + 
  scale_colour_brewer(palette = "RdBu" ) + 
  scale_fill_brewer(palette = "RdBu", na.value = "grey") + 
  guides(fill = guide_legend(title = "Relative Risk Quartile")) + 
  ggtitle(label="Relative Risk Quartile - IID Model, 2000") + 
  coord_sf(crs = 102008)

library(mapview)
map1<-final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(fitted_m3, breaks = quantile(fitted_m3, p=seq(0,1,length.out = 8))))
clrs <- colorRampPalette(brewer.pal(8, "RdBu"))
mapView(as(map1, "Spatial"), zcol="qrr", legend=T, col.regions=clrs, map.types="OpenStreetMap")

# Map BYM model to look for spatial trends
us_co$sp_re<-mod3$summary.random$struct$mean[1:3108]
us_co%>%
  mutate(qse = cut(sp_re, breaks = quantile(sp_re, p = seq(0,1,length.out = 6)), 
                   include.lowest = T))%>%
  ggplot() +
  geom_sf(aes(fill = qse)) + 
  scale_colour_brewer(palette = "RdBu") + 
  scale_fill_brewer(palette = "RdBu", na.value="grey") + 
  guides(fill = guide_legend(title = "Spatial Excess Risk")) + 
  ggtitle(label = "Spatial Random Effect - BYM Model") + 
  coord_sf(crs = 102008)


# Exceedence probabilities - spatial clustering in relative risk
# Use posterior marginals
thetastar <- 1.5 #theta*
inlaprob <- unlist(lapply(mod3$marginals.fitted.values, function(X){
  1 - inla.pmarginal(thetastar, X)
}))
hist(inlaprob)
# If the density, or Pr(θ>θ∗) is high, then there is evidence that the excess risk is not only high, but significantly high.

# Visualize the high exceedence probabilities
final.dat$exceedprob <- inlaprob

# 2000
final.dat %>%
  filter(year %in% c(2000)) %>%
  mutate(qrr = cut(exceedprob, 
                   breaks = c(0, .5, .9, .95, .99, 1), 
                   include.lowest = T)) %>%
  ggplot() +
  geom_sf(aes(fill = qrr)) +
  scale_colour_brewer(palette = "Blues" ) + 
  scale_fill_brewer(palette = "Blues", na.value = "grey") +
  guides(fill = guide_legend(title = "")) +
  ggtitle(label = expression(paste("Exceedence Probability Relative Risk ",
                                   "Pr( ",theta," >1.5"," )  - 2000") )) + 
  coord_sf(crs = 102008)

# 2007
final.dat %>%
  filter(year %in% c(2000)) %>%
  mutate(qrr = cut(exceedprob, 
                   breaks = c(0, .5, .9, .95, .99, 1), 
                   include.lowest = T)) %>%
  ggplot() +
  geom_sf(aes(fill = qrr)) +
  scale_colour_brewer(palette = "Blues" ) + 
  scale_fill_brewer(palette = "Blues", na.value = "grey") +
  guides(fill = guide_legend(title = "")) +
  ggtitle(label = expression(paste("Exceedence Probability Relative Risk ",
                                   "Pr( ",theta," >1.5"," )  - 2000") )) + 
  coord_sf(crs = 102008)

library(mapview)
map1 <- final.dat %>% 
  filter(year %in% c(2007)) %>% 
  mutate(qrr = cut(exceedprob, 
                   breaks = c(0, .5, .9, .95, .99, 1), include.lowest = T))

clrs <- colorRampPalette(brewer.pal(6, "Blues"))
mapView(as(map1, "Spatial"), 
        zcol = "qrr", 
        legend = T, 
        col.regions = clrs, 
        map.types = "OpenStreetMap")
