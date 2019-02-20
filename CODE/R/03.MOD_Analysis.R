# March of Dimes Analysis #
# 03: Analysis Script     #
# Kevin Weiss             #
# 02/04/2019              #

## Package and data setup -----------------
rm(list = ls())

library(dplyr) # for data manipulation
library(plyr) # for data manipulation
library(tidyr) # for data manipulation
library(magrittr) # for chaining actions together
# install.packages("INLA", repos = c(getOption("repos"),
#                                    INLA = "https://inla.r-inla-download.org/R/stable"),
#                  dep = TRUE)
library(INLA)

## Model Analysis --------------------

# Read in Summary data files from SAS (done on Git)
model1 <- readRDS(file = "DATA/nchs_births/R/data/model1.rda")
model2 <- readRDS(file = "DATA/nchs_births/R/data/model2.rda")
model3 <- readRDS(file = "DATA/nchs_births/R/data/model3.rda")
model4 <- readRDS(file = "DATA/nchs_births/R/data/model4.rda")
model5 <- readRDS(file = "DATA/nchs_births/R/data/model5.rda")
model6 <- readRDS(file = "DATA/nchs_births/R/data/model6.rda")
gatestmodel1 <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1.rda")
gatestmodel1singleyear <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1singleyear.rda")
model1singleyear <- readRDS(file = "DATA/nchs_births/R/data/model1singleyear.rda")

# Data Description

# Model 1: YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
# Model 2: YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
# Model 3: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS
# Model 4: YEAR x COUNTY x RACE restricted to NH-Black/NH-White (including multiple births)
# Model 5: YEAR x COUNTY x RACE x AGE (including multiple births)
# Model 6: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION (including multiple births)

# Plotting data ------------------
# Options = binomial, poisson, negative binomial
# Distribution of IMR by yearggplot(data = final.dat) + 
  geom_histogram(aes(x = log(rate) , y = 0.5 * ..density..)) + 
  facet_wrap(~ year) +
  ggtitle(label = "Distribution of Infant Mortality Rate by Year", 
          subtitle = "US Counties, 2000-2007")

# Distribution of IMR by year
ggplot(data = final.dat) + 
  geom_histogram(aes(x = log(rate/E_d) , y = 0.5* ..density..)) + 
  facet_wrap(~ year) +
  ggtitle(label = "Distribution of Infant Mortality Relative Risk by Year", 
          subtitle = "US Counties, 2000-2007")

ggplot(data = final.dat) + 
  geom_histogram(aes(x = deaths , y = 0.5*..density..)) + 
  facet_wrap(~year) +
  ggtitle(label = "Distribution of Deaths by Year", subtitle = "Texas Counties, 2000-2007")

ggplot(data = final.dat) + 
  geom_histogram(aes(x = deaths/E_d , y = 0.5*..density..)) + 
  facet_wrap(~year) +
  ggtitle(label = "Distribution of Mortality Relative Risk by Year", 
          subtitle = "Texas Counties, 2000-2007")


# # Create dataset
# final.dat %>%
#   filter(year %in% c(2000)) %>%
#   mutate(qrr = cut(I(rate/E_d), breaks = quantile(I(rate/E_d), p = seq(0,1,length.out = 5)), 
#                    include.lowest = T)) %>%
#   ggplot() + geom_sf(aes(fill = qrr)) +
#   scale_colour_brewer(palette = "RdBu" ) + 
#   scale_fill_brewer(palette = "RdBu", na.value = "grey") + 
#   guides(fill = guide_legend(title = "Relative Risk Quartile")) + 
#   ggtitle(label = "Relative Risk Quartile - raw data, 2000") + 
#   coord_sf(crs = 102008)

# Null model ----------------------------------------
null <- bmiz~ 1 + # Fixed effects
  f(struct, model = "iid") # Random effects
# THis is a gaussian model
fit0 <- inla(null, 
             family = "gaussian", 
             data = indat2, 
             num.threads = 2, 
             verbose = F,  
             control.inla = list(strategy = 'gaussian',  # Gaussian model
                               int.strategy = 'eb'), 
             control.compute = list(dic = T),
             control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)
# Model summary
summary(fit0)
1/fit0$summary.hyperpar$mean
m <- inla.tmarginal(
  function(x) 1/x,
  fit0$marginals.hyperpar$'Precision for struct')
plot(m, 
     type = "l", 
     main = c("Posterior distibution for between county variance", "- Null Model -"))

# Plot observed vs. fitted values
plot(x = fit0$summary.fitted.values$mean, y = final.dat$rate/final.dat$E_d , 
     ylab = "Observed", xlab = "Estimated" )

# Individual-level demographic characteristics ------------------
individ <- bmiz ~ black + hispanic + other + lths + coll + agez + # Fixed effects
  f(struct, model="iid") # Random effects
fit1 <- inla(individ, 
           family = "gaussian", 
           data=indat2, 
           num.threads = 2, 
           verbose = F, 
           control.inla = list(strategy = 'gaussian', 
                               int.strategy = 'eb'), 
           control.compute = list(dic = T,
                                  waic = T),
           control.predictor = list(link = 1)) #estimate predicted values & their marginals or not?)

# Model summary
summary(fit1)
1/fit1$summary.hyperpar$mean

m1 <- inla.tmarginal(
  function(x) 1/x,
  fit1$marginals.hyperpar$'Precision for struct')

plot(m1, 
     type = "l", 
     main = c("Posterior distibution for between county variance", "- Random Intecept Model -"))

# County-level predictor (Z-scored poverty rate) ----------
# Compute these (earlier on)
# cov_dat <- cov_dat %>%
#   mutate(cofips = GEOID,
#        pwhite = P007003/summary_value, 
#        pblack = P007004/summary_value, 
#        phisp = P007010/summary_value,
#        medhhinc = as.numeric(scale(P053001)), 
#        ppov = P089002/P089001)


#Model specification:
f1 <- rate ~ scale(pblack) + scale(phisp) + scale(ppov) + year #Fixed effects
f2 <- bmiz ~ black + hispanic + other + lths + coll + agez + povz + #Fixed effects
  f(struct, model="iid") #Random effects
fit2 <- inla(f1, 
           family = "gaussian", 
           data=indat2, 
           num.threads = 2, 
           verbose = F, 
           control.inla = list(strategy = 'gaussian', 
                               int.strategy = 'eb'), 
           control.compute = list(dic = T,
                                  waic = T),
           control.predictor = list(link = 1))

summary(fit2)

1/fit2$summary.hyperpar$mean

m1<- inla.tmarginal(
  function(x) 1/x,
  fit1$marginals.hyperpar$'Precision for struct')

plot(m1, type="l", main=c("Posterior distibution for between county variance", "- Multi-Level Model -"))

# BYM Model-----------
# Specify spatial connectivity matrix
# final.dat$year_c <- final.dat$year - 2004
f3 <- rate ~ scale(pblack) + scale(phisp) + scale(ppov) + # Fixed effects
  f(struct, model = "bym", constr = TRUE, scale.model = TRUE, graph = H) + # Spatial matrix
  #f(struct, model="bym", graph=mat,constr=TRUE, scale.model=TRUE) + graph = mat
  # f(struct, model="bym", graph="/Users/ozd504/Google Drive/dem7263/data/us.gra",
  #   hyper = list(prec.unstruct=list(initial=5.3), prec.spatial=list(initial=5.3))) # alternative spatial matrix
  f(year, model="iid") #temporal random effect
mod3 <- inla(formula = f3,data = final.dat,
             family = "nbinomial",  # should this be "binomial"?
             E = E_d, 
             control.compute = list(waic = TRUE,
                                    dic = TRUE), 
             num.threads = 3, 
             verbose = F,
             # control.family = list(hyper = list(prec=list(initial= .028))),
             control.predictor = list(link = 1))

#total model summary
summary(mod3)

# Posterior distibution for between county variance
m3a <- inla.tmarginal(
  function(x) (1/x),
  mod3$marginals.hyperpar$`Precision for struct (iid component)`)
m3b <- inla.tmarginal(
  function(x) (1/x),
  mod3$marginals.hyperpar$`Precision for struct (spatial component)`)
m3c <- inla.tmarginal(
  function(x) (1/x),
  mod3$marginals.hyperpar$`Precision for year`)

plot(m3a, type = "l", 
     main = c("Posterior distibution for between county variance", "- IID model -"), 
     xlim = c(0, .07),  ylim = c(0,600))
lines(m3b, col = "red")
lines(m3c, col = "green")

inla.hpdmarginal(.95, m3a)
inla.hpdmarginal(.95, m3b)
inla.hpdmarginal(.95, m3c)
# values show very low spatially correlated variance in data and very low temporal heterogeneity 

# Space-time mapping of the fitted values
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

# Other BYM model -----------------------------------
fit3<-inla(bmiz~black+hispanic+other+lths+coll+ agez+povz+f(struct, model="bym", graph="/Users/ozd504/Google Drive/dem7263/data/us.gra",hyper = list(prec.unstruct=list(initial=5.3), prec.spatial=list(initial=5.3))), family = "gaussian", data=indat2, num.threads = 1, verbose = F, control.inla=list(strategy='gaussian', int.strategy='eb'), control.compute = list(dic=T), control.family = list(hyper = list(prec=list(initial= .028))))
## Warning in INLA::f(struct, model = "bym", graph = "/Users/ozd504/Google

1/fit3$summary.hyperpar$mean

plot(y = mod3$summary.random$year_c$mean,
     x = unique(final.dat$year), type = "l")
m3 <- inla.tmarginal(
  function(x) 1/x,
  fit3$marginals.hyperpar$`Precision for struct (iid component)`)
m32 <- inla.tmarginal(
  function(x) 1/x,
  fit3$marginals.hyperpar$`Precision for struct (spatial component)`)

plot(m3, 
     type = "l", 
     main = c("Posterior distibution for between county variance", "- Multi-level BYM Model IID-")) 
summary(fit3)

plot(m32, type = "l", 
     main = c("Posterior distibution for between county variance", "- Multi-level BYM Model Spatial-"))


# Basic county-level random intercept model --------------------------------
# heterogeneity in mortality rate for each county
f2<- rate ~ scale(pblack) + scale(phisp) + scale(ppov) + year + #fixed effects
  f(struct, model = "iid")  #random effects

mod2 <- inla(formula = f2,
             data = final.dat,
             family = "nbinomial", 
             E = E_d, 
             control.compute = list(waic = T), 
             control.predictor = list(link = 1),
             num.threads = 3, 
             verbose = F)

#total model summary
summary(mod2)

# Plot Marginal distributions of hyperparameters
m2 <- inla.tmarginal(
  function(x) (1/x), #invert the precision to be on variance scale
  mod2$marginals.hyperpar$`Precision for struct`)
inla.hpdmarginal(.95, marginal = m2)
plot(m2, type = "l", 
     main = c("Posterior distibution for between county variance", "- IID model -"), 
     xlim = c(0, .1))

# Observed vs. fitted
#Observed vs. Fitted values
plot(x= mod2$summary.fitted.values$mean, y=final.dat$deaths/final.dat$E_d , ylab="Observed", xlab="Estima
     ted" )
points(x= mod1$summary.fitted.values$mean, y=final.dat$deaths/final.dat$E_d, col=2)
legend("topleft", legend = c("GLM", "GLMM(IID)"), col=c(1,2), pch=1)

final.dat$fitted_m2<-mod2$summary.fitted.values$mean
final.dat%>%
  filter(year%in%c(2000))%>%
  mutate(qrr=cut(fitted_m2, breaks = quantile(fitted_m2, p=seq(0,1,length.out = 8))))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdB
                                                                                           u", na.value="grey")+guides(fill=guide_legend(title="Relative Risk Quartile"))+ggtitle(label="Relative Risk Quartile - IID Model, 2000")+coord_sf(crs = 102008)
final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(fitted_m2, breaks = quantile(fitted_m2, p=seq(0,1,length.out = 8))))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdB
                                                                                           u", na.value="grey")+guides(fill=guide_legend(title="Relative Risk Quartile"))+ggtitle(label="Relative Risk Quartile - IID Model, 2007")+coord_sf(crs = 102008)


map1<-final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(fitted_m2, breaks = quantile(fitted_m2, p=seq(0,1,length.out = 8))))
clrs <- colorRampPalette(brewer.pal(8, "RdBu"))
mapView(as(map1, "Spatial"), zcol="qrr", legend=T, col.regions=clrs, map.types="OpenStreetMap")


# Others from Ian --------


# Zero-inflated Poisson?
model <- inla(ptb ~ dob_yy + combfips + racehisp_recode, family = c("poisson"),
     data = gatestmodel1, control.predictor = list(link = 1))




# Testing -----------------------
# https://gist.github.com/famuvie/639e3aaebba1ba0b1862215b02cccabe
# https://txrdc.tamu.edu/wp-content/uploads/sites/20/2018/03/TAMU_May14_Bayes_workshop.pdf
#


# Code from Ian ----------
#  R-INLA Conditional Autoregressive (CAR) Besag–York–Mollié (BYM) Model

## See Chapter 6 (especially 6.1.2 for BYM) in the "Spatial and Spatio-Temporal Bayesian Models in R-INLA" by Blangiardo and Cameletti (2015)
# Section 6.3 is about ZIP models

#Poisson Intercept model
form_null <- ptb ~ 1 + f(idx, model = "bym", graph = adj) # intercept and bym random effects; graph is set to sparse adjacency matrix
# Can add any variable for multivariate analysis: Case_Count ~ 1 + VAR1 + f(idx, model = "bym", graph = adj)
m_null_pois <- INLA::inla(form_null,
                          data = dat_df, 
                          family = "poisson", # for Poisson model
                          control.predictor = list(compute = T), # to compute predicted values
                          control.compute = list(dic = T, waic = T), # to calculate model performance measurements; here, DIC and WAIC
                          # offset = log(population), # offset here is optional, if expected counts use argument E
                          verbose = FALSE # set to TRUE if want a more detailed print out, helps debugging
)
summary(m_null_pois) # summary of model

# Zero-inflated Poisson 0 ----------------------------
# ZIP0 assumes all zeros are structural

form_null <- Case_Count ~ 1 + f(idx, model = "bym", graph = adj)  # repeat of above
m_null_zip0 <- INLA::inla(form_null, 
                          data = dat_df, 
                          family = "zeroinflatedpoisson0", # for ZIP0 model
                          control.predictor = list(compute = T), 
                          control.compute = list(dic = T, waic = T),
                          # offset = log(population),  # offset here is optional, if expected counts use argument E
                          verbose = FALSE
)
summary(m_null_zip0) # summary of model


# Zero-inflated Poisson 1 ---------------------

# ZIP1 assumes some zeros are structural and some are sampling zeroes

form_null <- Case_Count ~ 1 + f(idx, model = "bym", graph = adj) # repeat of above
m_null_zip1 <- INLA::inla(form_null,
                          data = dat_df, 
                          family = "zeroinflatedpoisson1",  # for ZIP1 model
                          control.predictor = list(compute = T), 
                          control.compute = list(dic = T, waic = T),
                          # offset = log(population),  # offset here is optional, if expected counts use argument E
                          verbose = FALSE
)
summary(m_null_zip1) # summary of model


# Hyperparameter Controls  --------------------


# BYM has spatially unstructured and spatially structured random effects
# Can control the priors on both

form_null_control <- Case_Count ~ 1 + f(idx, model = "bym", graph = adj,
                                        hyper = list(prec.unstruct= list(prior = "loggamma", # spatially unstructured random effect (v)
                                                                         param = c(0.001,0.001) # modify as desired, default = logGamma(1,0.0005)
                                        ),
                                        prec.spatial = list(prior = "loggamma", # spatially structured random effect (u)
                                                            param = c(0.01,0.01) # modify as desired, default = logGamma(1,0.0005)
                                        )
                                        )
                                        
)

m_null_control <- INLA::inla(form_null_control, 
                             family = "poisson", 
                             data = dat_df, 
                             control.predictor = list(compute = T), 
                             control.compute = list(dic = T, waic = T),
                             control.inla = list(tolerance = 1e-20, # can control precision
                                                 h = 1e-08 # can control h (search step parameter)
                             ),
                             # offset = log(population),  # offset here is optional, if expected counts use argument E
                             verbose = FALSE
)
summary(m_null_control) # summary of model


# Export -----------------
save(m_null_pois, m_null_zip0, m_null_zip1, m_null_control,
     file = "inla_bym.Rdata"
)

# Interpretation -----------------


# All interpretation of parameters is done on a log scale, so
# exp(β)=% change in the mean
# round(exp(coef(usfitbin)), 3)


# Check if any fixed effect 95% credibility interval span the null value (0)
# If it does not, statistically significant variable
# Intercept is the average outcome rate in the entire study region
# Exponentiate fixed effect coefficient means and 95% credibility interval
# Examine uncertainty of model by looking at the random effects and proportion of variance

## Exponentiate fixed effect coefficients and random effects
# Either exp() of the summary values (mean, 0.025quant, 0.975quant) or using the marginals (for some reason they can be slightly different)
exp(m_null_pois$summary.fixed[1,1]) # mean beta_0
exp(m_null_pois$summary.fixed[1,3]) # lower 95%CI beta_0
exp(m_null_pois$summary.fixed[1,5]) # upper 95%CI beta_0

exp.b0.mean <- INLA::inla.emarginal(exp,m_null_pois$marginals.fixed[[1]]) # mean beta_0
exp.b0.95CI <- INLA::inla.qmarginal(c(0.025,0.975), # 95%CI beta_0
                                    INLA::inla.tmarginal(exp,m_null_pois$marginals.fixed[[1]])
)
# REPEAT FOR ANY OTHER VARIABLES IN THE MODEL #

# If no other risk factors in the model (Disease Map), the county-specific relative risks are the exp(u + v)
# If other risk factors in the model (Ecological Regression), the county-specific residuals relative risks are the exp(u + v) compared to the entire extent after the risk factors are taken into account

nAreas <- nrow(dat_df) # number of areal units
UHres <-  m_null_pois$summary.random$idx$mean[1:nAreas] # area specific spatially unstructured residuals
SHres <-  m_null_pois$summary.random$idx$mean[(nAreas+1):(2*nAreas)] # area specific spatially structured residuals
csi <- m_null_pois$marginals.random$idx[1:nAreas] # reminder the first set of random effect marginals are u + v
zeta <- lapply(UH_coef, function(x) INLA::inla.emarginal(exp,x)) # posterior mean for random effects (relative risk, or if offset then counts)

a <- 0
prob_excess <- lapply(csi, function(x){1-INLA::inla.pmarginal(a,x)}) # posterior probability of (residual) relative risk exceeding 1 (excess risk)

## Examine uncertainty
# Proportion of variance
mat.marg <- matrix(NA, nrow=nAreas, ncol=100000) # create matrix to sample marginals
m <- m_null_pois$marginals.random$idx
for (i in 1:nAreas){
  #first nAreas values of the random effects are UH (u+v)
  #second nAreas values of the random effects are SH (u)
  u <- m[[nAreas+i]]
  mat.marg[i,] <- INLA::inla.rmarginal(100000, u)
}
var.SH <- apply(mat.marg, 2, var) # emperical variance of spatially structured random effect

var.UH <- INLA::inla.rmarginal(100000, # expected variance of spatially unstructured random effect
                               INLA::inla.tmarginal(function(x) 1/x, m_null_pois$marginals.hyper$`Precision for ID (iid component)`)
)

perc.var.SH <- mean(var.SH/(var.SH+var.UH)) # calculate proportion of variance
perc.var.SH # View proportion of variance
# If closer to 1 then a large part of the variability is explained by the spatial structure
# Separate calculations if model was scaled (option scale.model = TRUE) see Blangiardo and Cameletti pp 186

## For ZIP models, pull out and interpret the zero-probability parameter
# inverse logit transformation of pi_0
# probability of pi_0 should be higher in ZIP0 model than ZIP1 models
# default = Normal(-2,1)
m_null_zip1$summary.hyperpar[1,] 
prob_zero1 <- m_null_zip1$summary.hyper[1,1]+(1-m_null_zip1$summary.hyper[1,1])*exp(-exp(m_null_zip1$summary.linear.predictor$mean)*log(spdf$population)) # area specific zero-probability 

# Any area specific predicted values have a 95% CI that crosses the null (0)
ci95 <- m_null_pois$summary.fitted.values[,c(3,5)]
ci95$sig <- with(ci95, ci95$'0.025quant' < 0 & ci95$'0.975quant' > 0) # can plot these, too (FALSE = Significant b.c. TRUE = cross null value)
table(is.na(ci95$sig)) # global count of CIs that cross null (0)

# size of 95% CI
ci95range <- abs(m_null_pois$summary.fitted.values[,5] - m_null_pois$summary.fitted.values[,3]) # upper 95% CI - lower 95% CI


# Data visualization ---------------------------

# Append calculated values to SPDF
spdf$m_null_pois_fit <- m_null_pois$summary.fitted.values[,1] # area specific predicted mean values (RR if no offset, counts if offset)
spdf$m_null_pois_sd <- m_null_pois$summary.fitted.values[,2] # area specific predicted sd values (RR if no offset, counts if offset)
spdf$m_null_pois_ci95range <- ci95range # area specific size of 95% CI of predicted values (RR if no offset, counts if offset)
spdf$m_null_pois_UH <- UHres # area specific spatially unstructured residuals
spdf$m_null_pois_SH <- SHres # area specific spatially structured residuals
spdf$m_null_pois_RR <- unlist(zeta)/log(population) # area specific (residual) relative risk (if using offset to convert back to risk)
spdf$m_null_pois_RR <- unlist(zeta) # area specific (residual) relative risk (if not using offset because already risk)
spdf$m_null_pois_eRR <- unlist(probUH) # area specific probability of excess risk UH
spdf$m_null_pois_DIC <- m_null_pois$dic$local.dic # area specific DIC
spdf$m_null_pois_WAIC <- m_null_pois$dic$local.dic # area specific WAIC
spdf$m_null_zip1_probZ <- prob_zero1 # area specific probability of observing zero for ZIP1

# Plot
# col_lab <- XXXXXX # names of colorkey labels in vector
# at_lab <- XXXXX # location of colorkey lables in vector
# at_plot <- XXXXX # location of color breaks of plot in vector
# font_fam <- XXXXX # font family as a character
exfact <- 1 # PNG expansion factor (for higher res images)
grDevices::png("m_null_pois_fit.png", width = 2000*exfact, height = 2000*exfact) # create PNG
sp::spplot(spdf,  # SPDF
           "m_null_pois_fit", # column, variable
           col.regions=c("transparent", rev(colorspace::sequential_hcl(101))), # color scheme
           par.settings = list(axis.line = list(col =  'transparent')), # remove axess for maps
           colorkey=list(labels = list(labels = col_lab, # for custom colorkey labels
                                       at = at_lab, # for custom colorkey label positions, usually reduce the bottom and top number so they fit within window
                                       cex = 4*exfact, # for custom size of labels
                                       fontfamily= font_fam, # for custom font of labels
                                       fontface = 1), # for direction of labels
                         width = 1*exfact # for custom width of colorkey
           ),
           at = at_plot, # set custom color breaks
           lwd = 1*exfact  # custom line width
)
grid::grid.text("INSERT LABEL FOR COLORKEY", # for overall label of colorkey
                x=unit(0.925, "npc"), # x position
                y=unit(0.50, "npc"), # y position
                rot=90, # for vertical colorkey
                gp = gpar(fontsize = 50*exfact, # for custom size of overall label
                          fontfamily= font_fam # for custom font of overall label
                )
)
dev.off() # turn off image device


# Read in Config file ------------------
# config <- load_config(repo = repo,
#                       indicator_group = indicator_group,
#                       indicator = indicator)
