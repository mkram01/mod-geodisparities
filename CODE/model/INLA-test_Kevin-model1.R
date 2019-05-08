## INLA Model 1
## Year, County, 2-Category Race/Ethnicity
## 04/24/2019

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

## Assign FIPS Code lists
# Census Regions
southfips <- c('10', '11', '12','13', '24', '37', '45', '51', '54', '01', '21', '28', '47', '05', '22', '40', '48')
westfips <- c('04', '08', '16', '30', '32', '35', '49', '56', '02', '06', '15', '41', '53')
midwest <- c('17', '18', '26', '39', '55', '19', '20', '27', '29', '31', '38', '46')
northeastfips <- c('09', '23', '25', '33', '44', '50', '34', '36', '42')

# Census Divisions
newenglandfips <- c('09', '23', '25', '33', '44', '50')
midatlanticfips <- c('34', '36', '42')
southatlanticfips <- c('10', '11', '12','13', '24', '37', '45', '51', '54')
escfips <- c('01', '21', '28', '47')
wscfips <- c('05', '22', '40', '48')
encfips <- c('17', '18', '26', '39', '55')
wncfips <- c('19', '20', '27', '29', '31', '38', '46')
pacfips <- c('02', '06', '15', '41', '53')
mountainfips <- c('04', '08', '16', '30', '32', '35', '49', '56')

# Create summarized dataset
southatlantic <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode %in% c(2,3),
         substr(combfips,1,2) %in% southatlanticfips,
         dob_yy %in% 2007:2016) %>%
  mutate(black = ifelse(racehisp_recode == 3, 1, 0),
         combfips = factor(combfips)) %>%
  group_by(dob_yy, combfips, black) %>%
  summarise(vptb = sum(vptb) + 1,
            ptb = sum(ptb) + 1,
            births = sum(births) + 1)


## Spatial data ----------------------------------------------------------------
## Prep spatial data for region only
#Read in national county shapefile and save in MOD folder as `.gpkg`.
cty_sf <- st_read('../../data/spatial/cb_2016_us_county_500k.shp') %>%
  filter(STATEFP %in% southatlanticfips) %>%
  st_transform(102003) # Albers Equal Area
st_write(cty_sf, '../../data/spatial/southatlantic_county.gpkg', delete_dsn = T)

## Read in spatial data
# This is the data saved above. It is saved as `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.

southatlantic_sp <- st_read('../../data/spatial/southatlantic_county.gpkg') %>%
  inner_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
            ptb = sum(ptb),
            births = sum(births),
            rawvptb = vptb / births * 1000,
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

# Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
southatlantic_sp$ID <- seq_len(nrow(southatlantic_sp))

# Create long version (e.g. repeated rows for each year within county) as an
# `sf` object useful for facet printing of year x race.

southatlantic_sf <- southatlantic_sp %>%
  st_as_sf() %>%
  select(GEOID, ID) %>%
  right_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
         ID2 = ID, # ID and ID2 will be for f() in INLA
         year_c = dob_yy - 2007)  %>%# scale year so intercept interpretable
  arrange(ID)


## Create adjacency matrix using k-nearest neighbors
## I'm going to use k-nearest neighbors, with $k=6$. I will force symmetry so
## that if $a$ is neighbors with $b$, then $b$ must also be neighbors with $a$.

# Create knn neighbor object
southatlantic_knn <- southatlantic_sp %>%
coordinates() %>%  # get centroids
knearneigh(k = 6) %>% # calculate the 6 nearest neighbors
knn2nb(sym = T)  # knn neighbor object

# Write an INLA adjacency file
nb2INLA('../../data/spatial/southatlantic_knn6.adj', southatlantic_knn)

# Look at KNN object
summary(southatlantic_knn)
#plot(southatlantic_knn, coords = coordinates(as(cty_sf, 'Spatial')))
#So counties have from 6 to 11 neighbors (>6 because of forced symmetry).

#View INLA object
g <- inla.read.graph('../../data/spatial/southatlantic_knn6.adj')
summary(g)
plot(g)

# Summary of object
# * n is the size of the graph (e.g. the number of areal units)
# * nnbs are the number of neighbors for each node.
# The summary (names) tells us that there are either $4, 5, 6$ or $7$
# neighbors per node
# (count) says how many counties have each number of neighbors. __Note that
# the * symmetry = T above is what forced some counties to have > 4 neighbors__
# * ncc is about _connected components_ of the graph and this tells us how many are in the list (e.g. only 1)


# Create basemap of raw (pooled) rates
# These are rates pooled across all years and simply serve a reference for
# when we start getting modeled estimates.
tm_shape(southatlantic_sp) +
  tm_fill('rawptb',
          style = 'quantile') +
  tm_borders()

# INLA Models
# Use regional object object as input data (`nrow(region)`
# rows = `length(unique(region$combfips))` # counties x
# `length(unique(region$dob_yy))` years x
# `length(unique(southatlantic$black))` race groups age categories).

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
summary(m1)
# Interpretation of model 1
# * Baseline risk in 2007 for whites is $exp(intercept)$ = `r round(exp(m1$summary.fixed$mean[1]), digits = 4) * 1000` *preterm births* per 1,000 live births.
# * There is a modest increasing rate with annual relative increase of `r round(exp(m1$summary.fixed$mean[2]), digits = 2)`.
# * The black-white disparity across the entire study period is `r round(exp(m1$summary.fixed$mean[3]), digits = 2)`


### Extracting marginal posterior estimates
# This is the posterior of the (aspatial) random effect for county on the scale of risk ratio/SMR:
southatlantic_sp$m1_iid_re <- unlist(lapply(m1$marginals.random$ID,
                                      function(x) inla.emarginal(exp, x)))
# This is the posterior of the fitted values. Note that there are
# n=1590 fitted values (unlike the n=159 random effects),
# so I will add this to the `region_sf` dataset.
southatlantic_sf$m1_fit <- unlist(lapply(m1$marginals.fitted.values,
                                   function(x) inla.emarginal(mean, x))) /
  southatlantic_sf$births


### Mapping Model 1 posterior
# These are the relative deviations of each county from the global average
# preterm birth rate controlling for time trend and race.
tm_shape(southatlantic_sp) +
  tm_fill('m1_iid_re',
          style = 'quantile',
          palette = 'div',
          midpoint = 1) +
  tm_borders()

# Mapping the race x year specific rates. **NOTE** that these rates only include
# fixed effects for race and year, and thus differences over time only reflect
# the modest fixed slope for year.

tm_shape(southatlantic_sf) +
  tm_fill('m1_fit',
          style = 'quantile') +
  tm_borders() +
  tm_facets(by = c('year_c', 'black'))

## M2: Poisson Random intercept, `bym` -------------------------------------
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
summary(m2)

# Something seems odd...the DIC and the fixed effects estimates for model M2
# with `bym` prior are identical to model M1 with `iid` prior...need to
# investigate further. **NOTE:** I also tried changing model M2 to a `besag` +
# `iid` and it gives same DIC as `bym`, but `besag` alone has poorer fit
# suggesting there is no added info from complexity of spatial prior. Need to
# re-check this with other outcomes or other states.

### Extracting marginal posterior estimates
### Looking at the random effects. The object `m2$summary.random$id` is the mean
### and quantile from the posterior of each random effect.
dim(m2$summary.random$ID)

#The dimensions of the `summary.random$ID` object within the model is $2n$ rows,
#where *n = counties*. The **first 159** rows are the *area-specific residual*
#which is to say the sum of the spatially structured and the spatially
#unstructured random effects. The **second 159** rows is the spatially
#structured random effects only.
round(head(m2$summary.random$ID), 3)

# To extract the area-specific residuals (sum of two random effects), and put
# them on the *relative risk* scale. Note the object `m2$marginals.random$ID`
# is an approximation of the posterior distribution. Using the function
# `inla.emarginal()` computes the expected marginal given that summary object.
# NOTE:** because the random effects are *per-county* there will only be *n=159*, therefore putting these in the wide file, `southatlantic_sp`
nCounty <- length(unique(southatlantic_sf$GEOID)) # how many unique geographies
re2 <- m2$marginals.random$ID[1:nCounty] # extract only the area total residuals
southatlantic_sp$smr2 <- unlist(lapply(re2, function(x) inla.emarginal(exp, x)))

# Get fitted posterior marginals for each year x race group:
southatlantic_sf$m2_fit <- unlist(lapply(m2$marginals.fitted.values,
                                   function(x) inla.emarginal(mean, x))) /
southatlantic_sf$births

### Mapping M2 random effects
# Map of the area-specific random effects (relative SMR conditional on race, year):
tm_shape(southatlantic_sp) +
  tm_fill('smr2',
          style = 'quantile',
          palette = 'div') +
  tm_borders()

# Map of the year x race facets, which is still not that interesting because the
# year effect is still fixed, so the change down columns is just 1% per year.
tm_shape(southatlantic_sf) +
  tm_fill('m2_fit',
          style = 'quantile') +
  tm_borders() +
  tm_facets(c('year_c', 'black'))

## M3: Poisson Random slope for race + random bym intercept --------------------
# This model has spatial prior on random intercept, and an
# independent (aspatial) prior on the random slope for race, allowing black-white
# disparities to vary by county. Year is still a fixed effect.
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
summary(m3)
#This does have modestly better fit (DIC) compared to M1 or M2.

### Extracting marginal posterior for Model 3
# From this model I am interested in the area-specific residuals (same as in M2),
# but can also now get the area-specific racial disparities.
# First, the county residuals (spatially structured + unstructured):
re3 <- m3$marginals.random$ID[1:nCounty] # extract only the area total residuals
southatlantic_sp$smr3 <- unlist(lapply(re3, function(x) inla.emarginal(exp, x)))

# Second, the county racial disparities (these are iid, aspatial), and are
# denoted by their grouping variable, `ID2` (which is just a copy of `ID`).
# I am interested in the overall Black-White disparity, so am adding the
# random slope to the fixed effect for race
bl3 <- m3$marginals.random$ID2
bltemp <- unlist(lapply(bl3, function(x) inla.emarginal(mean, x)))
southatlantic_sp$rrBlack <- exp(bltemp + m3$summary.fixed$mean[3])

### Mapping M3 random effects
tm_shape(southatlantic_sp) +
  tm_fill(c('smr3', 'rrBlack'),
          style = 'quantile',
          palette = 'div') +
  tm_borders()


## M4: Poisson BYM spatial slope for race + spatial bym intercept --------------
# This model has spatial prior on random intercept, and an independent
# (aspatial) prior on the random slope for race, allowing black-white
# disparities to vary by county. Year is still a fixed effect.

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
summary(m4)

### Extracting marginal posterior
### From this model I am interested in the area-specific residuals
### (same as in M2), but can also now get the area-specific racial disparities.

# First, the county residuals (spatially structured + unstructured):
re4 <- m4$marginals.random$ID[1:nCounty] # extract only the area total residuals
southatlantic_sp$smr4 <- unlist(lapply(re4, function(x) inla.emarginal(exp, x)))

# Second, the county racial disparities (these are iid, aspatial), and are
# denoted by their grouping variable, `ID2` (which is just a copy of `ID`). I am
# interested in the overall Black-White disparity, so am adding the random
# spatially structured slope to the fixed effect for race
bl4 <- m4$marginals.random$ID2[1:nCounty]
bltemp <- unlist(lapply(bl4, function(x) inla.emarginal(mean, x)))
southatlantic_sp$rrBlack <- exp(bltemp + m4$summary.fixed$mean[3])

### Mapping M4 random effects
tm_shape(southatlantic_sp) +
  tm_fill(c('smr4', 'rrBlack'),
          style = 'quantile',
          palette = 'div') +
  tm_borders()


## M5: Neg Bin Random intercept, iid ------------------------------------------
# Random intercept for county with independent (aspatial) prior.
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
summary(m5)
# (Unaltered) Interpretation:**
# * Baseline risk in 2007 for whites is $exp(intercept)$ = `r round(exp(m1$summary.fixed$mean[1]), digits = 4) * 1000` *preterm births* per 1,000 live births.
# * There is a modest increasing rate with annual relative increase of `r round(exp(m1$summary.fixed$mean[2]), digits = 2)`.
# * The black-white disparity across the entire study period is `r round(exp(m1$summary.fixed$mean[3]), digits = 2)`


### Extracting marginal posterior estimates
# This is the posterior of the (aspatial) random effect for county on the
# scale of risk ratio/SMR:
southatlantic_sp$m5_iid_re <- unlist(lapply(m5$marginals.random$ID,
                                      function(x) inla.emarginal(exp, x)))
# This is the posterior of the fitted values. Note that there are n=1590 fitted
# values (unlike the n=159 random effects), so I will add this to the `region_sf` dataset.
southatlantic_sf$m5_fit <- unlist(lapply(m5$marginals.fitted.values,
                                   function(x) inla.emarginal(mean, x))) /
  southatlantic_sf$births

### Mapping Model 5 posterior
# These are the relative deviations of each county from the global average
#  preterm birth rate controlling for time trend and race.
tm_shape(southatlantic_sp) +
   tm_fill('m5_iid_re',
           style = 'quantile',
           palette = 'div',
           midpoint = 1) +
  tm_borders()

# Mapping the race x year specific rates. **NOTE** that these rates only include
# fixed effects for race and year, and thus differences over time only reflect the modest fixed slope for year.
tm_shape(southatlantic_sf) +
  tm_fill('m5_fit',
          style = 'quantile') +
  tm_borders() +
  tm_facets(by = c('year_c', 'black'))

## M6: Neg Bin Random intercept, `bym` -----------------------------------------
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
summary(m6)

### Extracting marginal posterior estimates
# Looking at the random effects. The object `m2$summary.random$id` is the mean and
# quantile from the posterior of each random effect.
dim(m6$summary.random$ID)

# The dimensions of the `summary.random$ID` object within the model is $2n$ rows,
# where *n = counties*. The **first 159** rows are the *area-specific residual*
# which is to say the sum of the spatially structured and the spatially
# unstructured random effects. The **second 159** rows is the spatially
# structured random effects only.
round(head(m6$summary.random$ID), 3)

# To extract the area-specific residuals (sum of two random effects), and put
# them on the *relative risk* scale. Note the object `m2$marginals.random$ID`
# is an approximation of the posterior distribution. Using the function
# `inla.emarginal()` computes the expected marginal given that summary object.

# **NOTE:** because the random effects are *per-county* there will only be *n=159*, therefore putting these in the wide file, `southatlantic_sp`
nCounty <- length(unique(southatlantic_sf$GEOID)) # how many unique geographies
re6 <- m6$marginals.random$ID[1:nCounty] # extract only the area total residuals
southatlantic_sp$smr6 <- unlist(lapply(re6,
                                 function(x) inla.emarginal(exp, x)))

# Get fitted posterior marginals for each year x race group:
southatlantic_sf$m6_fit <- unlist(lapply(m6$marginals.fitted.values,
                                   function(x) inla.emarginal(mean, x))) /
   southatlantic_sf$births

### Mapping M6 random effects
# Map of the area-specific random effects (relative SMR conditional on race, year):
tm_shape(southatlantic_sp) +
  tm_fill('smr6',
          style = 'quantile',
          palette = 'div') +
  tm_borders()

# And a map of the year x race facets, which is still not that interesting
# because the year effect is still fixed, so the change down columns is just 1% per year.
tm_shape(southatlantic_sf) +
  tm_fill('m6_fit',
          style = 'quantile') +
  tm_borders() +
  tm_facets(c('year_c', 'black'))

## M7: Neg Bin Random slope for race + random bym intercept --------------------
# This model has spatial prior on random intercept, and an independent
# (aspatial) prior on the random slope for race, allowing black-white
# disparities to vary by county. Year is still a fixed effect.
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
summary(m7)

### Extracting marginal posterior
# From this model I am interested in the area-specific residuals (same as in M2),
# but can also now get the area-specific racial disparities.
# First, the county residuals (spatially structured + unstructured):
re7 <- m7$marginals.random$ID[1:nCounty] # extract only the area total residuals
southatlantic_sp$smr7 <- unlist(lapply(re7,
                                 function(x) inla.emarginal(exp, x)))

# Second, the county racial disparities (these are iid, aspatial), and are
# denoted by their grouping variable, `ID2` (which is just a copy of `ID`). I am
# interested in the overall Black-White disparity, so am adding the random slope
# to the fixed effect for race
bl7 <- m7$marginals.random$ID2
bltemp <- unlist(lapply(bl7,
                        function(x) inla.emarginal(mean, x)))
southatlantic_sp$rrBlack <- exp(bltemp + m7$summary.fixed$mean[3])

### Mapping M7 random effects
tm_shape(southatlantic_sp) +
  tm_fill(c('smr7', 'rrBlack'),
          style = 'quantile',
          palette = 'div') +
  tm_borders()

## M8: Neg Bin BYM spatial slope for race + spatial bym intercept --------------
# This model has spatial prior on random intercept, and an independent (aspatial)
#  prior on the random slope for race, allowing black-white disparities to vary
#  by county. Year is still a fixed effect.
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
summary(m8)

### Extracting marginal posterior
# From this model I am interested in the area-specific residuals (same as in M2),
# but can also now get the area-specific racial disparities.
# First, the county residuals (spatially structured + unstructured):
re8 <- m8$marginals.random$ID[1:nCounty] # extract only the area total residuals
southatlantic_sp$smr8 <- unlist(lapply(re8,
                                 function(x) inla.emarginal(exp, x)))

# Second, the county racial disparities (these are iid, aspatial), and are
# denoted by their grouping variable, `ID2` (which is just a copy of `ID`).
# I am interested in the overall Black-White disparity, so am adding the random
# spatially structured slope to the fixed effect for race
bl8 <- m8$marginals.random$ID2[1:nCounty]
bltemp <- unlist(lapply(bl8, function(x) inla.emarginal(mean, x)))
southatlantic_sp$rrBlack <- exp(bltemp + m8$summary.fixed$mean[3])

### Mapping M8 random effects
tm_shape(southatlantic_sp) +
  tm_fill(c('smr8', 'rrBlack'),
          style = 'quantile',
          palette = 'div') +
  tm_borders()

# Model checking and selection -------------------------------------------------
# For model checking we are interested in whether the model is performing well
# using the predictive distribution to evaluate if the assumptions on the model
# are plausible and to check for outliers.

# Two indices in INLA use cross-validation for evaluating goodness of fit of model:
# the *conditional predictive ordinate* (CPO) and the
# *probability integral transform* (PIT).
# This is covered pages 165-171 in the Blangiardo & Cameletti book.

# First, check that the estimation of CPT and PIT was succesful:
sum(m1$cpo$failure)
sum(m2$cpo$failure)
sum(m3$cpo$failure)
sum(m4$cpo$failure)
sum(m5$cpo$failure)
sum(m6$cpo$failure)
sum(m7$cpo$failure)
sum(m8$cpo$failure)
# So there were no failures in estimating this, which is important for next steps.

### Histogram of PIT
# Second, look at the distribution of the PIT, where tendency towards a
# uniform distribution is indicative of reasonable predictive fit.
hist(m1$cpo$pit)
hist(m2$cpo$pit)
hist(m3$cpo$pit)
hist(m4$cpo$pit)
hist(m5$cpo$pit)
hist(m6$cpo$pit)
hist(m7$cpo$pit)
hist(m8$cpo$pit)
# This does NOT look uniform, so would want to evaluate alternative models!!

### Comparing log(cpo)
# Third, the sum of the log of the CPO can be used to compare among competing
# models, where *larger values denote better fitting model*.
sum(log(m1$cpo$cpo))
sum(log(m2$cpo$cpo))
sum(log(m3$cpo$cpo))
sum(log(m4$cpo$cpo))
sum(log(m5$cpo$cpo))
sum(log(m6$cpo$cpo))
sum(log(m7$cpo$cpo))
sum(log(m8$cpo$cpo))
# Ok, this is slightly different from DIC above (which looked identical in M1
# and M2). Here it looks like the best fitting (largest) is M3, and that
# M1 (aspatial) is just a tiny, tiny bit 'better' than M2. Not big enough to
# make decision, but at least not identical, as it appeard in DIC.

### Posterior predictive check
# The *posterior predictive distribution* is likelihood of replicate data $y_i^*$,
# having observed data $y$. The *posterior predictive p-value* is the
# $p(y_i^* <y_i|y)$. Here, we are using Model 3 as a test.
predicted.p.value <- c()
n <- nrow(southatlantic_sf)
for (i in 1:n) {
  predicted.p.value[i] <- inla.pmarginal(q = southatlantic_sf$ptb[i],
                                         marginal = m3$marginals.fitted.values[[i]])
  }

# Now plot the posterior means for the predictive distribution versus the observed values:
plot(southatlantic_sf$ptb, m3$summary.fitted.values$mean,
xlab = 'Observed PT count', ylab = 'Mean posterior predicted count')

# So in general predictions are close to observed values.
hist(predicted.p.value, main = '',
     xlab = 'Posterior Predictive p-values')

# If the model were *good-fitting* we would expect most of the p-values to be
# in the middle...to not be significantly different. The fact that there are
# many at extremes (high and low probability) suggests poor fit.


# Model Comparison
df <- rbind(cbind(m1$dic$dic, m1$waic$waic, sum(log(m1$cpo$cpo))),
            cbind(m2$dic$dic, m2$waic$waic, sum(log(m2$cpo$cpo))),
            cbind(m3$dic$dic, m3$waic$waic, sum(log(m3$cpo$cpo))),
            cbind(m4$dic$dic, m4$waic$waic, sum(log(m4$cpo$cpo))),
            cbind(m5$dic$dic, m5$waic$waic, sum(log(m5$cpo$cpo))),
            cbind(m6$dic$dic, m6$waic$waic, sum(log(m6$cpo$cpo))),
            cbind(m7$dic$dic, m7$waic$waic, sum(log(m7$cpo$cpo))),
            cbind(m8$dic$dic, m8$waic$waic, sum(log(m8$cpo$cpo))))
colnames(df) <- c("DIC", "WAIC", "Sum(log(CPO))")
rownames(df) <- c("M1: Poisson iid intercept",
                  "M2: Poisson bym intercept",
                  "M3: Poisson bym intercept, iid race slope",
                  "M4: Poisson bym intercept, bym race slope",
                  "M5: Neg Bin iid intercept",
                  "M6: Neg Bin bym intercept",
                  "M7: Neg Bin bym intercept, iid race slope",
                  "M8: Neg Bin bym intercept, bym race slope")
print(df)
