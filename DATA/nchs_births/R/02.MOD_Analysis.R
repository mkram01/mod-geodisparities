# March of Dimes Analysis #
# 02: Analysis Script     #
# Kevin Weiss             #
# 02/04/2019              #

## Package and Data setup -----------------
rm(list = ls())
library(sas7bdat)
library(haven)
library(Hmisc)
library(feather)
library(dplyr)
library(plyr)
library(tidyr)
library(magrittr)
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

# Data Description

# Model 1: YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
# Model 2: YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
# Model 3: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS
# Model 4: YEAR x COUNTY x RACE restricted to NH-Black/NH-White (including multiple births)
# Model 5: YEAR x COUNTY x RACE x AGE (including multiple births)
# Model 6: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION (including multiple births)


# Zero-inflated Poisson?
model <- inla(ptb ~ dob_yy + combfips + racehisp_recode, family = c("poisson"), 
     data = gatestmodel1, control.predictor = list(link = 1)) 


# Testing -----------------------
# Links:
# https://haakonbakka.bitbucket.io/alltopics.html
# https://haakonbakka.bitbucket.io/organisedtopics.html
# https://haakonbakka.bitbucket.io/btopic102.html
# https://haakonbakka.bitbucket.io/btopic116.html
# https://haakonbakka.bitbucket.io/btopic115.html
# https://haakonbakka.bitbucket.io/btopic125.html
# http://discovery.ucl.ac.uk/1415919/1/Baio_BlaCamBaiRue.pdf
# https://www.stat.washington.edu/peter/591/INLA.html

formula <- Petal.Length ~ 1 + Petal.Width
output <- inla(formula, family = "gaussian", data = iris)
summary(output)

# output commands
output$summary.fixed
output$summary.hyperpar

# posterior marginal distribution
output$marginals.fixed
output$marginals.hyperpar

# plot posterior distribution of fixed parameter
beta1_post <- output$marginals.fixed$Petal.Width
plot(beta1_post, type = "l",xlab = expression(beta[1]),
       ylab = expression(tilde(p)(paste(beta[1],"|",y))))

# quantile (95% credibility interval)
inla.qmarginal(0.025, beta1_post)
inla.qmarginal(0.975, beta1_post)


# Hyper parameter
names(output$marginals.hyperpar)
prec_post <- output$marginals.hyperpar$"Precision for the Gaussian observations"
# an equivalent command is
prec_post <- output$marginals.hyperpar[[1]]
inla.emarginal(fun = function(x) 1/x, marg = prec_post)

# SD of hyperparameter variance
m1 <- inla.emarginal(function(x) 1/x, prec_post)
m2 <- inla.emarginal(function(x) (1/x)^2, prec_post)
sd <- sqrt(m2 - m1^2)
sd

# Plot posterior distribution of hyperparameter variance
# inla.tmarginal transforms given marginal distribution
plot.default(inla.tmarginal(function(x) 1/x,prec_post),
             type = "l",xlab = expression(sigmâ2),
             ylab = expression(tilde(p)(paste(sigmâ2,"|",y))))




# Read in Config file ------------------
config <- load_config(repo = repo,
                      indicator_group = indicator_group,
                      indicator = indicator)
