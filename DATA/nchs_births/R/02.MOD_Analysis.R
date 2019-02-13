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
gatestmodel1singleyear <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1singleyear.rda")

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
# http://www.maths.bath.ac.uk/~jjf23/inla/multilevel.html
# https://rpubs.com/corey_sparks/132760
# http://www.maths.bath.ac.uk/~jjf23/inla/multiple.html
# https://gist.github.com/famuvie/639e3aaebba1ba0b1862215b02cccabe
# https://txrdc.tamu.edu/wp-content/uploads/sites/20/2018/03/TAMU_May14_Bayes_workshop.pdf
#

#Corey Sparks Rpubs
library(INLA)
library(car)
#load brfss
load("~/Google Drive/dem7283/data/brfss_11.Rdata")
nams <- names(brfss_11)
newnames <- gsub("_", "", nams)
names(brfss_11)<-tolower(newnames)

brfss_11$statefip<-sprintf("%02d", brfss_11$state )
brfss_11$cofip<-sprintf("%03d", brfss_11$cnty )
brfss_11$cofips<-paste(brfss_11$statefip, brfss_11$cofip, sep="")

#bmi
brfss_11$bmi<-ifelse(is.na(brfss_11$bmi5)==T, NA, brfss_11$bmi5/100)
#poor or fair health
brfss_11$badhealth<-ifelse(brfss_11$genhlth %in% c(4,5),1,0)

#race
brfss_11$black<-recode(brfss_11$racegr2, recodes="2=1; 9=NA; else=0", as.factor.result=T)
brfss_11$white<-recode(brfss_11$racegr2, recodes="1=1; 9=NA; else=0", as.factor.result=T)
brfss_11$other<-recode(brfss_11$racegr2, recodes="3:4=1; 9=NA; else=0", as.factor.result=T)
brfss_11$hispanic<-recode(brfss_11$racegr2, recodes="5=1; 9=NA; else=0", as.factor.result=T)

#education level
brfss_11$lths<-recode(brfss_11$educa, recodes="1:3=1;9=NA; else=0", as.factor.result=F)
brfss_11$coll<-recode(brfss_11$educa, recodes="5:6=1;9=NA; else=0", as.factor.result=F)

#employment
brfss_11$employ<-recode(brfss_11$employ, recodes="1:2='Employed'; 2:6='nilf'; 7='retired'; 8='unable'; else=NA", as.factor.result=T)
brfss_11$employ<-relevel(brfss_11$employ, ref='Employed')

#marital status
brfss_11$marst<-recode(brfss_11$marital, recodes="1='married'; 2='divorced'; 3='widowed'; 4='separated'; 5='nm';6='cohab'; else=NA", as.factor.result=T)
brfss_11$marst<-relevel(brfss_11$marst, ref='married')

#income
brfss_11$inc<-as.factor(ifelse(brfss_11$incomg==9, NA, brfss_11$incomg))

#Age cut into intervals
brfss_11$agec<-cut(brfss_11$age, breaks=c(0,24,39,59,79,99))

# higher-level predictors
acsecon<-read.csv("~/Google Drive/dem7283/data/aff_download/ACS_10_5YR_DP03_with_ann.csv")
acsecon$povrate<-acsecon[, "HC03_VC156"]
acsecon$unemployed<-acsecon[, "HC03_VC13"]
acsecon$cofips<-substr(acsecon$GEO.id, 10,14)
acsecon$povz<-scale(acsecon$povrate, center=T, scale=T)
acsecon$unempz<-scale(acsecon$unemployed, center=T, scale=T)
acsecon<-acsecon[, c("cofips", "povrate","povz", "unemployed","unempz")]

head(acsecon)

# join data
joindata<-merge(brfss_11, acsecon, by="cofips", all.x=T, all.y=F)
#and merge the data back to the kids data
joindata$bmiz<-scale(joindata$bmi, center=T, scale=T)
joindata$agez<-scale(joindata$age, center=T, scale=T)


library(maptools)
library(spdep)
gpclibPermit()

uscos<-readShapePoly("/Users/ozd504/Google Drive/dem7263/data/co99_d00.shp")
uscos@data$cofips<-as.character(paste(uscos$STATE, uscos$COUNTY, sep=""))
uscos2<-unionSpatialPolygons(uscos, IDs = uscos@data$cofips)
uscos2<-as(uscos2, "SpatialPolygonsDataFrame")
uscos2@data$cofips<-as.character(row.names(uscos2))
uscos2$state<-substr(uscos2$cofips, 1,2)

#See what counties are in the brfss data
brfco<-unique(brfss_11$cofips)

cob<-uscos2[uscos2$cofips%in% brfco,]
cob$struct<-1:length(cob$cofips)
plot(cob)
nbs<-knearneigh(coordinates(cob), longlat = T, k = 4)
nbs<-knn2nb(nbs, row.names = cob$struct, sym = T)
wts<-nb2listw(nbs)
plot(cob)
plot(wts, coords=coordinates(cob), add=T, col=2)





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
