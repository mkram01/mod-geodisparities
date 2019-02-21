
m.est$est.obese<-fit_est$summary.fitted.values$mean
m.est.est<-tapply(m.est$est.obese, m.est$struct, mean, na.rm=T)
m.est.est<-data.frame(struct=names(unlist(m.est.est)), obeseest=unlist(m.est.est))
#m.est<-m.est[is.na(m.est$bmi)==T,]
msa.est<-merge(tx_ec, m.est.est, by.y="struct", by.x="struct", all.x=T, sort=F)
head(msa.est@data)

library(mapview)
clrs <- colorRampPalette(brewer.pal(8, "Blues"))
mapview(msa.est,"obeseest", col.regions=clrs, map.types="OpenStreetMap")


# Multilevel model in INLA
library(INLA)
indat<-indat[is.na(indat$struct)==F,]
fit_in1<- inla(obese~ race_eth+agec+f(struct, model="iid"),
               family = "binomial", Ntrials =1,
               data=indat, num.threads = 2)
summary(fit_in1)

1/fit_in1$summary.hyperpar
m<- inla.tmarginal(
  function(x) (1/x),
  fit_in1$marginals.hyperpar$`Precision for struct`)
inla.hpdmarginal(.95, marginal=m)

plot(m, type="l", main=c("Posterior distibution for between MSA variance", "- Random Intercept model -"))

# That is our individual level, random intercept model. Now I will fit a model that includes MSA level demographic
# characteristics, the Gini index, %black and %hispanic. This corresponds to a multi-level model with higher level predictors:
fit_in2<- inla(obese~ race_eth+agec+giniz+zpblack+zphisp+f(struct, model="iid"),
               data=indat,
               family="binomial", Ntrials = 1)
summary(fit_in2)
1/fit_in2$summary.hyperpar
m2<- inla.tmarginal(
  function(x) (1/x),
  fit_in2$marginals.hyperpar$`Precision for struct`)
inla.hpdmarginal(.95, marginal=m2)
plot(m2, type="l", main=c("Posterior distibution for between MSA variance", "- Multi-level model -"))

fit_in3<- inla(obese~ race_eth+agec+giniz+zpblack+zphisp+
                 f(struct, model="bym", graph=mat),
               family = "binomial", Ntrials = 1,
               data=indat,
               control.predictor = list(link=1))
summary(fit_in3)

m3_sp<- inla.tmarginal(
  function(x) (1/x),
  fit_in3$marginals.hyperpar$`Precision for struct (spatial component)`)
m3_iid<- inla.tmarginal(
  function(x) (1/x),
  fit_in3$marginals.hyperpar$`Precision for struct (iid component)`)
inla.hpdmarginal(.95, marginal=m3_sp)

inla.hpdmarginal(.95, marginal=m3_iid)
plot(m3_sp, type="l", main=c("Posterior distibution for between Spatial MSA variance", "- Multi-level mod
                             el -"), xlim=c(0, .015))
lines(m3_iid, col=2,lty=2)
legend("topright", legend=c("Spatial Variance", "IID Variance"), col=c(1,2), lty=c(1,2))



# Links --------------------------


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


# Faraway code for multilevel --------
# http://www.maths.bath.ac.uk/~jjf23/inla/multilevel.html

# In INLA, look at precision to see if priors need to be changed
data(jsp, package="faraway")
jspr <- jsp[jsp$year==2,]
mjspr <- data.frame(rbind(jspr[,1:6],jspr[,1:6]),subject=factor(rep(c("english","math"),c(953,953))),score=c(jspr$english/100,jspr$math/40))
mjspr$craven <- mjspr$raven-mean(mjspr$raven)

ggplot(mjspr, aes(x=raven, y=score))+geom_jitter(alpha=0.25)+facet_grid(gender ~ subject)

# Construct unique lables of nested factor levels of class and student
mjspr$school <- factor(mjspr$school)
mjspr$classch <- factor(paste(mjspr$school,mjspr$class,sep="."))
mjspr$classchid <- factor(paste(mjspr$school,mjspr$class,mjspr$id,sep="."))


# Default prior model
formula <- score ~ subject*gender+craven*subject+social + f(school, model="iid") + f(classch, model="iid") + f(classchid, model="iid")
result <- inla(formula, family="gaussian", data=mjspr)
result <- inla.hyperpar(result)
summary(result)

# Informative priors
# Define it so the mean value of gamma prior is set to the inverse of the 
# variance of the residuals of the fixed-effects only model. 
# We expect the error variances to be lower than this variance so this is an overestimate. 
# The variance of the gamma prior (for the precision) is controlled by the apar 
# shape parameter in the code.
# 
apar <- 0.5
lmod <- lm(math ~ social+craven, jspr)
bpar <- apar*var(residuals(lmod))
lgprior <- list(prec = list(prior="loggamma", param = c(apar,bpar)))
formula = math ~ social+craven+f(school, model="iid", hyper = lgprior)+f(classch, model="iid", hyper = lgprior)
result <- inla(formula, family="gaussian", data=jspr)
result <- inla.hyperpar(result)
summary

# Compute the transforms to an SD scale for the random effect terms. Make a table of summary statistics for the posteriors:
sigmaschool <- inla.tmarginal(function(x) 1/sqrt(exp(x)), result$internal.marginals.hyperpar[[2]])
sigmaclass <- inla.tmarginal(function(x) 1/sqrt(exp(x)), result$internal.marginals.hyperpar[[3]])
sigmaepsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[1]])
restab <- sapply(result$marginals.fixed, function(x) inla.zmarginal(x,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigmaschool,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigmaclass,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigmaepsilon,silent=TRUE))
colnames(restab) <- c(names(lmod$coef),"school","class","epsilon")
data.frame(restab)

# Plot SD Posteriors
ddf <- data.frame(rbind(sigmaschool,sigmaclass,sigmaepsilon),errterm=gl(3,1024,labels = c("school","class","epsilon")))
ggplot(ddf, aes(x,y, linetype=errterm))+geom_line()+xlab("math")+ylab("density")+xlim(0,7)
# Look for reasonability

# Penalized complexity prior
# This requires that we specify a scaling for the SDs of the random effects. We use the SD of the residuals of the fixed effects only model (what might be called the base model in the paper) to provide this scaling.
lmod <- lm(math ~ social+craven, jspr)
sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
formula = math ~ social+craven+f(school, model="iid", hyper = pcprior)+f(classch, model="iid", hyper = pcprior)
result <- inla(formula, family="gaussian", data=jspr)
result <- inla.hyperpar(result)
summary(result)

# Compute summaries as before
sigmaschool <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[2]])
sigmaclass <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[3]])
sigmaepsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[1]])
restab <- sapply(result$marginals.fixed, function(x) inla.zmarginal(x,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigmaschool,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigmaclass,silent=TRUE))
restab <- cbind(restab, inla.zmarginal(sigmaepsilon,silent=TRUE))
colnames(restab) <- c(names(lmod$coef),"school","class","epsilon")
data.frame(restab)

#Plot
ddf <- data.frame(rbind(sigmaschool,sigmaclass,sigmaepsilon),errterm=gl(3,1024,labels = c("school","class","epsilon")))
ggplot(ddf, aes(x,y, linetype=errterm))+geom_line()+xlab("math")+ylab("density")+xlim(0,7)
# look at posteriors

# Faraway code for multiple response multilevel --------
data(jsp, package="faraway")
jspr <- jsp[jsp$year==2,]
mjspr <- data.frame(rbind(jspr[,1:6],jspr[,1:6]),subject=factor(rep(c("english","math"),c(953,953))),score=c(jspr$english/100,jspr$math/40))
mjspr$craven <- mjspr$raven-mean(mjspr$raven)

ggplot(mjspr, aes(x=raven, y=score))+geom_jitter(alpha=0.25)+facet_grid(gender ~ subject)

# Default prior model
# Construct unique labels for nested factor levels of class and student:
mjspr$school <- factor(mjspr$school)
mjspr$classch <- factor(paste(mjspr$school,mjspr$class,sep="."))
mjspr$classchid <- factor(paste(mjspr$school,mjspr$class,mjspr$id,sep="."))

formula <- score ~ subject*gender+craven*subject+social + f(school, model="iid") + f(classch, model="iid") + f(classchid, model="iid")
result <- inla(formula, family="gaussian", data=mjspr)
result <- inla.hyperpar(result)
summary(result)
# Look at precision


# Informative gamma
# Define it so the mean value of gamma prior is set to the inverse of the variance of the residuals of the fixed-effects only model. We expect the error variances to be lower than this variance so this is an overestimate. The variance of the gamma prior (for the precision) is controlled by the apar parameter.
apar <- 0.5
lmod <- lm(score ~ subject*gender+craven*subject+social,mjspr)
bpar <- apar*var(residuals(lmod))
lgprior <- list(prec = list(prior="loggamma", param = c(apar,bpar)))
formula = score ~ subject*gender+craven*subject+social+f(school, model="iid", hyper = lgprior)+f(classch, model="iid", hyper = lgprior)+f(classchid, model="iid", hyper = lgprior)
result <- inla(formula, family="gaussian", data=mjspr)
result <- inla.hyperpar(result)
summary(result)

#Compute the transforms to an SD scale for the field and error. Make a table of summary statistics for the posteriors:
sigmaschool <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[2]])
sigmaclass <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[3]])
sigmaid <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[4]])
sigmaepsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[1]])
restab=sapply(result$marginals.fixed, function(x) inla.zmarginal(x,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaschool,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaclass,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaid,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaepsilon,silent=TRUE))
colnames(restab) = c(names(lmod$coef),"school","class","id","epsilon")
data.frame(restab)


#Also construct a plot of the SD posteriors:
ddf <- data.frame(rbind(sigmaschool,sigmaclass,sigmaid,sigmaepsilon),errterm=gl(4,1024,labels = c("school","class","id","epsilon")))
ggplot(ddf, aes(x,y, linetype=errterm))+geom_line()+xlab("score")+ylab("density")+xlim(0,0.15)

#Penalized Complexity Prior
# In Simpson et al (2015), penalized complexity priors are proposed. This requires that we specify a scaling for the SDs of the random effects. We use the SD of the residuals of the fixed effects only model (what might be called the base model in the paper) to provide this scaling.

lmod <- lm(score ~ subject*gender+craven*subject+social,mjspr)
sdres <- sd(residuals(lmod))
pcprior <- list(prec = list(prior="pc.prec", param = c(3*sdres,0.01)))
formula = score ~ subject*gender+craven*subject+social+f(school, model="iid", hyper = pcprior)+f(classch, model="iid", hyper = pcprior)+f(classchid, model="iid", hyper = pcprior)
result <- inla(formula, family="gaussian", data=mjspr)
result <- inla.hyperpar(result)
summary(result)


#Compute the summaries as before:
sigmaschool <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[2]])
sigmaclass <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[3]])
sigmaid <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[4]])
sigmaepsilon <- inla.tmarginal(function(x) 1/sqrt(exp(x)),result$internal.marginals.hyperpar[[1]])
restab=sapply(result$marginals.fixed, function(x) inla.zmarginal(x,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaschool,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaclass,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaid,silent=TRUE))
restab=cbind(restab, inla.zmarginal(sigmaepsilon,silent=TRUE))
colnames(restab) = c(names(lmod$coef),"school","class","id","epsilon")
data.frame(restab)


# Plots
ddf <- data.frame(rbind(sigmaschool,sigmaclass,sigmaid,sigmaepsilon),errterm=gl(4,1024,labels = c("school","class","id","epsilon")))
ggplot(ddf, aes(x,y, linetype=errterm))+geom_line()+xlab("score")+ylab("density")+xlim(0,0.15)

## CDC Webinar -----------------
#https://nces.ed.gov/fcsm/pdf/GIG_Workshop_2016_Mapping_Suicide_Death_Rates_NCHS.pdf




#read in shapefile and data
county.map <- readShapePoly('//path to shapefile here/shapefilename.shp',IDvar="NUMFIPS")
suic <- read.csv("//path to data here/datafilename.csv",header=TRUE)

#create spatial data frame
polys <- SpatialPolygonsDataFrame(county.map,data=as.data.frame(county.map),match.ID=TRUE)

#obtain lat long coordinates
coords <- coordinates(polys)
polys$x <- coords[,1]
polys$y <- coords[,2]

#create adjacency matrix, neighbors list - here using Delaunay Triangulation
triang <- tri2nb(coords, row.names=NULL)
neib <- nb2WB(triang)

#calculate sum of number of neighbors
neib$sumnb <- sum(neib$num)

#how many neighbors for each county?
summary(neib$num)

#set seed if you want to replicate results
set.seed(1234)

#create a file with the required info about what counties/units are neighbors
inla.geobugs2inla(neib$adj, neib$num, graph.file="suicides_map")
#create the model - here a binomial model for suicide deaths/population, including a
# random effect for county (iid), a spatially structured county-level
#random effect (besag), a random effect for time (type 1 random walk),
# and a county-year specific iid residual term
# 
countyid <- rep(1:3140,each=10) #number of counties
countyid2 <- countyid #number of counties for second random effect
resid <- rep(1:31400) #number of county-year observations
year <- rep(1:10,len=31400) #year variable
numerator <- suic$numerator
denominator <- suic$denominator
data <- data.frame(numerator, denominator, countyid, countyid2, resid, year)

# Evaluated each of the terms to see if better fit
formula7<-numerator ~ 1 + f(countyid, model="iid") +  # • County-level non-spatial random effect (iid)
  f(countyid2, model = "besag", graph = "suicides_map") + # • County-level spatially structured random effect
  f(year, model = "rw1")+ # • Year random effect (type 1 random walk)
  f(resid, model = "iid") # • Space-time interaction term (residual, iid)

result7 <- inla(formula7, 
                family = "Binomial", 
                Ntrials = denominator,
                data = data,
                control.compute = list(dic = TRUE,
                                     cpo = TRUE))

#get fit statistics
result7$dic$dic;result7$dic$p.eff

# 
# Model Components DIC n.eff
# 1.Simple random effects, 𝑣𝑖 𝛼0 + 𝑣𝑖 150371.4 2316
# 2.Spatial 𝑢𝑖 and non-Spatial 𝑣𝑖
# , random effects 𝛼0 +𝑢𝑖 +𝑣𝑖 149966.2 2316
# Random time effects
# 3. Correlated time effects, 𝜑1𝑡 𝛼0 + 𝑢𝑖 + 𝑣𝑖 + 𝜑1𝑡 148008.6 1884
# 4. Uncorrelated time effects, 𝜑2𝑡
# Full Model
# 𝛼0 + 𝑢𝑖 + 𝑣𝑖 + 𝜑2𝑡 148010.3 1886
# 5. Space time interaction term, 𝜓𝑖𝑡 𝛼0 +𝑢𝑖+𝑣𝑖 + 𝜑2𝑡+𝜓𝑖𝑡 147821.9 2766
# Full Model with Covariates
#  𝜑1𝑡+𝜓𝑖𝑡+𝑿𝒊𝒕′𝜷 147181.1 1896
