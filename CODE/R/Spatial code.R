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

# Corey Sparks -------------
# https://rpubs.com/corey_sparks/439277
# Spatial GLMM(s) using the INLA Approximation
# Data
files <- list.files("DATA/CoreySparks/nhgis_vs/", pattern = "*.csv", full.names = T)
vital <- lapply(files, read.csv, header = T)

df <- ldply(vital, data.frame)
df$cofips <- paste(substr(df$GISJOIN, 2,3), substr(df$GISJOIN, 5,7), sep = "")

df <- df %>%
  filter(YEAR %in% 2000:2007) %>%
  mutate(rate = as.numeric(AGWG001) ) %>% #Edited from AGWJ001
  select(YEAR, cofips, rate)
head(df)


# Census pop estimates
popurl <- url("http://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv")
pops <- read.csv(popurl)
names(pops) <- tolower(names(pops))
pops <- pops %>%
  mutate(cofips = paste(sprintf(fmt = "%02d", state), sprintf(fmt = "%03d",county), sep = "")) %>%
  filter(sumlev == 50, !state %in% c(2, 15))

head(pops)

# Data prep
# Reshape pop estimates to long
pops.long <- reshape(data = pops, idvar = "cofips", 
                     varying = list(names(pops)[9:16]), 
                   direction = "long", 
                   drop = names(pops)[c(2,3,4,5,6,8,17,18,19,20)], 
                   v.names = "population")
pops.long$year <- pops.long$time + 1999
head(pops.long)


# Merge pop estimates with data frame
dat.long <- merge(pops.long, df, by.x = c("cofips", "year"), 
                  by.y = c("cofips", "YEAR"))
head(dat.long)
# Output is organized by County by year

# Get 2000 census data
# Requires API key now
cov_dat <- get_decennial(geography = "county", year = 2000, sumfile = "sf3",
                       summary_var = "P001001",
                       variables = c("P007003", "P007004","P007010","P053001", "P089001", "P089002" ),
                       output = "wide")

cov_dat <- cov_dat %>%
  mutate(cofips = GEOID,
         pwhite = P007003/summary_value, 
         pblack = P007004/summary_value, 
         phisp = P007010/summary_value,
         medhhinc = as.numeric(scale(P053001)), 
         ppov = P089002/P089001)


final.dat < -merge(dat.long, cov_dat, by="cofips")
head(final.dat)

# Expected cases (raw counts of events relative to some expected value, or population offset)
rates <- aggregate(rate ~ 1, final.dat, mean) #in this case, we will standardize to the average IMR for the period

final.dat$E_d <- rates$rate

final.dat <- final.dat[order(final.dat$cofips, final.dat$year),]
final.dat$id <- 1:dim(final.dat)[1]

head(final.dat)
options(scipen = 999)

# Census polygons, drop non-contiguous
us_co <- counties(cb = T)
us_co <- us_co %> %
  subset(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))


# Create contiguity matrix
#In INLA, we don't need FIPS codes, we need a simple numeric index for our counties
us_co$struct <- 1:dim(us_co@data)[1]
nbs <- knearneigh(coordinates(us_co), k = 5, longlat = T) #k=5 nearest neighbors
nbs <- knn2nb(nbs, row.names = us_co$struct, sym = T) #force symmetry!!
mat <- nb2mat(nbs, style = "B",zero.policy = TRUE)
colnames(mat) <- rownames(mat) 
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])

nb2INLA("cl_graph",nbs)
am_adj <- paste(getwd(),"/cl_graph",sep = "")
H <- inla.read.graph(filename = "cl_graph")
image(inla.graph2matrix(H), xlab = "", ylab = "", main = "")


# Plot geographies
us_co <- st_as_sf(us_co)
us_co$cofips <- paste(us_co$STATEFP, us_co$COUNTYFP, sep = "")
us_co %>%
  ggplot() + geom_sf() + coord_sf(crs = 102008)
us_co

final.dat <- merge( us_co,final.dat, by = "cofips")
final.dat <- final.dat[order(final.dat$cofips, final.dat$year),]


# Model setup
# Options = binomial, poisson, negative binomial
# Distribution of IMR by year
ggplot(data = final.dat) + 
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

final.dat %>%
  filter(year %in% c(2000)) %>%
  mutate(qrr = cut(I(rate/E_d), breaks = quantile(I(rate/E_d), p = seq(0,1,length.out = 5)), 
                   include.lowest = T)) %>%
  ggplot() + geom_sf(aes(fill = qrr)) +
  scale_colour_brewer(palette = "RdBu" ) + 
  scale_fill_brewer(palette = "RdBu", na.value = "grey") + 
  guides(fill = guide_legend(title = "Relative Risk Quartile")) + 
  ggtitle(label = "Relative Risk Quartile - raw data, 2000") + 
  coord_sf(crs = 102008)


#Model specification:
f1 <- rate~scale(pblack) + scale(phisp) + scale(ppov) + year

#Model fit
mod1 <- inla(formula = f1,data = final.dat, #linear predictor - fixed effects
           family = "nbinomial", E = E_d,  #marginal distribution for the outcome, expected count
           control.compute = list(waic = T), # compute DIC or not?
           control.predictor = list(link = 1), #estimate predicted values & their marginals or not?
           num.threads = 3, 
           verbose = F)
#model summary
summary(mod1)

# Plot observed vs fitted values
plot(x = mod1$summary.fitted.values$mean, y = final.dat$rate/final.dat$E_d , 
     ylab = "Observed", xlab = "Estimated" )

# Basic county-level random intercept model
# heterogeneity in mortality rate for each county
f2<- rate ~ scale(pblack) + scale(phisp) + scale(ppov) + year + #fixed effects
  f(struct, model = "iid")  #random effects

mod2 <- inla(formula = f2,data = final.dat,
           family = "nbinomial", E = E_d, 
           control.compute = list(waic = T), 
           control.predictor = list(link = 1),
           num.threads = 3, 
           verbose = F)

#total model summary
summary(mod2)

# Marginal distributions of hyperparameters
m2 <- inla.tmarginal(
  function(x) (1/x), #invert the precision to be on variance scale
  mod2$marginals.hyperpar$`Precision for struct`)

#95% credible interval for the variance
inla.hpdmarginal(.95, marginal = m2)
plot(m2, type = "l", 
     main = c("Posterior distibution for between county variance", "- IID model -"), 
     xlim = c(0, .01))

final.dat$fitted_m2 <- mod2$summary.fitted.values$mean

final.dat %>%
  filter(year %in% c(2000)) %>%
  mutate(qrr = cut(fitted_m2, 
                 breaks = quantile(fitted_m2, p = seq(0, 1, length.out = 6)), 
                 include.lowest = T)) %>%
  ggplot() + 
  geom_sf(aes(fill = qrr)) + scale_colour_brewer(palette = "RdBu" ) + 
  scale_fill_brewer(palette = "RdBu", na.value = "grey") + 
  guides(fill = guide_legend(title = "Relative Risk Quartile")) +
  ggtitle(label = "Relative Risk Quartile - IID Model, 2000") +
  coord_sf(crs = 102008)
# library(mapview)
# 
# map1<-final.dat%>%
#   filter(year%in%c(2007))%>%
#   mutate(qrr=cut(fitted_m2, breaks = quantile(fitted_m2, p=seq(0,1,length.out = 8))))
# clrs <- colorRampPalette(brewer.pal(8, "RdBu"))
# mapView(as(map1, "Spatial"), zcol="qrr", legend=T, col.regions=clrs)

# BYM Model 
# Specify spatial connectivity matrix
#final.dat$year_c <- final.dat$year - 2004
f3 <- rate ~ scale(pblack) + scale(phisp) + scale(ppov) +
  f(struct, model = "bym", constr = T, scale.model = T, graph = H) +
  f(year, model="iid") #temporal random effect
mod3 <- inla(formula = f3,data = final.dat,
           family = "nbinomial", E = E_d, 
           control.compute = list(waic = T), 
           num.threads = 3, 
           verbose = F,
           control.predictor = list(link = 1))

#total model summary
summary(mod3)

plot(y = mod3$summary.random$year_c$mean,
     x = unique(final.dat$year), type = "l")


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

# 2000
final.dat %>%
  filter(year %in% c(2000)) %>%
  mutate(qrr = cut(fitted_m3, 
                   breaks = quantile(fitted_m3, p = seq(0, 1,length.out = 6)), 
                   include.lowest = T)) %>%
  ggplot() + 
  geom_sf(aes(fill = qrr)) + 
  scale_colour_brewer(palette = "RdBu" ) +
  scale_fill_brewer(palette = "RdBu", na.value = "grey") +
  guides(fill = guide_legend(title = "Relative Risk Quartile")) + 
  ggtitle(label = "Relative Risk Quartile - IID Model, 2000") +
  coord_sf(crs = 102008)

# 2007
final.dat %>%
  filter(year %in% c(2007)) %>%
  mutate(qrr = cut(fitted_m3, 
                   breaks = quantile(fitted_m3, p = seq(0, 1,length.out = 6)), 
                   include.lowest = T)) %>%
  ggplot() + 
  geom_sf(aes(fill = qrr)) + 
  scale_colour_brewer(palette = "RdBu" ) +
  scale_fill_brewer(palette = "RdBu", na.value = "grey") +
  guides(fill = guide_legend(title = "Relative Risk Quartile")) + 
  ggtitle(label = "Relative Risk Quartile - IID Model, 2000") +
  coord_sf(crs = 102008)



#map1 <- final.dat%>%
#  filter(year%in%c(2007)) %>%
#  mutate(qrr=cut(fitted_m3, 
#  breaks = quantile(fitted_m3, p = seq(0,1,length.out = 8))))
#clrs <- colorRampPalette(brewer.pal(8, "RdBu"))
#mapView(as(map1, "Spatial"), zcol = "qrr", legend = T, col.regions = clrs)


# Map BYM model to look for spatial trends
us_co$sp_re<-mod3$summary.random$struct$mean[1:3108]
us_co%>%
  mutate(qse=cut(sp_re, breaks = quantile(sp_re, p=seq(0,1,length.out = 6)), include.lowest = T))%>%
  ggplot()+geom_sf(aes(fill=qse))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdBu", na.value="grey")+guides(fill=guide_legend(title="Spatial Excess Risk"))+ggtitle(label="Spatial Random Effect - BYM Model")+coord_sf(crs = 102008)




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

#map1 < -final.dat%>%
# filter(year %in% c(2007))%>%
#  mutate(qrr = cut(exceedprob, 
#  breaks = c(0, .5, .9, .95, .99, 1), 
#  include.lowest = T))
#clrs <- colorRampPalette(brewer.pal(6, "Blues"))
#mapView(as(map1, "Spatial"), zcol = "qrr", legend = T, col.regions = clrs, 
#map.types = "OpenStreetMap")


# Corey Sparks v2 -----------------------
# https://rpubs.com/corey_sparks/132760
# Bayesian Multi-level regression model using INLA
library(INLA)
library(car)

#load brfss
brfss_11 <- read.csv("DATA/CoreySparks/brfss_11.csv")
#load("DATA/brfss_11.Rdata")
nams <- names(brfss_11)
newnames <- gsub("_", "", nams)
names(brfss_11) <- tolower(newnames)

brfss_11$statefip <- sprintf("%02d", brfss_11$state )
brfss_11$cofip <- sprintf("%03d", brfss_11$cnty )
brfss_11$cofips <- paste(brfss_11$statefip, brfss_11$cofip, sep = "")

#bmi
brfss_11$bmi <- ifelse(is.na(brfss_11$bmi5) == T, NA, brfss_11$bmi5/100)
#poor or fair health
brfss_11$badhealth <- ifelse(brfss_11$genhlth %in% c(4,5),1,0)

#race
brfss_11$black <- car::recode(brfss_11$racegr2, 
                         recodes = "2=1; 9=NA; else=0", 
                         as.factor = T)
brfss_11$white <- car::recode(brfss_11$racegr2, 
                         recodes = "1=1; 9=NA; else=0", 
                         as.factor = T)
brfss_11$other <- car::recode(brfss_11$racegr2, 
                         recodes = "3:4=1; 9=NA; else=0", 
                         as.factor = T)
brfss_11$hispanic <- car::recode(brfss_11$racegr2, 
                            recodes = "5=1; 9=NA; else=0", 
                            as.factor = T)

#education level
brfss_11$lths <- recode(brfss_11$educa, 
                        recodes = "1:3=1;9=NA; else=0", 
                        as.factor = F)
brfss_11$coll <- recode(brfss_11$educa, 
                        recodes = "5:6=1;9=NA; else=0", 
                        as.factor = F)

#employment
brfss_11$employ <- recode(brfss_11$employ, 
                          recodes = "1:2='Employed'; 2:6='nilf'; 7='retired'; 8='unable'; else=NA", 
                          as.factor = T)
brfss_11$employ <- relevel(brfss_11$employ, 
                           ref = 'Employed')

#marital status
brfss_11$marst <- recode(brfss_11$marital, 
                         recodes = "1='married'; 2='divorced'; 3='widowed'; 4='separated'; 5='nm';6='cohab'; else=NA", 
                         as.factor = T)
brfss_11$marst <- relevel(brfss_11$marst, 
                          ref = 'married')

#income
brfss_11$inc <- as.factor(ifelse(brfss_11$incomg == 9, NA, brfss_11$incomg))

#Age cut into intervals
brfss_11$agec <- cut(brfss_11$age, breaks = c(0, 24, 39, 59, 79, 99))

# higher-level predictors - ACS - individual
#I will also add in some Census variables from the ACS 2010 5 Year estimates
#load in ACS data from Factfinder
acsecon <- read.csv("DATA/CoreySparks/ACS_10_SF4_DP03_with_ann.csv")
acsecon$povrate <- acsecon[, "HC03_VC156"]
acsecon$unemployed <- acsecon[, "HC03_VC13"]
acsecon$cofips <- substr(acsecon$GEO.id, 10, 14)
acsecon$povz <- scale(as.numeric(acsecon$povrate), center = T, scale = T)
acsecon$unempz <- scale(as.numeric(acsecon$unemployed), center = T, scale = T)
acsecon <- acsecon[, c("cofips", "povrate","povz", "unemployed","unempz")]

head(acsecon)

# join data
joindata <- merge(brfss_11, acsecon, by = "cofips", all.x = T, all.y = F)

#and merge the data back to the kids data
joindata$bmiz <- scale(joindata$bmi, center = T, scale = T)
joindata$agez <- scale(joindata$age, center = T, scale = T)

#This next part of the code gets only the counties from the shapefile that match counties in the BRFSS.
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

nb2INLA(file="/Users/ozd504/Google Drive/dem7263/data/us.gra", nbs)

mdat2<-cob@data

indat<-merge(joindata, mdat2, by.x="cofips", by.y="cofips", all.x=T)
indat<-indat[order(indat$struct),]
indat2<-indat[complete.cases(indat[,c("bmi", "badhealth", "black", "educa", "agez")] ),]
length(unique(indat2$cofips))

# Fit null model
fit0<-inla(bmiz~1+f(struct, model="iid"), family = "gaussian", data=indat2, num.threads = 2, verbose = F,  control.inla=list(strategy='gaussian', int.strategy='eb'), control.compute = list( dic=T) )
summary(fit0)
1/fit0$summary.hyperpar$mean
m<- inla.tmarginal(
  function(x) 1/x,
  fit0$marginals.hyperpar$'Precision for struct')

plot(m, type="l", main=c("Posterior distibution for between county variance", "- Null Model -"))

# Now I will fit a model that includes individual level demographic characteristics:
fit1<-inla(bmiz~black+hispanic+other+lths+coll+ agez+f(struct, model="iid"), family = "gaussian", data=indat2, num.threads = 2, verbose = F, control.inla=list(strategy='gaussian', int.strategy='eb'), control.compute = list(dic=T ))

1/fit1$summary.hyperpar$mean

m1<- inla.tmarginal(
  function(x) 1/x,
  fit1$marginals.hyperpar$'Precision for struct')

plot(m1, type="l", main=c("Posterior distibution for between county variance", "- Random Intecept Model -"))

#Next a model with a county level predictor, the z-scored poverty rate

fit2<-inla(bmiz~black+hispanic+other+lths+coll+ agez+povz+f(struct, model="iid"), family = "gaussian", data=indat2, num.threads = 2, verbose = F, control.inla=list(strategy='gaussian', int.strategy='eb'), control.compute = list(dic=T ))

summary(fit2)

1/fit2$summary.hyperpar$mean

m1<- inla.tmarginal(
  function(x) 1/x,
  fit1$marginals.hyperpar$'Precision for struct')

plot(m1, type="l", main=c("Posterior distibution for between county variance", "- Multi-Level Model -"))


#Finally, we model the county level means using the Besag-York and Mollie model that we had used on the areal data last week.

fit3<-inla(bmiz~black+hispanic+other+lths+coll+ agez+povz+f(struct, model="bym", graph="/Users/ozd504/Google Drive/dem7263/data/us.gra",hyper = list(prec.unstruct=list(initial=5.3), prec.spatial=list(initial=5.3))), family = "gaussian", data=indat2, num.threads = 1, verbose = F, control.inla=list(strategy='gaussian', int.strategy='eb'), control.compute = list(dic=T), control.family = list(hyper = list(prec=list(initial= .028))))
## Warning in INLA::f(struct, model = "bym", graph = "/Users/ozd504/Google
## Drive/dem7263/data/us.gra", : The graph for the model bym has 3 connected
## components!!! Model is revised accordingly.
## 

1/fit3$summary.hyperpar$mean
m3<- inla.tmarginal(
  function(x) 1/x,
  fit3$marginals.hyperpar$`Precision for struct (iid component)`)
m32<- inla.tmarginal(
  function(x) 1/x,
  fit3$marginals.hyperpar$`Precision for struct (spatial component)`)

plot(m3, type="l", main=c("Posterior distibution for between county variance", "- Multi-level BYM Model IID-")) 
summary(fit3)

plot(m32, type="l", main=c("Posterior distibution for between county variance", "- Multi-level BYM Model Spatial-"))


# Corey Sparks v3 --------------------------------------------------------------
# https://txrdc.tamu.edu/wp-content/uploads/sites/20/2018/03/TAMU_May14_Bayes_workshop.pdf
# Bayesian spatial models using INLA approximation
files<-list.files("~/Google Drive/a&m_stuff/workshop_5_14_18/vita_stat/", pattern = "*.csv", full.names =
                    T)
vital<-lapply(files, read.csv, header=T)

df <- ldply(vital, data.frame)
df$cofips<-paste(substr(df$GISJOIN, 2,3), substr(df$GISJOIN, 5,7), sep="")
df<-df%>%
  filter(YEAR %in%2000:2007, STATEA==480)%>%
  mutate(births=AGWE001, deaths=AGWG001)%>%
  select(YEAR, cofips, births, deaths)
head(df)

popurl<-url("http://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est0
0int-tot.csv")
pops<-read.csv(popurl)
names(pops)<-tolower(names(pops))
pops<-pops%>%
  mutate(cofips = paste(sprintf(fmt = "%02d", state), sprintf(fmt = "%03d",county), sep=""))%>%
  filter(sumlev==50, state==48)
pops$struct<-1:dim(pops)[1]
pops.long<-reshape(data = pops, idvar = "cofips", varying = list(names(pops)[9:16]), direction="long", drop = names(pops)[c(2,3,4,5,6,8,17,18,19,20)], v.names = "population")
pops.long$year<-pop


dat.long<-merge(pops.long, df, by.x=c("cofips", "year"), by.y=c("cofips", "YEAR"))
#v00<-load_variables(year=2000, dataset = "sf3", cache = T)
cov_dat<-get_decennial(geography = "county", state = "TX", year = 2000, sumfile = "sf3",
                       summary_var = "P001001",
                       variables = c("P007003", "P007004","P007010","P053001", "P089001", "P089002" ),
                       output = "wide")
## Getting data from the 2000 decennial Census
cov_dat<-cov_dat%>%
  mutate(cofips=GEOID,pwhite=P007003/summary_value, pblack=P007004/summary_value, phisp=P007010/summary_v
         alue,medhhinc=as.numeric(scale(P053001)), ppov=P089002/P089001)
final.dat<-merge(dat.long, cov_dat, by="cofips")
rates<-aggregate(cbind(deaths, births,population)~1, final.dat,sum)
rates$dr<-rates$deaths/rates$population
rates$br<-rates$births/rates$population
final.dat$E_d<-final.dat$population*rates$dr
final.dat$E_b<-final.dat$population*rates$br
final.dat<-final.dat[order(final.dat$cofips, final.dat$year),]
final.dat$id<-1:dim(final.dat)[1]
head(final.dat)

#Next we make the spatial information, we get the polygons from census directly using counties from the tigris package.
us_co<-counties(state = "TX", cb = T)
#In INLA, we don't need FIPS codes, we need a simple numeric index for our counties
us_co$struct<-1:dim(us_co@data[1])

nbs<-poly2nb(us_co, queen = T, row.names = us_co$struct)
mat <- nb2mat(nbs, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat)
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
nb2INLA("am_graph",nbs)
am_adj <-paste(getwd(),"/am_graph",sep="")
H<-inla.read.graph(filename="am_graph")
image(inla.graph2matrix(H), xlab="", ylab="", main="")

tx<-st_as_sf(us_co)
tx%>%
  ggplot()+geom_sf()

final.dat<-merge( tx,final.dat, by="struct")


ggplot(data = final.dat)+geom_histogram(aes(x =deaths , y=0.5*..density..))+facet_wrap(~year)+
  ggtitle(label = "Distribution of Deaths by Year", subtitle = "Texas Counties, 2000-2007")

ggplot(data = final.dat)+geom_histogram(aes(x =deaths/E_d , y=0.5*..density..))+facet_wrap(~year)+
  ggtitle(label = "Distribution of Mortality Relative Risk by Year", subtitle = "Texas Counties, 2000-200
          7")

#Model specification:
f1<-deaths~scale(pblack)+scale(phisp)+scale(ppov)+year
#Model fit
mod1<-inla(formula = f1,data = final.dat, #linear predictor - fixed effects
           family = "nbinomial", E = E_d, #marginal distribution for the outcome, expected count
           control.compute = list(dic=T), # compute DIC or not?
           control.predictor = list(link=1)) #estimate predicted values & their marginals or not?
#model summary
summary(mod1)


# Plot our observed vs fitted values
plot(x= mod1$summary.fitted.values$mean, y=final.dat$deaths/final.dat$E_d , ylab="Observed", xlab="Estima
     ted" )

#Now we add basic nesting of rates within counties, with a random intercept term for each county. This would allow there to be
#heterogenity in the mortality rate for each county, over and above each county’s observed characteristics.
#This model would be:
  #where here is the precision, not the variance and precision = 1/variance. INLA puts a log-gamma prior on the the
#precision by default.
f2<-deaths~scale(pblack)+scale(phisp)+scale(ppov)+year+ #fixed effects
  f(struct, model = "iid") #random effects
mod2<-inla(formula = f2,data = final.dat,
           family = "nbinomial", E = E_d,
           control.compute = list(dic=T),
           control.predictor = list(link=1))
#total model summary
summary(mod2)

# plot the posterior marginal of the hyperparameter in this model, in thiscase 

m2<- inla.tmarginal(
  function(x) (1/x), #invert the precision to be on variance scale
  mod2$marginals.hyperpar$`Precision for struct`)
inla.hpdmarginal(.95, marginal=m2)

plot(m2, type="l", main=c("Posterior distibution for between county variance", "- IID model -"), xlim=c(0
                                                                                                        , .1))
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


#For the BYM model we must specify the spatial connectivity matrix in the random effect.
#final.dat$year_c<-final.dat$year - 2004
f3<-deaths~scale(pblack)+scale(phisp)+scale(ppov)+
  f(struct, model = "bym", constr = T, scale.model = T, graph = mat)+
  f(year, model="iid") #temporal random effect
mod3<-inla(formula = f3,data = final.dat,
           family = "nbinomial", E = E_d,
           control.compute = list(dic=T),
           control.predictor = list(link=1))
#total model summary
summary(mod3)


plot(y=mod3$summary.random$year_c$mean,x=unique(final.dat$year), type="l")

m3a<- inla.tmarginal(
  function(x) (1/x),
  mod3$marginals.hyperpar$`Precision for struct (iid component)`)
m3b<- inla.tmarginal(
  function(x) (1/x),
  mod3$marginals.hyperpar$`Precision for struct (spatial component)`)
m3c<- inla.tmarginal(
  function(x) (1/x),
  mod3$marginals.hyperpar$`Precision for year`)
plot(m3a, type="l", main=c("Posterior distibution for between county variance", "- IID model -"), xlim=c(
  0, .1), ylim=c(0,300))
lines(m3b, col="red")
lines(m3c, col="green")


inla.hpdmarginal(.95,m3a)
inla.hpdmarginal(.95,m3b)
inla.hpdmarginal(.95,m3c)
#This indicates very low spatially correlated variance in these data and very low temporal heterogenity as well.


final.dat$fitted_m3<-mod3$summary.fitted.values$mean
final.dat%>%
  filter(year%in%c(2000))%>%
  mutate(qrr=cut(fitted_m3, breaks = quantile(fitted_m3, p=seq(0,1,length.out = 8))))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdB
                                                                                           u", na.value="grey")+guides(fill=guide_legend(title="Relative Risk Quartile"))+ggtitle(label="Relative Ri
                                                                                                                                                                                  sk Quartile - IID Model, 2000")+coord_sf(crs = 102008)

final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(fitted_m3, breaks = quantile(fitted_m3, p=seq(0,1,length.out = 8))))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdB
                                                                                           u", na.value="grey")+guides(fill=guide_legend(title="Relative Risk Quartile"))+ggtitle(label="Relative Ri
                                                                                                                                                                                  sk Quartile - IID Model, 2007")+coord_sf(crs = 102008)

library(mapview)
map1<-final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(fitted_m3, breaks = quantile(fitted_m3, p=seq(0,1,length.out = 8))))
clrs <- colorRampPalette(brewer.pal(8, "RdBu"))
mapView(as(map1, "Spatial"), zcol="qrr", legend=T, col.regions=clrs, map.types="OpenStreetMap")


#Map of spatial random effects
#It is common to map the random effects from the BYM model to look for spatial trends, in this case, there are not strong spatial signals:
  tx$sp_re<-mod3$summary.random$struct$mean[1:254]
tx%>%
  mutate(qse=cut(sp_re, breaks = quantile(sp_re, p=seq(0,1,length.out = 8))))%>%
  ggplot()+geom_sf(aes(fill=qse))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdB
                                                                                           u", na.value="grey")+guides(fill=guide_legend(title="Spatial Excess Risk"))+ggtitle(label="Spatial Random
                                                                                                                                                                               Effect - BYM Model")+coord_sf(crs = 102008)

#Exceedence probabilities
# In Bayesian spatial models that are centered on an epidemiological type of outcome, it is common to examine the data for
# spatial clustering. One way to do this is to examine the clustering in the relative risk from one of these GLMM models. For
# instance if is the relative risk
# from one of our Negative binomial models above. We can use the posterior marginals of the relative risk to ask
# where is a specific level of excess risk, say 50% extra or . If the density, or is high, then there is
# evidence that the excess risk is not only high, but significantly high.

thetastar<-1.5#theta*
inlaprob<- unlist(lapply(mod3$marginals.fitted.values, function(X){
  1-inla.pmarginal(thetastar, X)
}))
hist(inlaprob)

# So, we see lots of occasions where the exceedence probability is greater than .9. We can visualize these in a map.
final.dat$exceedprob<-inlaprob
final.dat%>%
  filter(year%in%c(2000))%>%
  mutate(qrr=cut(exceedprob, breaks = c(0, .5, .9, .95, .99, 1), include.lowest = T))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "Blues" )+scale_fill_brewer(palette = "Bl
                                                                                            ues", na.value="grey")+guides(fill=guide_legend(title=""))+ggtitle(label=expression(paste("Exceedence Pro
                                                                                                                                                                                      bability Relative Risk ","Pr( ",theta," >1.5"," ) - 2000") ))+coord_sf(crs = 102008)

final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(exceedprob, breaks = c(0, .5, .9, .95, .99, 1), include.lowest = T))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "Blues" )+scale_fill_brewer(palette = "Bl
                                                                                            ues", na.value="grey")+guides(fill=guide_legend(title="Relative Risk Quartile"))+ggtitle(label=expression
                                                                                                                                                                                     (paste("Exceedence Probability Relative Risk ","Pr( ",theta," >1.5"," ) - 2007") ))+coord_sf(crs = 10200
                                                                                                                                                                                                                                                                                  8)

library(mapview)
map1<-final.dat%>%
  filter(year%in%c(2007))%>%
  mutate(qrr=cut(exceedprob, breaks = c(0, .5, .9, .95, .99, 1), include.lowest = T))

clrs <- colorRampPalette(brewer.pal(6, "Blues"))
mapView(as(map1, "Spatial"), zcol="qrr", legend=T, col.regions=clrs, map.types="OpenStreetMap")


#Corey Sparks v4 -multilevel--------
#load brfss
brfss_14 <- load("DATA/CoreySparks/brfss_14.Rdata")
nams <- names(brfss_14)

#we see some names are lower case, some are upper and some have a little _ in the first position. This isa nightmare.
newnames<-gsub(pattern = "x_",replacement = "",x = nams)
names(brfss_14)<-tolower(newnames)
#BMI
brfss_14$bmi<-ifelse(is.na(brfss_14$bmi5)==T, NA, brfss_14$bmi5/100)
brfss_14$obese<-ifelse(brfss_14$bmi>30,1,0)
#Poor or fair self rated health
#brfss_14$badhealth<-ifelse(brfss_14$genhlth %in% c(4,5),1,0)
brfss_14$badhealth<-recode(brfss_14$genhlth, recodes="4:5=1; 1:3=0; else=NA")
#race/ethnicity
brfss_14$black<-recode(brfss_14$racegr3, recodes="2=1; 9=NA; else=0")
brfss_14$white<-recode(brfss_14$racegr3, recodes="1=1; 9=NA; else=0")
brfss_14$other<-recode(brfss_14$racegr3, recodes="3:4=1; 9=NA; else=0")
brfss_14$hispanic<-recode(brfss_14$racegr3, recodes="5=1; 9=NA; else=0")
brfss_14$race_eth<-recode(brfss_14$racegr3, recodes="1='nhwhite'; 2='nh black'; 3='nh other';
                          4='nh multirace'; 5='hispanic'; else=NA", as.factor = T)
brfss_14$race_eth<-relevel(brfss_14$race_eth, ref = "nhwhite")
#insurance
brfss_14$ins<-ifelse(brfss_14$hlthpln1==1,1,0)
#income grouping
brfss_14$inc<-ifelse(brfss_14$incomg==9, NA, brfss_14$incomg)
#education level
brfss_14$educ<-recode(brfss_14$educa, recodes="1:2='0Prim'; 3='1somehs'; 4='2hsgrad';
 5='3somecol'; 6='4colgrad';9=NA", as.factor=T)
#brfss_14$educ<-relevel(brfss_14$educ, ref='0Prim')
#employment
brfss_14$employ<-recode(brfss_14$employ, recodes="1:2='Employed'; 2:6='nilf';
 7='retired'; 8='unable'; else=NA", as.factor=T)
brfss_14$employ<-relevel(brfss_14$employ, ref='Employed')
#marital status
brfss_14$marst<-recode(brfss_14$marital, recodes="1='married'; 2='divorced'; 3='widowed';
 4='separated'; 5='nm';6='cohab'; else=NA", as.factor=T)
brfss_14$marst<-relevel(brfss_14$marst, ref='married')
#Age cut into intervals
brfss_14$agec<-cut(brfss_14$age80, breaks=c(0,24,39,59,79,99), include.lowest = T)

#I want to see how many people we have in each MSA in the data:
  
#Now we will begin fitting the multilevel regression model with the msa
#that the person lives in being the higher level
  head(data.frame(name=table(brfss_14$mmsaname),id=unique(brfss_14$mmsa)))
#How many total MSAs are in the data?
length(table(brfss_14$mmsa))


# Higher-level predictors
library(acs)
#Get 2010 ACS median household incomes for tracts in Texas
msaacs<-geo.make(msa="*")
acsecon<-acs.fetch(key=mykey, endyear=2010, span=5, geography=msaacs, variable = c("B19083_001","B17001_0
                                                                                   01","B17001_002", "B03002_001","B03002_004", "B03002_012" ))
colnames(acsecon@estimate)

msaecon<-data.frame(gini=acsecon@estimate[, "B19083_001"],
                    ppoverty=acsecon@estimate[, "B17001_002"]/acsecon@estimate[, "B17001_001"],
                    pblack=acsecon@estimate[,"B03002_004"]/acsecon@estimate[, "B03002_001"],
                    phisp=acsecon@estimate[,"B03002_012"]/acsecon@estimate[, "B03002_001"],
                    giniz=scale(acsecon@estimate[, "B19083_001"]),
                    ppovertyz=scale(acsecon@estimate[, "B17001_002"]/acsecon@estimate[, "B17001_001"]))
msaecon$zpblack<-scale(msaecon$pblack)
msaecon$zphisp<-scale(msaecon$phisp)
msaecon$ids<-paste(acsecon@geography$metropolitanstatisticalareamicropolitanstatisticalarea)

Let’s see the geographic variation in these economic indicators:
  library(tigris)
msa<-core_based_statistical_areas(cb=T)
msa_ec<-geo_join(msa, msaecon, "CBSAFP", "ids", how="inner")
tx_ec<-msa_ec[grep(msa_ec$NAME, pattern = "TX"), ]
library(RColorBrewer)
library(sp)
spplot(tx_ec, "gini", at=quantile(tx_ec$gini), col.regions=brewer.pal(n=6, "Reds"), col="transparent", ma
       in="Gini Coefficient")

spplot(tx_ec, "phisp", at=quantile(tx_ec$phisp), col.regions=brewer.pal(n=6, "Reds"), col="transparent",
       main="Percent Hispanic")

#Create spatial information for higher level units
tx_ec$struct<-1:dim(tx_ec)[1]
city.est.dat<-tx_ec@data[,c( "giniz","ppovertyz", "zpblack", "zphisp", "struct")]
city.est.dat$obese<-NA
head(city.est.dat)

brfss_14$cbsa<-as.character(brfss_14$mmsa)
indat<-merge(brfss_14, tx_ec, by.x="cbsa", by.y="CBSAFP", all.x=T)
brf.est<-indat[, c("giniz","ppovertyz", "zpblack", "zphisp", "struct", "obese")]
brf.est<-brf.est[order(brf.est$struct),]
head(brf.est)

##Here is where I add the cities that need to be estimated to the rest of the data
m.est<-rbind(city.est.dat, brf.est)
struct.in<-unique(brf.est$struct)
m.est$comp<-ifelse(m.est$struct%in%struct.in ,1,0)
m.est$rm<-ifelse(m.est$comp==1&is.na(m.est$obese)==T,1,0)
m.est<-m.est[-which(m.est$rm==1),]
m.est<-m.est[is.na(m.est$struct)==F,]
m.est<-m.est[order(m.est$struct),]
#
# fake_dat<-expand.grid(race_eth=levels(brfss_14$race_eth), agec=levels(brfss_14$agec), CBSAFP=levels(as.
factor(tx_ec$CBSAFP) ))
# fake_dat<-merge(fake_dat, tx_ec, by="CBSAFP")
library(spdep)
nbs<-knearneigh(coordinates(tx_ec), longlat = T, k = 4)
nbs<-knn2nb(nbs, row.names = tx_ec$struct, sym = T)
mat <- nb2mat(nbs, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat)
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])


fit_est<- inla(obese~ giniz+zpblack+zphisp+
                 f(struct, model="bym", graph=mat,constr=TRUE, scale.model=TRUE),
               family = "binomial",Ntrials = 1,
               data=m.est, num.threads = 2,
               control.predictor = list(link=1))
# control.inla=list(strategy='gaussian'))
#
summary(fit_est)

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


# All interpretation of parameters is done on a log scale, so
# exp(β)=% change in the mean
# round(exp(coef(usfitbin)), 3)

# Links --------------------------
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

#### Ian -----------
# Convert to spatial df --------------------------------------------------------

# County Shapefiles (Based on 2010 County Boundaries)
tigris_spdf <- tigris::counties()

# fn <- list.files(pattern = "DATA/nchs_births/R/Data/*.rda")
# for (i in fn) {
#   load(i)
# }

model1singleyear <- readRDS(file = "DATA/nchs_births/R/data/model1singleyear.rda")

# load your health data
health_df <- model1singleyear # with a "FIPS" column and "Case_Counts" column

# Merge case data with county shapefile
spdf <- tigris::geo_join(tigris_spdf, health_df, "GEOID", "combfips")
tigris_spdf <- NULL
spdf$births[is.na(spdf$births)] <- 0 # Set NA counties to 0 
spdf$ptb[is.na(spdf$ptb)] <- 0 # Set NA counties to 0
spdf$lptb[is.na(spdf$lptb)] <- 0 # Set NA counties to 0
spdf$mptb[is.na(spdf$mptb)] <- 0 # Set NA counties to 0
spdf$vptb[is.na(spdf$vptb)] <- 0 # Set NA counties to 0

# Albers Coordinate Reference System for United States
cref1 <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +7_0=0 +a=6570997 +b=6370997 +units=m +no_defs"
spdf <- sp::spTransform(spdf, sp::CRS(cref1)) # transform US counties to albers CRS

# Create adjacency matrix
spdf_adj <- spdep::poly2nb(spdf, queen = T) # set queen = F for rook
adj <- spdep::nb2mat(spdf_adj, style = "B", # create binary matrix
                     zero.policy=TRUE)  # added to avoid error
adj <- as(adj, "dgTMatrix") # create sparse matrix, memory conservation

# Create ID variable (if not already in SPDF)
spdf$idx <- 1:nrow(spdf)

# Extract data.frame from the SDPF
dat_df <- as.data.frame(spdf) 



# Ian ------------------------------------------
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










###################################
##### Zero-inflated Poisson 0 ##### 
###################################

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

###################################
##### Zero-inflated Poisson 1 ##### 
###################################

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

###################################
##### Hyperparameter Controls #####
###################################

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


##################
##### EXPORT #####
##################

# Export
save(m_null_pois, m_null_zip0, m_null_zip1, m_null_control,
     file = "inla_bym.Rdata"
)

############################
###### INTERPRETATION ######
############################

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

##############################
##### DATA VISUALIZATION #####
##############################

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



