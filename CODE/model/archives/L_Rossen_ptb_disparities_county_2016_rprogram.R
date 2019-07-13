rm(list=ls())

library(pixmap)
library(sp)
library(INLA)
library(yaml)
library(boot)
library(glmnet)
library(beanplot)
library(rgeos)
library(svDialogs)
library(rgdal)
library(sf)
library(readxl)
library(maps)
library(Rcpp)
library(dplyr)
library(ggplot2)
library(CARBayes)
library(RColorBrewer)
library(Hmisc)
library(spdep)
library(deldir)
library(tidyverse)

library(maptools)
library(CARBayes)
source("//cdc.gov//private//A728//vyz5//pit.r")

#https://www.math.uzh.ch/fileadmin/math/events/bilgm11/inla-tutorial-2.pdf
#http://www.bioss.ac.uk/rsse/2013/September2013slides-Illian.pdf
#https://rpubs.com/corey_sparks/132760
################################################################
################################################################
outputdir<-"//***//output_ptb_county//"

setwd("//***//")
imr<-read_excel("imrptbHP2020_2016_us.xlsx",sheet=1)

countycodes<-read_excel("//***//nchsurcodes2013.xlsx",sheet="Data")

imrdat<-merge(countycodes,imr,by="countyres",all=T)
imrdat<-imrdat[is.na(imrdat$FIPS)==F,]

#no white births for 15005 (kalawoo), 48301 (loving, TX), 51560 (clifton forge city, VA)

#imrdat<-imrdat[!imrdat$FIPS %in% c("15005","48301","51560"),]

#using TIGER/LINE file from 2010 as base map, North America Albers Equal Area projection
#AK 02201, 02232 and 02280 not in shapefile, but in data file, 

imrdat$FIPS[imrdat$FIPS=="02198"]<-"02201"
imrdat$FIPS[imrdat$FIPS=="02105"]<-"02232"
imrdat$FIPS[imrdat$FIPS=="02230"]<-"02232"
imrdat$FIPS[imrdat$FIPS=="02280"]<-"02195"
imrdat$FIPS[imrdat$FIPS=="02158"]<-"02270"

#independent cities in VA - not reported correctly or inconsistently
imrdat$FIPS[imrdat$FIPS=="51595"]<-"51081"
imrdat$FIPS[imrdat$FIPS=="51520"]<-"51191"
imrdat$FIPS[imrdat$FIPS=="51530"]<-"51163"
imrdat$FIPS[imrdat$FIPS=="51580"]<-"51081"
imrdat$FIPS[imrdat$FIPS=="51678"]<-"51163"


#county.map$id[county.map$id==48301]<-48495  #loving tx combine with nearest county - Winkler ???


imr1315<-imrdat %>%
  #select(ST_ABBREV,FIPS,totbldth,totblbrth,totwhdth,totwhbrth,year) %>%
  filter(year>=2014) %>%
  mutate(state=ST_ABBREV)  %>%
  group_by(FIPS, state) %>%
  summarise(blnum=sum(ptbbl,na.rm=T),
            blden=sum(totblbrthnomis,na.rm=T),
            whnum=sum(ptbwh,na.rm=T),
            whden=sum(totwhbrthnomis,na.rm=T),
            totalbirths=sum(totbrthsnomis)) 
  
#percent of births that are black, as covariate?
#need to go back and recalculate total #, not just white and black
pctbl<- imr1315 %>% 
  group_by(FIPS) %>%
  mutate(pctbl=100*(blden/(totalbirths)))


dataimr<-merge(imr1315,pctbl)

#exceedance probs in inla https://rpubs.com/corey_sparks/127161
 

# 2015 shapefile - missing 46113 and 51515
# county.map15 = read_sf('NA_AlbersEqArea_3142_2015.shp')
# names(county.map15)
# county.map15$FIPS<-paste(county.map15$STATEFP,county.map15$COUNTYFP,sep="")
# co<-as.data.frame(county.map15$FIPS)
# names(co)<-c("FIPS")
# co$inshp<-1
# mg<-merge(pctbl,co,by="FIPS",all=T)
# 

county.map = read_sf('NA_AlbersEqArea_3143.shp')
names(county.map)
county.map$FIPS<-paste(county.map$STATE,county.map$COUNTY,sep="")

co<-as.data.frame(county.map$FIPS)
names(co)<-c("FIPS")
co$inshp<-1

###getting # of FIPS that don't merge with shapefile
mg<-merge(pctbl,co,by="FIPS",all=T)

county.map$FIPS[county.map$FIPS=="02198"]<-"02201"
county.map$FIPS[county.map$FIPS=="02105"]<-"02232"
county.map$FIPS[county.map$FIPS=="02230"]<-"02232"
county.map$FIPS[county.map$FIPS=="02280"]<-"02195"
county.map$FIPS[county.map$FIPS=="02158"]<-"02270"


county.map$FIPS[county.map$FIPS=="51515"]<-"51019"

#independent cities in VA - not reported correctly or inconsistently
county.map$FIPS[county.map$FIPS=="51595"]<-"51081"
county.map$FIPS[county.map$FIPS=="51520"]<-"51191"
county.map$FIPS[county.map$FIPS=="51530"]<-"51163"
county.map$FIPS[county.map$FIPS=="51580"]<-"51081"
county.map$FIPS[county.map$FIPS=="51678"]<-"51163"


polys<-as(county.map,"Spatial")

newco <- gUnaryUnion(polys, id = county.map$FIPS)

Gid <- sapply(slot(newco, "polygons"), function(x) slot(x, "ID")) 

nco<-as.data.frame(Gid)
names(nco)<-c("FIPS")
nco$inshp<-1

dataimr$indat<-1

mg2<-merge(dataimr,nco,by="FIPS",all=T)


imr<-mg2 %>%
  group_by(FIPS) %>%
  summarise(blnum=sum(blnum,na.rm=T),
            blden=sum(blden,na.rm=T),
            whnum=sum(whnum,na.rm=T),
            whden=sum(whden,na.rm=T),
            pctbl=100*(blden/(totalbirths)))  


imr$ID<-imr$FIPS

imr %>%
  summarise(blden0=sum(blden==0),
            whden0=sum(whden==0),
            bldenlt20=sum(blden<20),
            whdenlt20=sum(whden<20),
            blnum0=sum(blnum==0),
            whnum0=sum(whnum==0),
            blnumlt20=sum(blnum<20),
            whnumlt20=sum(whden<20))

mgdat<-merge(nco,imr,by="FIPS",all=T)
row.names(mgdat)<-mgdat$ID
#check, merged correctly with 3142 counties

########################
#spatial weights matrix#
########################

poly<-SpatialPolygonsDataFrame(newco,data=as.data.frame(mgdat),match.ID=T)

coords<-coordinates(poly)

triang<-tri2nb(coords, row.names=NULL)
neib<-nb2WB(triang)

sum(neib$num)
neib$sumnb=sum(neib$num) #18822 
adj<-neib$adj
num<-neib$num

summary(neib$num)
# > summary(neib$num) #number of neighbors per county, min of 3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00    5.00    6.00    5.99    7.00   14.00 

##########################
bl<-mgdat %>%
  dplyr::select(FIPS,blnum,blden,pctbl,ID)
names(bl)<-c("FIPS","num","den","pctbl","ID")
bl$race<-"bl"

wh<-mgdat %>%
  dplyr::select(FIPS,whnum,whden,pctbl,ID)
names(wh)<-c("FIPS","num","den","pctbl","ID")
wh$race<-"wh"

Y<-rbind(bl,wh)
  
Y<-arrange(Y, race,ID)
fips<-Y$ID
N=length(Y$ID)
n=N/2
region<-as.numeric(factor(as.character(Y$ID)))      
region2<-as.numeric(factor(as.character(Y$ID)))
race2<-ifelse(Y$race=="wh",0,1)
i.intercept<-as.numeric(factor(Y$ID))
j.intercept<-2*as.numeric(factor(as.numeric(i.intercept)))
#j.intercept<-as.numeric(factor(Y$ID))
ind2<-as.numeric(factor(Y$ID))
intx<-as.numeric(factor(paste(Y$ID,Y$race,sep=".")))
numerator<-Y$num
denominator<-Y$den
pctbl<-Y$pctbl
numerator2<-ifelse(Y$num>Y$den,Y$den,Y$num)  #because of the record weights, numerator can be > denominator, need to ensure this doesn't happen

denominator2<-ifelse(Y$den>0,Y$den,(Y$den+1)) #need to have at least 1 in the denominator, so add 1 for counties with zero black births

Y$crude<-Y$num/Y$den

exp<-denominator2 #for Poisson models, exp=number of births
exp2<-denominator2*(9.4/100) #for Poisson models, exp=HP2020 target

data<-data.frame(fips,numerator2,race2,exp,exp2,i.intercept,j.intercept,intx,numerator,denominator,denominator2, region, region2, ind2,pctbl)

data2<-arrange(data, fips,race2)
data<-data2

setwd('C://***//') # must be letter drive
set.seed(1234)
inla.geobugs2inla(neib$adj, neib$num, graph.file="imr_map")


#############################################
# best fitting from above, try binomial model
# uncorrelated random intercept and slope ? 
# best fitting model for ptb
#############################################
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)

formulars2<-numerator2~race2+
  f(region, race2, model="iid",graph="imr_map",constr=T,hyper=list(prec=list(prior="gaussian",param=c(0,1))))+
 f(intx,model="iid")+
  f(region2,model="bym",graph="imr_map",param = c(prior.iid, prior.besag))

result1b<-inla(formulars2,family="binomial",Ntrials = denominator2 ,data=data, control.inla=list(cmin=0),control.compute=list(dic=TRUE,cpo=TRUE,waic=T,config=T))#,verbose=T)

result1b$dic$dic;result1b$dic$p.eff;result1b$waic$waic
# [1] 35968.14
# [1] 1941.325
# [1] 35858.19

plot(result1b)
log.score1rsb= -mean(log(result1b$cpo$cpo),na.rm=T)
log.score1rsb
# [1] 2.967243

vpit <- result1b$cpo$pit
vcpo <- result1b$cpo$cpo
Pxm1help <- vpit - vcpo
### be sure to avoid negative PITs
Pxm1 <- ifelse(Pxm1help<0,0,Pxm1help)
plot(pit(J=20, x=data$numerator, Px=vpit, Pxm1=Pxm1), ylim=c(0,3), ylab="Relative frequency", xlab="PIT")

write.csv(result1b$summary.fitted.values,paste(outputdir,"model6bin_v2.csv",sep=""),row.names=F)


# ################################################################################################
#https://haakonbakka.bitbucket.io/btopic112.html

numsim=500
s1000 = inla.posterior.sample(numsim,result1b,intern=F) 

dt<-data.frame(s1000[[1]]$latent)
dt$num<-1:length((dt[,1]))

simlist<-vector('list',numsim)
#s1000[[1]] =inla.link.invlogit(s1000[[1]]$latent)

for(i in 1:numsim) { 
  simlist[[i]]<-paste("sim",i,sep=".")
  assign(paste("sim",i,sep="."),s1000[[i]]$latent)
  sims<-get(paste(simlist[[i]]))
  sims1<-(as.data.frame(cbind(sims[1:6272], 
                             rbind(as.data.frame(sims[6273:9408]),as.data.frame(sims[6273:9408])), 
                             sims[9409:15680], 
                             rbind(as.data.frame(sims[15681:18816]),as.data.frame(sims[15681:18816])),
                             rbind(as.data.frame(sims[18817:21952]),as.data.frame(sims[18817:21952])))))
  names(sims1)<-c("pred","region","intx","reg2.1","reg2.2")
  sims1$int<-sims[21953]
  sims1$fix<-sims[21954]
  sims<-cbind(data[,c(1,3)],sims1)
  sims$p1<-inv.logit(sims$int+sims$region+sims$intx+sims$reg2.1+sims$reg2.2+(sims$race2*sims$fix))
  sims$p2<-inv.logit(sims$pred)
  sims<-cbind(sims,result1b$summary.fitted.values)
  
  assign(paste(simlist[[i]]),sims)
  
  rm(sims,sims1)
}


allsim<-sim.1
allsim$sim<-1
for (i in 2:numsim){
  simdat<-get(paste(simlist[[i]][1]))
  simdat$sim<-paste(i)
  allsim<-rbind(allsim,simdat)
}

for (i in 1:numsim){
  rm(list=paste("sim",i,sep="."))
}


simrace<-allsim %>%
  arrange(sim,fips,race2) %>%
  dplyr::select(fips,race2,p2,`0.5quant`,sim) %>%
  group_by(fips,sim) %>%
  mutate(rdiff=p2/lag(p2),adiff=p2-lag(p2)) %>%
  ungroup() %>%
  group_by(fips,race2) %>%
  summarise(med_rate=100*median(p2),
            med_rdiff=median(rdiff,na.rm=T),
            lower95r=quantile(rdiff,prob=c(.025),na.rm=T),
            upper95r=quantile(rdiff,prob=c(.975),na.rm=T),
            med_adiff=100*median(adiff,na.rm=T),
            lower95a=100*quantile(adiff,prob=c(.025),na.rm=T),
            upper95a=100*quantile(adiff,prob=c(.975),na.rm=T),
            inlamed=100*median(`0.5quant`))

  
head(simrace)
write.csv(simrace,paste(outputdir,"racediffs_ptb_county.csv",sep=""),row.names=F)


#######################################################################################################################################
# best fitting model...
# exp=HP2020 target
#######################################################################################################################################
#######################################################################################################################################

result1nocorrs_hp<-inla(formulars2,family="poisson",E=exp2,data=data, control.inla=list(cmin=0),control.compute=list(dic=TRUE,cpo=TRUE,waic=T))#,verbose=T)
result1nocorrs_hp$dic$dic;result1nocorrs_hp$dic$p.eff;result1nocorrs_hp$waic$waic
# [1] 36150.13
# [1] 1779.66
# [1] 36032.51
plot(result1nocorrs_hp)
log.score1nocorrs_hp= -mean(log(result1nocorrs_hp$cpo$cpo),na.rm=T)
log.score1nocorrs_hp
# [1] 2.964607


vpit <- result1nocorrs_hp$cpo$pit
vcpo <- result1nocorrs_hp$cpo$cpo
Pxm1help <- vpit - vcpo
### be sure to avoid negative PITs
Pxm1 <- ifelse(Pxm1help<0,0,Pxm1help)
plot(pit(J=20, x=data$numerator, Px=vpit, Pxm1=Pxm1), ylim=c(0,2), ylab="Relative frequency", xlab="PIT")

write.csv(result1nocorrs_hp$summary.fitted.values,paste(outputdir,"model4_hp_v2.csv",sep=""),row.names=F)











