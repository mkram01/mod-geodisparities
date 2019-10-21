# March of Dimes Analysis      #
# 01: Data Cleaning Script     #
# Kevin Weiss                  #
# 02/04/2019                   #

## Package and Data setup -----------------
rm(list = ls())
library(sas7bdat) # for data transfer
library(haven) # for data transfer
library(Hmisc) # for data transfer
library(dplyr) # for data manipulation
library(plyr) # for data manipulation
library(tidyr) # for data manipulation
library(magrittr) # for chaining actions together

##  Convert model SAS files to R data files (note: done on local computer, not on Git)
# Emory Computer
model1 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model1.sas7bdat"))
model2 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model2.sas7bdat"))
model3 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model3.sas7bdat"))
model4 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model4.sas7bdat"))
model5 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model5.sas7bdat"))
model6 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model6.sas7bdat"))

# Set NA values to 0
model1$mptb[which(is.na(model1$mptb))] <- 0 
model1$lptb[which(is.na(model1$lptb))] <- 0
model1$vptb[which(is.na(model1$vptb))] <- 0
model1$ptb[which(is.na(model1$ptb))] <- 0

model2$mptb[which(is.na(model2$mptb))] <- 0
model2$lptb[which(is.na(model2$lptb))] <- 0
model2$vptb[which(is.na(model2$vptb))] <- 0
model2$ptb[which(is.na(model2$ptb))] <- 0

model3$mptb[which(is.na(model3$mptb))] <- 0
model3$lptb[which(is.na(model3$lptb))] <- 0
model3$vptb[which(is.na(model3$vptb))] <- 0
model3$ptb[which(is.na(model3$ptb))] <- 0

model4$mptb[which(is.na(model4$mptb))] <- 0
model4$lptb[which(is.na(model4$lptb))] <- 0
model4$vptb[which(is.na(model4$vptb))] <- 0
model4$ptb[which(is.na(model4$ptb))] <- 0

model5$mptb[which(is.na(model5$mptb))] <- 0
model5$lptb[which(is.na(model5$lptb))] <- 0
model5$vptb[which(is.na(model5$vptb))] <- 0
model5$ptb[which(is.na(model5$ptb))] <- 0

model6$mptb[which(is.na(model6$mptb))] <- 0
model6$lptb[which(is.na(model6$lptb))] <- 0
model6$vptb[which(is.na(model6$vptb))] <- 0
model6$ptb[which(is.na(model6$ptb))] <- 0

# Rename total births variable
model1 <- rename(model1, replace = c("_FREQ_" = "births"))
model2 <- rename(model2, replace = c("_FREQ_" = "births"))
model3 <- rename(model3, replace = c("_FREQ_" = "births"))
model4 <- rename(model4, replace = c("_FREQ_" = "births"))
model5 <- rename(model5, replace = c("_FREQ_" = "births"))
model6 <- rename(model6, replace = c("_FREQ_" = "births"))

# Add ID variable (CDC method - need to revisit)
length(unique(model1$combfips)) #3148
length(unique(model2$combfips)) #3148
length(unique(model3$combfips)) #3147
length(unique(model4$combfips)) #3148
length(unique(model5$combfips)) #3148
length(unique(model6$combfips)) #3147

# Model 1: YEAR x COUNTY x RACE restricted to SINGLETONS and NH-Black/NH-White
# Model 2: YEAR x COUNTY x RACE x AGE restricted to SINGLETONS
# Model 3: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION restricted to SINGLETONS
# Model 4: YEAR x COUNTY x RACE restricted to NH-Black/NH-White (including multiple births)
# Model 5: YEAR x COUNTY x RACE x AGE (including multiple births)
# Model 6: YEAR x COUNTY x RACE x AGE x MARITAL x EDUCATION (including multiple births)
# 
# model1 <- model1[order(model1$combfips, model1$dob_yy),]
# model1$countyid <- rep(1:length(unique(model1$combfips)), each = 20) # number of counties
# model1$countyid2 <- model1$countyid #number of counties for second random effect
# model1$resid <- rep(1:62960) #number of county-year observations (3148*10 years)
# model1$year <- rep(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10), 
#                      len = 62960) #year variable
# 
# model2 <- model2[order(model2$combfips, model2$dob_yy),]
# model2$countyid <- rep(1:length(unique(model2$combfips)), each = 20) # number of counties
# model2$countyid2 <- model2$countyid #number of counties for second random effect
# model2$resid <- rep(1:1699920) #number of county-year observations (3148*10 years *6 race * 9 age)
# model2$year <- rep(c(rep(1, 18), rep2:18 
#                    len = 1699920) #year variable

# Expected cases (raw counts of events relative to some expected value, or population offset)
ptbrates <- aggregate(ptb ~ 1, model1, mean) #standardize to mean ptb rate
model1$meanptb <- ptbrates$ptb
ptbrates <- aggregate(ptb ~ 1, model2, mean) #standardize to mean ptb rate
model2$meanptb <- ptbrates$ptb
ptbrates <- aggregate(ptb ~ 1, model3, mean) #standardize to mean ptb rate
model3$meanptb <- ptbrates$ptb
ptbrates <- aggregate(ptb ~ 1, model4, mean) #standardize to mean ptb rate
model4$meanptb <- ptbrates$ptb
ptbrates <- aggregate(ptb ~ 1, model5, mean) #standardize to mean ptb rate
model5$meanptb <- ptbrates$ptb
ptbrates <- aggregate(ptb ~ 1, model6, mean) #standardize to mean ptb rate
model6$meanptb <- ptbrates$ptb

mptbrates <- aggregate(mptb ~ 1, model1, mean) #standardize to mean mptb rate
model1$meanmptb <- mptbrates$mptb
mptbrates <- aggregate(mptb ~ 1, model2, mean) #standardize to mean mptb rate
model2$meanmptb <- mptbrates$mptb
mptbrates <- aggregate(mptb ~ 1, model3, mean) #standardize to mean mptb rate
model3$meanmptb <- mptbrates$mptb
mptbrates <- aggregate(mptb ~ 1, model4, mean) #standardize to mean mptb rate
model4$meanmptb <- mptbrates$mptb
mptbrates <- aggregate(mptb ~ 1, model5, mean) #standardize to mean mptb rate
model5$meanmptb <- mptbrates$mptb
mptbrates <- aggregate(mptb ~ 1, model6, mean) #standardize to mean mptb rate
model6$meanmptb <- mptbrates$mptb

vptbrates <- aggregate(vptb ~ 1, model1, mean) #standardize to mean vptb rate
model1$meanvptb <- vptbrates$vptb
vptbrates <- aggregate(vptb ~ 1, model2, mean) #standardize to mean vptb rate
model2$meanvptb <- vptbrates$vptb
vptbrates <- aggregate(vptb ~ 1, model3, mean) #standardize to mean vptb rate
model3$meanvptb <- vptbrates$vptb
vptbrates <- aggregate(vptb ~ 1, model4, mean) #standardize to mean vptb rate
model4$meanvptb <- vptbrates$vptb
vptbrates <- aggregate(vptb ~ 1, model5, mean) #standardize to mean vptb rate
model5$meanvptb <- vptbrates$vptb
vptbrates <- aggregate(vptb ~ 1, model6, mean) #standardize to mean vptb rate
model6$meanvptb <- vptbrates$vptb

lptbrates <- aggregate(lptb ~ 1, model1, mean) #standardize to mean lptb rate
model1$meanlptb <- lptbrates$lptb
lptbrates <- aggregate(lptb ~ 1, model2, mean) #standardize to mean lptb rate
model2$meanlptb <- lptbrates$lptb
lptbrates <- aggregate(lptb ~ 1, model3, mean) #standardize to mean lptb rate
model3$meanlptb <- lptbrates$lptb
lptbrates <- aggregate(lptb ~ 1, model4, mean) #standardize to mean lptb rate
model4$meanlptb <- lptbrates$lptb
lptbrates <- aggregate(lptb ~ 1, model5, mean) #standardize to mean lptb rate
model5$meanlptb <- lptbrates$lptb
lptbrates <- aggregate(lptb ~ 1, model6, mean) #standardize to mean lptb rate
model6$meanlptb <- lptbrates$lptb


# Output GA test file
gatestmodel1 <- model1 %>%
  filter(substr(combfips, 1, 2) == "13")

gatestmodel1singleyear <- model1 %>%
  filter(substr(combfips, 1, 2) == "13") %>%
  filter(dob_yy == "2007")

model1singleyear <- model1 %>%
  filter(dob_yy == "2007")

# Save as .rda files
saveRDS(model1, file = "DATA/nchs_births/R/data/model1.rda")
saveRDS(model2, file = "DATA/nchs_births/R/data/model2.rda")
saveRDS(model3, file = "DATA/nchs_births/R/data/model3.rda")
saveRDS(model4, file = "DATA/nchs_births/R/data/model4.rda")
saveRDS(model5, file = "DATA/nchs_births/R/data/model5.rda")
saveRDS(model6, file = "DATA/nchs_births/R/data/model6.rda")
saveRDS(gatestmodel1, file = "DATA/nchs_births/R/data/gatestmodel1.rda")
saveRDS(gatestmodel1singleyear, file = "DATA/nchs_births/R/data/gatestmodel1singleyear.rda")
saveRDS(model1singleyear, file = "DATA/nchs_births/R/data/model1singleyear.rda")
