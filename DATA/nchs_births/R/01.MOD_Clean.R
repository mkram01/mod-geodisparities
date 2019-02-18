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
library(sp) # for spatial data manipulation
library(spdep) # for adjacency matrix creation
library(tigris) # for TIGRIS county shapefile
library(colorspace) # for HCL color palette
library(grid) # for spplot customization

##  Convert model SAS files to R data files (note: done on local computer, not on Git)
# Emory Computer
model1 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model1.sas7bdat"))
model2 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model2.sas7bdat"))
model3 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model3.sas7bdat"))
model4 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model4.sas7bdat"))
model5 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model5.sas7bdat"))
model6 <- as.data.frame(haven::read_sas("C:/Users/kweiss2/Documents/March of Dimes/model6.sas7bdat"))

# Set NA values to 0
model1$mptb[which(is.na(model1$mptb))] <- model1$lptb[which(is.na(model1$lptb))] <- 0
model1$vptb[which(is.na(model1$vptb))] <- model1$ptb[which(is.na(model1$ptb))] <- 0

model2$mptb[which(is.na(model2$mptb))] <- model2$lptb[which(is.na(model2$lptb))] <- 0
model2$vptb[which(is.na(model2$vptb))] <- model2$ptb[which(is.na(model2$ptb))] <- 0

model3$mptb[which(is.na(model3$mptb))] <- model3$lptb[which(is.na(model3$lptb))] <- 0
model3$vptb[which(is.na(model3$vptb))] <- model3$ptb[which(is.na(model3$ptb))] <- 0

model4$mptb[which(is.na(model4$mptb))] <- model4$lptb[which(is.na(model4$lptb))] <- 0
model4$vptb[which(is.na(model4$vptb))] <- model4$ptb[which(is.na(model4$ptb))] <- 0

model5$mptb[which(is.na(model5$mptb))] <- model5$lptb[which(is.na(model5$lptb))] <- 0
model5$vptb[which(is.na(model5$vptb))] <- model5$ptb[which(is.na(model5$ptb))] <- 0

model6$mptb[which(is.na(model6$mptb))] <- model6$lptb[which(is.na(model6$lptb))] <- 0
model6$vptb[which(is.na(model6$vptb))] <- model6$ptb[which(is.na(model6$ptb))] <- 0

# Rename total births variable
model1 <- rename(model1, replace = c("_FREQ_" = "births"))
model2 <- rename(model2, replace = c("_FREQ_" = "births"))
model3 <- rename(model3, replace = c("_FREQ_" = "births"))
model4 <- rename(model4, replace = c("_FREQ_" = "births"))
model5 <- rename(model5, replace = c("_FREQ_" = "births"))
model6 <- rename(model6, replace = c("_FREQ_" = "births"))

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
