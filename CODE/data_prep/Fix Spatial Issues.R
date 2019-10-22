# Description Source: https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
#	FIPS Code 51515 *(Bedford City, VA) - add to 51019
# FIPS Code 46113 (Shannon County, SD) - rename to 46102
#	FIPS Code 02201 (Prince of Wales-Outer-Ketchikan Census Area) - Drop
#	FIPS Code 02232 (Skagway-Hoonah-Angoon Census Area, AK) - Drop
#	FIPS Code 02270	(Wade Hampton Census Area, AK)
#	FIPS Code 02280 (Wrangell-Petersburg Census Area, AK) - Drop94 – 2013

rm(list = ls())
library(dplyr)

# Model 1 --------------------------------------------
model1adj <- readRDS('../../data/nchs_births/R/Data/model1.rda') 

# 9/16/19 - Michael troubleshooting:
model1adj <- readRDS('nchs_births/R/Data/model1.rda') 

# Drop Alaskan FIPS code issues
model1adj <- model1adj[which(!(model1adj$combfips %in% c('02201', '02232', 
                                                         '02270', '02280'))),]
# Rename South Dakotan FIPS
model1adj$combfips[which(model1adj$combfips == '46113')] <- '46102'

#### Merge Virginian FIPS
sub <- model1adj[which(model1adj$combfips %in% c('51515', '51019')),]
model1adj <- model1adj[which(!(model1adj$combfips %in% c('51515', '51019'))),]

# Aggregate all birth categories
births <- aggregate(births ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
ptb <- aggregate(ptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
vptb <- aggregate(vptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
lptb <- aggregate(lptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
mptb <- aggregate(mptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)

# Merge dataset
merged <- left_join(births, ptb, by = c("dob_yy", "racehisp_recode"))
merged <- left_join(merged, vptb, by = c("dob_yy", "racehisp_recode"))
merged <- left_join(merged, lptb, by = c("dob_yy", "racehisp_recode"))
merged <- left_join(merged, mptb, by = c("dob_yy", "racehisp_recode"))
merged$combfips <- rep("51019", nrow(merged))
merged <- merged[, c(1, 8, 2:7)]

# Re-sort dataset
model1adj <- as.data.frame(rbind(model1adj, merged))
model1adj <- model1adj[with(model1adj, order(dob_yy, combfips, racehisp_recode)), ]

# Save dataset
saveRDS(model1adj, file = "../../data/nchs_births/R/Data/model1adj.rda")

# Model 2 ---------------------------------------------------------
model2adj <- readRDS('../../data/nchs_births/R/Data/model2.rda') 

# Drop Alaskan FIPS code issues
model2adj <- model2adj[which(!(model2adj$combfips %in% c('02201', '02232', 
                                                         '02270', '02280'))),]
# Rename South Dakotan FIPS
model2adj$combfips[which(model2adj$combfips == '46113')] <- '46102'

#### Merge Virginian FIPS
sub <- model2adj[which(model2adj$combfips %in% c('51515', '51019')),]
model2adj <- model2adj[which(!(model2adj$combfips %in% c('51515', '51019'))),]

# Aggregate all birth categories
births <- aggregate(births ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
ptb <- aggregate(ptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
vptb <- aggregate(vptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
lptb <- aggregate(lptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
mptb <- aggregate(mptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)

# Merge dataset
merged <- left_join(births, ptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged <- left_join(merged, vptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged <- left_join(merged, lptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged <- left_join(merged, mptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged$combfips <- rep("51019", nrow(merged))
merged <- merged[, c(1, 9, 2:8)]

# Re-sort dataset
model2adj <- as.data.frame(rbind(model2adj, merged))
model2adj <- model2adj[with(model2adj, 
                            order(dob_yy, combfips, racehisp_recode, mager9)), ]

# Save dataset
saveRDS(model2adj, file = "../../data/nchs_births/R/Data/model2adj.rda")

# Model 3 -------------------------------------------------
model3adj <- readRDS('../../data/nchs_births/R/Data/model3.rda') 

# Drop Alaskan FIPS code issues
model3adj <- model3adj[which(!(model3adj$combfips %in% c('02201', '02232', 
                                                         '02270', '02280'))),]
# Rename South Dakotan FIPS
model3adj$combfips[which(model3adj$combfips == '46113')] <- '46102'

#### Merge Virginian FIPS
sub <- model3adj[which(model3adj$combfips %in% c('51515', '51019')),]
model3adj <- model3adj[which(!(model3adj$combfips %in% c('51515', '51019'))),]

# Aggregate all birth categories
births <- aggregate(births ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                    data = sub, FUN = sum)
ptb <- aggregate(ptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                 data = sub, FUN = sum)
vptb <- aggregate(vptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                  data = sub, FUN = sum)
lptb <- aggregate(lptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                  data = sub, FUN = sum)
mptb <- aggregate(mptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                  data = sub, FUN = sum)

# Merge dataset
merged <- left_join(births, ptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                        "mar", "meduc_r"))
merged <- left_join(merged, vptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                         "mar", "meduc_r"))
merged <- left_join(merged, lptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                        "mar", "meduc_r"))
merged <- left_join(merged, mptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                         "mar", "meduc_r"))
merged$combfips <- rep("51019", nrow(merged))
merged <- merged[, c(1, 11, 2:10)]

# Re-sort dataset
model3adj <- as.data.frame(rbind(model3adj, merged))
model3adj <- model3adj[with(model3adj, 
                            order(dob_yy, combfips, racehisp_recode, mager9, mar, meduc_r)), ]

# Save dataset
saveRDS(model3adj, file = "../../data/nchs_births/R/Data/model3adj.rda")

# Model 4 -------------------------------------------------
model4adj <- readRDS('../../data/nchs_births/R/Data/model4.rda') 

# Drop Alaskan FIPS code issues
model4adj <- model4adj[which(!(model4adj$combfips %in% c('02201', '02232', 
                                                         '02270', '02280'))),]
# Rename South Dakotan FIPS
model4adj$combfips[which(model4adj$combfips == '46113')] <- '46102'

#### Merge Virginian FIPS
sub <- model4adj[which(model4adj$combfips %in% c('51515', '51019')),]
model4adj <- model4adj[which(!(model4adj$combfips %in% c('51515', '51019'))),]

# Aggregate all birth categories
births <- aggregate(births ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
ptb <- aggregate(ptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
vptb <- aggregate(vptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
lptb <- aggregate(lptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)
mptb <- aggregate(mptb ~ dob_yy + racehisp_recode, data = sub, FUN = sum)

# Merge dataset
merged <- left_join(births, ptb, by = c("dob_yy", "racehisp_recode"))
merged <- left_join(merged, vptb, by = c("dob_yy", "racehisp_recode"))
merged <- left_join(merged, lptb, by = c("dob_yy", "racehisp_recode"))
merged <- left_join(merged, mptb, by = c("dob_yy", "racehisp_recode"))
merged$combfips <- rep("51019", nrow(merged))
merged <- merged[, c(1, 8, 2:7)]

# Re-sort dataset
model4adj <- as.data.frame(rbind(model4adj, merged))
model4adj <- model4adj[with(model4adj, order(dob_yy, combfips, racehisp_recode)), ]

# Save dataset
saveRDS(model4adj, file = "../../data/nchs_births/R/Data/model4adj.rda")

# Model 5 -------------------------------------------------
model5adj <- readRDS('../../data/nchs_births/R/Data/model5.rda') 

# Drop Alaskan FIPS code issues
model5adj <- model5adj[which(!(model5adj$combfips %in% c('02201', '02232', 
                                                         '02270', '02280'))),]
# Rename South Dakotan FIPS
model5adj$combfips[which(model5adj$combfips == '46113')] <- '46102'

#### Merge Virginian FIPS
sub <- model5adj[which(model5adj$combfips %in% c('51515', '51019')),]
model5adj <- model5adj[which(!(model5adj$combfips %in% c('51515', '51019'))),]

# Aggregate all birth categories
births <- aggregate(births ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
ptb <- aggregate(ptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
vptb <- aggregate(vptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
lptb <- aggregate(lptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)
mptb <- aggregate(mptb ~ dob_yy + racehisp_recode + mager9, data = sub, FUN = sum)

# Merge dataset
merged <- left_join(births, ptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged <- left_join(merged, vptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged <- left_join(merged, lptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged <- left_join(merged, mptb, by = c("dob_yy", "racehisp_recode", "mager9"))
merged$combfips <- rep("51019", nrow(merged))
merged <- merged[, c(1, 9, 2:8)]

# Re-sort dataset
model5adj <- as.data.frame(rbind(model5adj, merged))
model5adj <- model5adj[with(model5adj, 
                            order(dob_yy, combfips, racehisp_recode, mager9)), ]

# Save dataset
saveRDS(model5adj, file = "../../data/nchs_births/R/Data/model5adj.rda")

# Model 6 -------------------------------------------------
model6adj <- readRDS('../../data/nchs_births/R/Data/model6.rda') 

# Drop Alaskan FIPS code issues
model6adj <- model6adj[which(!(model6adj$combfips %in% c('02201', '02232', 
                                                         '02270', '02280'))),]
# Rename South Dakotan FIPS
model6adj$combfips[which(model6adj$combfips == '46113')] <- '46102'

#### Merge Virginian FIPS
sub <- model6adj[which(model6adj$combfips %in% c('51515', '51019')),]
model6adj <- model6adj[which(!(model6adj$combfips %in% c('51515', '51019'))),]

# Aggregate all birth categories
births <- aggregate(births ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                    data = sub, FUN = sum)
ptb <- aggregate(ptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                 data = sub, FUN = sum)
vptb <- aggregate(vptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                  data = sub, FUN = sum)
lptb <- aggregate(lptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                  data = sub, FUN = sum)
mptb <- aggregate(mptb ~ dob_yy + racehisp_recode + mager9 + mar + meduc_r, 
                  data = sub, FUN = sum)

# Merge dataset
merged <- left_join(births, ptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                        "mar", "meduc_r"))
merged <- left_join(merged, vptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                         "mar", "meduc_r"))
merged <- left_join(merged, lptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                         "mar", "meduc_r"))
merged <- left_join(merged, mptb, by = c("dob_yy", "racehisp_recode", "mager9",
                                         "mar", "meduc_r"))
merged$combfips <- rep("51019", nrow(merged))
merged <- merged[, c(1, 11, 2:10)]

# Re-sort dataset
model6adj <- as.data.frame(rbind(model6adj, merged))
model6adj <- model6adj[with(model6adj, 
                            order(dob_yy, combfips, racehisp_recode, mager9, mar, meduc_r)), ]

# Save dataset
saveRDS(model6adj, file = "../../data/nchs_births/R/Data/model6adj.rda")
