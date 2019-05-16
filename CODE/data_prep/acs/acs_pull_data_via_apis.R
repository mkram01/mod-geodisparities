############################################
# code author: erin stearns
# script objective: obtain ACS 5-year estimates for 2007-2016 via the US Census Bureau's APIs
# date: 22 january 2019
###########################################

# ----------------- TO DO ---------------------
#
# Final clean up, create state and state code cols -- API not working properly on 2/20, will update once working


# To run the code below be sure to sign up for an API key here(https://api.census.gov/data/key_signup.html)
#   Then, if you’re on a non-shared computer, add your Census API key to your .Renviron profile and 
#         call it CENSUS_API_KEY. tidycensus will use it by default without any extra work on your part. 
#   If on a shared computer, within R, run:
    # # Add key to .Renviron
    # Sys.setenv(CENSUS_API_KEY=<YOURKEYHERE>)
    # # Reload .Renviron
    # readRenviron("~/.Renviron")
    # # Check to see that the expected key is output in your R console
    # Sys.getenv("CENSUS_API_KEY")

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table, tidycensus, tidyr, tidyverse,dplyr, feather, sf)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#source function script
source(paste0(repo, "/CODE/data_prep/acs/pull_acs_fxn.R"))

#setting working directory
setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

######################################################################################################
# -------------------------------------- load config ----------------------------------------------- #
######################################################################################################
config <- fread(file = paste0(data_repo, "/acs/config_acs_api_pull.csv"), stringsAsFactors = F)

#define the survey to use -- currently function can only handle one value here, if wanting 
#   to use different surveys for different indicators, will need to change some things
acs_survey <- unique(config$survey)

#define geography  -- currently function can only handle one value here, if wanting 
#   to use different surveys for different indicators, will need to change some things
geo <- unique(config$geo_level)

######################################################################################################
# -------------------------------------- call funciton --------------------------------------------- #
######################################################################################################
#get unique set of groups and years
grp_yrs <- (unique(config[,c('grouping','year'),with=F]))

#define a vector of groups -- used this in config, field entitled 'grouping'; will use this vector
#     to apply function over each element in vector
groups <- grp_yrs$grouping

#define a vector of years corresponding to groups to apply function over each element
years <- grp_yrs$year

#validation check:
if (length(groups) != length(years)){
  message("Your grouping and year vectors are of unequal length -- need to correct this to proceed")
  stop()
} else {
  message("Your grouping and year vectors are of equal length - WOOT WOOT!")
}

#call function
acs_all_list <- mapply(pull_acsdata, groups, years, SIMPLIFY = FALSE)

#bind all tables together into one
acs_all <- rbindlist(l=acs_all_list, use.names = TRUE)

#save as feather
#write_feather(acs_all, paste0(data_repo,"/acs/acs5_2009_2017_raw.feather"))

######################################################################################################
# -------------------------------------- wrangling ------------------------------------------------- #
######################################################################################################
#drop margin of error col & 'variable' col
acs_all <- acs_all[,-c("moe", "variable"),with=F]

######################################################################################################
# ------------------------------------- race/ethnicity --------------------------------------------- #
######################################################################################################
#calculate the following fields:
##  ---- percent black
##  ---- percent white
##  ---- percent hispanic
##  ---- black-white ratio

#grab "race_eth" group only
re <- acs_all[grouping == "race_eth",]

#cast wide on mod_label
re_wide <- dcast.data.table(re, GEOID + year + grouping + source + NAME ~ mod_label, value.var = "estimate")

#calculate percents
re_wide[,perc_black:=(pop_black/pop_total)]
re_wide[,perc_hisp:=(pop_hispanic/pop_total)]
re_wide[,perc_white:=(pop_white/pop_total)]

#calculate black-white ratio
re_wide[,blackwhite_ratio:=pop_black/pop_white]

#validation checks
if (nrow(re_wide[perc_black > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% of the population black-- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated percent population black numbers exceed 100%!")
}
if (nrow(re_wide[perc_hisp > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% of the population hispacnic-- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated percent population hispanic numbers exceed 100%!")
}
if (nrow(re_wide[perc_white > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% of the population white -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated percent population white numbers exceed 100%!")
}


#drop original cols
re_wide <- re_wide[,-c("pop_black", "pop_hispanic", "pop_white", "pop_total"),with=F]

#melt back long
re_long <- melt.data.table(re_wide, id.vars = c("GEOID","year","grouping","source","NAME"),
                           variable.name = "mod_label",
                           value.name = "estimate",
                           variable.factor = FALSE)
######################################################################################################
# ------------------------------------- income ----------------------------------------------------- #
######################################################################################################
#grab "income" group only -- no transformation needed
inc <- acs_all[grouping == "income",]

#change col order
inc_long <- inc[,c("GEOID", "year", "grouping", "source", "NAME", "mod_label", "estimate")]

######################################################################################################
# ------------------------------------- poverty ---------------------------------------------------- #
######################################################################################################
#grab "poverty" group only
pov <- acs_all[grouping == "poverty",]

#calculate the following fields:
##  ---- household poverty rate (hh poverty/total)
##  ---- female poverty rate (individual, female poverty/total)

#cast wide on mod_label
pov_wide <- dcast.data.table(pov, GEOID + year + grouping + source + NAME ~ mod_label, value.var = "estimate")

#calculate poverty rates
pov_wide[,poverty_hh_rate:=(poverty_hh/poverty_hh_denom)]
pov_wide[,poverty_fem_rate:=(poverty_females/poverty_ind_denom)]

#validation checks
if (nrow(pov_wide[poverty_hh_rate > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% of households living in poverty -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated poverty rates exceed 100%!")
}
if (nrow(pov_wide[poverty_fem_rate > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% of females living in poverty -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated female poverty rates exceed 100%!")
}

#drop original cols
pov_wide <- pov_wide[,-c("poverty_hh", "poverty_hh_denom", "poverty_females", "poverty_ind_denom"),with=F]

#melt back long
pov_long <- melt.data.table(pov_wide, id.vars = c("GEOID","year","grouping","source","NAME"),
                           variable.name = "mod_label",
                           value.name = "estimate",
                           variable.factor = FALSE)

######################################################################################################
# ------------------------------------- housing ---------------------------------------------------- #
######################################################################################################
#grab "housing" group only 
hous <- acs_all[grouping == "housing",]

#calculate the following fields:
##  ---- housing stability: (Owner occupied:Moved in past 1-5 years + Renter occupied:!!Moved in 2005 or later)/Total households
##  ---- housing tenure: Owners/renters

#cast wide on mod_label
hous_wide <- dcast.data.table(hous, GEOID + year + grouping + source + NAME ~ mod_label, value.var = "estimate")

#calculate housing stability
hous_wide[,stability:=((stability_owner + stability_renter)/stability_total)]
#calculate housing tenure
hous_wide[,tenure:=(tenure_owner/tenure_renter)]

#validation check
if (nrow(hous_wide[stability > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% stability  -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated stability exceed 100%!")
}

#drop original cols
hous_wide <- hous_wide[,-c("stability_owner", "stability_renter", "stability_total", 
                           "tenure_owner", "tenure_renter", "tenure_total"),with=F]

#melt back long
hous_long <- melt.data.table(hous_wide, id.vars = c("GEOID","year","grouping","source","NAME"),
                            variable.name = "mod_label",
                            value.name = "estimate",
                            variable.factor = FALSE)

######################################################################################################
# ------------------------------------- education -------------------------------------------------- #
######################################################################################################
#grab "education" group only
ed <- acs_all[grouping == "education",]

#calculate the following fields:
##  ---- Education < HS
##  ---- Education HS/GED
##  ---- Education some college or higher -- sum(some college + college or higher/total)

#cast wide on mod_label
ed_wide <- dcast.data.table(ed, GEOID + year + grouping + source + NAME ~ mod_label, value.var = "estimate")

#calculate fields
ed_wide[,edu_lt_hs:=(edu_lt_hs/edu_total)]
ed_wide[,edu_hs_ged:=(edu_hs_ged/edu_total)]
ed_wide[,edu_collegeplus:=((edu_some_college + edu_college)/edu_total)]

#validation checks
if (nrow(ed_wide[edu_lt_hs > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% less than HS  -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated 'less than HS' exceed 100%!")
}

if (nrow(ed_wide[edu_hs_ged > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% ged/hs  -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated 'ged/hs' exceed 100%!")
}

if (nrow(ed_wide[edu_collegeplus > 1,]) != 0) {
  message("Uh oh -- it appears you have over 100% college plus  -- check your calculations again")
  stop()
} else {
  message("Niiice! None of your calculated 'college plus' exceed 100%!")
}

#drop original cols
ed_wide <- ed_wide[,-c("edu_college", "edu_some_college", "edu_total"),with=F]

#melt back long
ed_long <- melt.data.table(ed_wide, id.vars = c("GEOID","year","grouping","source","NAME"),
                             variable.name = "mod_label",
                             value.name = "estimate",
                             variable.factor = FALSE)

######################################################################################################
# ------------------------------------- all together ----------------------------------------------- #
######################################################################################################
fin <- rbindlist(l=list(re_long, inc_long, pov_long, hous_long, ed_long))

#grab 2015 and copy for 2016 and 2017 and then bind again
tbl_2016 <- fin[year == 2015,]
tbl_2016 <- tbl_2016[,year:=2016]
tbl_2017 <- fin[year == 2015,]
tbl_2017 <- tbl_2017[,year:=2017]

#bind these 2 additional table to main
final <- rbindlist(l=list(fin, tbl_2016, tbl_2017))


## ----- TO DO ** -------------------
#create state col & remove whitespace
final[,state_name:=trimws(sapply(strsplit(NAME,","), "[", 2))]

#create state code col
final[,state_code:=trimws(substr(GEOID, start = 1, stop = 2))]

#rearrange col orders
final <- final[]

#save as feather
#write_feather(final, paste0(data_repo,"/acs/acs5_2007_2017_rtg.feather"))

######################################################################################################
# -------------------------------------- 2010 county boundaries ------------------------------------ #
######################################################################################################
#get base pop & associated spatial data -- NOT CURRENTLY WORKING -- return to last
acs_geo <- get_acs(geography = (unique(config$geo_level)),
                   variables = "B25001_001E",
                   year = 2010,
                   survey = "acs5",
                   geometry = TRUE
)

#saveRDS(acs_geo, paste0(data_repo,"/acs/2010_county_shp/acs_2010_counties.Rds"))


#****************************************************************************************************************
# -------------------- EVERYTHING BELOW THIS LINE IS STILL 'UNDER CONSTRUCTION' -----------------
#****************************************************************************************************************
