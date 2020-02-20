############################################
# code author: erin stearns
# script objective: prep new app inputs - contextual vars & model outcomes
# date: 3 november 2019
###########################################

rm(list = ls())

#NTS:
#   - do i need to even load the basegeo file?
#   - do i need the states file anymore? - no



# Summary of what this script does to prepare the data for being used by the app:
#    1. ACS & model data: Create state GEOID
#    2. ACS & model data: validate inputs
#    3. ACS & model metadata: remove trailing whitespace in display_names
#    4. Model metadata: create fields in model metadata that are present in contextual metadata so fields and dimensions match
#                      - Specifically create these 4 fields: category_name, plot_type, reverse_palette, data_source
#    5. ACS metadata: define MCD palette in contextual metadata
#    6. mapyears: wrangle
#                      - fix columns named 2015.1 and 2015.2 to read as 2016 and 2017, respectively
#                      - remove X from col names
#                      - join to contextual metadata to get display name -- this is what is used for matching in the app
#                      - reshape long 
#                      - visualization year (the year users can select)
#    7. ACS & model metadata: create 'label' field for in-app unit labeling 
#    8. ACS & model data: rename data fields to their human-readable versions (display names)
#    9. ACS & model data: join data together
#    10. ACS & model data: create county and state name fields  
#    11. ACS & model data: set CRS
#    12. ACS & model data: transform %s so all decimals
#    13. ACS & model data: save objects 


# Ways ACS and model data differ
#    1. ACS 'NAME' field includes county name, the word 'county', and state name while model data only has the county name, sans the word 'county'
#    2.
#    3.
#    4.
#    5.
#    6.
#    7.
######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse, geojsonsf, geojsonio, tidyr, plotly)

#set mapbox token if needed - https://docs.mapbox.com/help/glossary/access-token/
#Sys.setenv("MAPBOX_TOKEN" = "insert token here")

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
#setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

#contextual data & associated data dictionary
contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-data-4December2019.rds")
contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata-11feb2020.rds")

#perinatal model data & associated data dictionary
model_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-data-11feb2020.rds")
model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata-11feb2020.rds")

#matrix of variables x years with data source year as cell value -- to be used to dynamically plug year of data source into captioning
mapyears_data <- paste0(data_repo, "/app_inputs/pre_processed_inputs/map-years-4December2019.rds")

#base spatial layer -------------------------------------------------------------------------------------------------NTS:: why do i need this in here?
base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")

######################################################################################################
# -------------------------------------- functions ------------------------------------------------- #
######################################################################################################
null_nan_miss <- function(dt){
  # objective: function that prints out null, nan and missing vals for value of interest
  # inputs:
  #  REQUIRED:
  #       dt: data table -- expecting wide format
  # output:    report of null, nan and missing values

  # Assess nulls
  nas.vals <- dt[, lapply(.SD, function(x) length(x[is.na(x)])), .SD]
  nas.vals.long <- suppressWarnings(as.data.table(melt(nas.vals,
                                                       variable.name = "fieldname",
                                                       value.name = ("null_ct")))
  )

  # Assess nans
  nan.vals <- dt[, lapply(.SD, function(x) length(x[is.na(x)])), .SD]
  nan.vals.long <- suppressWarnings(as.data.table(melt(nan.vals,
                                                       variable.name = "fieldname",
                                                       value.name = ("nan_ct")))
  )

  # Assess missingness
  miss.vals <- dt[, lapply(.SD, function(x) length(x[unlist(x == "")])), .SD]
  miss.vals.long <- suppressWarnings(as.data.table(melt(miss.vals,
                                                        variable.name = "fieldname",
                                                        value.name = ("miss_ct")))
  )

  #rowbind together & output as one summary
  summary <- merge(nas.vals.long, nan.vals.long)
  summary <- merge(summary, miss.vals.long)

  #create new field to indicate if nas/nans/missings same count across
  summary[, unequal.counts:=0]
  summary[(null_ct != nan_ct | null_ct != miss_ct | nan_ct != miss_ct), unequal.counts := 1]

}

check_state <- function(data, state_id){
  # Function to check out a specific state
  #   Args to define:
  #                   data      = sf object containing data and a field called "state_geoid"
  #                   state_id  = character string of 2-digit state geoid
  #subset to state of interest
  state.data <- data %>%
    filter(state_geoid == (state_id))
  
  #how many counties?
  print(paste0("Your selected state has: ",length(unique(state.data$GEOID)), " counties in this dataset."))
  
  #plot
  plot_mapbox(state.data, split=~NAME.y) %>%
    layout(
      mapbox = list(
        zoom = 6
      )
    ) %>%
    config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
  
}

######################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
#load contextual data
acs <- readRDS(contextual_data)
#load meta data
cdd <- as.data.table(readRDS(contextual_dd))

#load perinatal outcome model data
mod <- readRDS(model_data)
#load meta data
mdd <- as.data.table(readRDS(model_dd))

#load matrix of vars with data source year
mapyears <- as.data.table(readRDS(mapyears_data))

#load state fips codes to get state name in model data --------------------------------------------------------------NTS:: use of this file might be part of issue
base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")


#load 2017 US counties spatial layer
basegeo <- st_read(base)

######################################################################################################
# -------------------------------------- 1. create state geoid ----------------------- #
######################################################################################################
# create state geoid for original data too for validation
acs$state_geoid <- substr(acs$GEOID, 0,2)
mod$state_geoid <- substr(mod$GEOID, 0,2)

######################################################################################################
# -------------------------------------- 2. validate input data --------------------------------------- #
######################################################################################################
#verify same states covered 
if ((length(unique(acs$state_geoid)) == length(unique(mod$state_geoid)))){
  message("Your ACS and model data have the same number of states")
} else {
  message("Eeeks. Your ACS and model data DO NOT have the same number of states")
}
acs.states <- unique(acs$state_geoid)
mod.states <- unique(mod$state_geoid)
acs.overlap <- length(acs.states[acs.states %in% mod.states])
mod.overlap <- length(mod.states[mod.states %in% acs.states])
if ((length(unique(acs$state_geoid)) == acs.overlap) & (length(unique(mod$state_geoid)) == mod.overlap)){
  message("Your ACS and model data cover the same states")
} else {
  message("Eeks. Your ACS and model data DO NOT cover the same states")
}

#verify same counties covered 
if ((length(unique(acs$GEOID)) == length(unique(mod$GEOID)))){
  message("Your ACS and model data have the same number of counties")
} else {
  message("Eeks. Your ACS and model data DO NOT have the same number of counties")
}
acs.counties <- unique(acs$GEOID)
mod.counties <- unique(mod$GEOID)
acs.county.overlap <- length(acs.counties[acs.counties %in% mod.counties])
mod.county.overlap <- length(mod.counties[mod.counties %in% acs.counties])
if ((length(unique(acs$GEOID)) == acs.county.overlap) & (length(unique(mod$GEOID)) == mod.county.overlap)){
  message("Your ACS and model data cover the same counties")
} else {
  message("Eeeks. Your ACS and model data DO NOT cover the same counties")
}


######################################################################################################
# -------------------------------------- 3. Metadata: get rid of trailing whitespace in var display names ---------------------------------------- #
######################################################################################################
#get rid of trailing whitespace in var display names
cdd$display_name <- trimws(cdd$display_name)
mdd$display_name <- trimws(mdd$display_name)

######################################################################################################
# -------------------------------------- 4. Metadata: create fields in model metadata that are present in contextual metadata ---------------------------------------- #
######################################################################################################
# ---- create fields in model metadata that are present in contextual metadata so fields and dimensions match
# what fields missing?
names(cdd)[!names(cdd) %in% names(mdd)] # category_name, plot_type, reverse_palette, data_source

#create plot_type & reverse_palette field in model metadata to match dimensions to context metadata
mdd <- mdd[, reverse_palette:=FALSE]
mdd <- mdd[palette == "BuPu",plot_type:="sequential"]
mdd <- mdd[palette == "PRGn",plot_type:="divergent"]

#create data_source field
mdd <- mdd[, data_source:="Modeled data"]

# ---- create category_name field
# i. define categories using display_names
m_ptb <- c("Preterm birth, Total",
           "Preterm birth, Black", "Preterm birth, Hispanic", "Preterm birth, White",                               
           "Preterm birth, Black:White Risk ratio", "Preterm birth, Hispanic:White Risk ratio",
           "Preterm birth, Black:White Risk difference", "Preterm birth, Hispanic:White Risk difference" )
m_vptb <- c("Very preterm birth, Total",
            "Very preterm birth, Black", "Very preterm birth, Hispanic", "Very preterm birth, White",
            "Very preterm birth, Black:White Risk ratio", "Very preterm birth, Hispanic:White Risk ratio", 
            "Very preterm birth, Black:White Risk difference", "Very preterm birth, Hispanic:White Risk difference")
m_eptb <- c("Early preterm birth, Total", 
            "Early preterm birth, Black", "Early preterm birth, Hispanic", "Early preterm birth, White", 
            "Early preterm birth, Black:White Risk ratio", "Early preterm birth, Hispanic:White Risk ratio",
            "Early preterm birth, Black:White Risk difference", "Early preterm birth, Hispanic:White Risk difference")
m_lptb <- c("Late preterm birth, Total",
            "Late preterm birth, Black", "Late preterm birth, Hispanic", "Late preterm birth, White",
            "Late preterm birth, Black:White Risk ratio", "Late preterm birth, Hispanic:White Risk ratio",
            "Late preterm birth, Black:White Risk difference", "Late preterm birth, Hispanic:White Risk difference" )
m_etb <- c("Early term birth, Total", 
           "Early term birth, Black", "Early term birth, Hispanic", "Early term birth, White",
           "Early term birth, Black:White Risk ratio", "Early term birth, Hispanic:White Risk ratio",
           "Early term birth, Black:White Risk difference","Early term birth, Hispanic:White Risk difference")

# ii. check that all vars assigned a category
m_veclength <-sum(length(m_ptb), length(m_vptb), length(m_eptb), length(m_lptb), length(m_etb))
m_vars <- c(m_ptb, m_vptb, m_eptb, m_lptb, m_etb)
if(length(unique(mdd$display_name)) == m_veclength){
  message("woohoo! all the number of model vars match those assigned groups")
} else {
  message("erm...the number of model vars does not match those assigned groups...try again.")
}
if(length(m_vars[m_vars %in% mdd$display_name]) == length(unique(mdd$display_name))){
  message("woohoo! all model vars have a group assigned")
} else {
  message("erm...you didn't assign a group to all your model vars...try again.")
}

# iii. Create and assign category var
mdd <- mdd[display_name %in% m_ptb, category_name:="Preterm birth"]
mdd <- mdd[display_name %in% m_vptb, category_name:="Very preterm birth"]
mdd <- mdd[display_name %in% m_eptb, category_name:="Early preterm birth"]
mdd <- mdd[display_name %in% m_lptb, category_name:="Late preterm birth"]
mdd <- mdd[display_name %in% m_etb, category_name:="Early term birth"] 

######################################################################################################
# -------------------------------------- 5. Metadata: define MCD palette in contextual metadata ---- #
######################################################################################################
# currently palette == 'custom', so need to define here
cdd <- cdd[varname == "MCD", palette := "#CDBCEC + #FFAD81 + #FF772E + #E8001F"]

######################################################################################################
# -------------------------------------- 6. mapyears: wrangle -------------------------------------- #
######################################################################################################
# ---- wrangling map-years ----
#fix columns named 2015.1 and 2015.2 to read as 2016 and 2017, respectively
setnames(mapyears, c("X2015.1", "X2015.2"), c("X2016","X2017"))

#remove X from col names
mapyrs_newnames <- as.character(seq(2007,2017,1))
mapyrs_oldnames <- names(mapyears)[!names(mapyears) %in% c("X", "varname")]
setnames(mapyears, mapyrs_oldnames, mapyrs_newnames)

#join to contextual metadata to get display name -- this is what is used for matching in the app
mapyears2 <- merge(mapyears, cdd, by = "varname")

# -- reshape long -- 
#subset to only needed columns
mapyears3 <- mapyears2[,c("display_name", mapyrs_newnames), with=FALSE]

#reshape long
mapyears4 <- gather(mapyears3, key = "viz_year", value = "source_year", -display_name)

#convert visualization year (the year users can select) to integer
mapyears4$viz_year <- as.integer(mapyears4$viz_year)

######################################################################################################
# -------------------------------------- 7. ACS & model metadata: create 'label' field for in-app unit labeling -------------------------------------- #
######################################################################################################
# ---- Contextual metadata variable unit labels
#create groupings
nolabel.vars <- c('MCD', 'RUCA')
num.vars <- c('MDrate', 'OBGYNrate', 'rtMHPRACT', 'rtTEENBIRTH', 'BWPOVRR', 'HWPOVRR', 'BWMEDINCOMERR',
              'HWMEDINCOMERR', 'NDI_scale', 'INCOME_8020', 'INCOME_8020_BLK', 'INCOME_8020_WHT',
              'INCOME_8020_HISP', 'ICE_INCOME_all', 'rtSocASSOC', 'SocCap', 'rtVIOLENTCR_ICPSR',
              'race_entropy', 'rtOPIOIDPRESCRIPT', 'rtDRUGPOISON'
)
percent.vars <- c('PCTFEMUNINSURED', 'pctOBESE', 'pctFAIRPOORHLTH', 'pctSPHH', 'pctFHH', 'pctHOUSE_DISTRESS',
                  'pctOVERCROWDHH', 'pctMOVE', 'pctNOHS', 'pctNOHS_BLK', 'pctNOHS_HISP', 'pctNOHS_NHWHT',
                  'pctCOLL', 'pctCOLL_BLK', 'pctCOLL_HISP', 'pctCOLL_NHWHT', 'pctPOV', 'pctPOV_BLK',
                  'pctPOV_HISP', 'pctPOV_NHWHT', 'pctOWNER_OCC', 'pctOWNER_OCC_BLK', 'pctOWNER_OCC_HISP',
                  'pctOWNER_OCC_NHWHT', 'pctPUBLICASST', 'pctBLK', 'pctHISP'
)
dollars.vars <- c('MEDHHINC','MEDHHINC_BLK', 'MEDHHINC_HISP', 'MEDHHINC_NHWHT')

# check that all vars assigned a label
acs.label_veclength <-sum(length(nolabel.vars), length(num.vars), length(percent.vars), length(dollars.vars))
acs.label_vars <- c(nolabel.vars, num.vars, percent.vars, dollars.vars)
if(length(unique(cdd$varname)) == acs.label_veclength){
  message("woohoo! all the number of contextual vars match those assigned labels")
} else {
  message("erm...the number of contextual vars does not match those assigned labels...try again.")
}
if(length(acs.label_vars[acs.label_vars %in% cdd$varname]) == length(unique(cdd$varname))){
  message("woohoo! all contextual vars have a label assigned")
} else {
  message("erm...you didn't assign a label to all your contextual vars...try again.")
}

#create label var and define using categories above
cdd <- cdd[varname %in% num.vars, label:='number']
cdd <- cdd[varname %in% percent.vars, label:='percent']
cdd <- cdd[varname %in% dollars.vars, label:='dollars']
cdd <- cdd[varname %in% nolabel.vars, label := 'nolabel']

# ---- Model metadata variable unit labels
mdd <- mdd[!str_detect(display_name, "ratio"), label:= 'percent']
mdd <- mdd[str_detect(display_name, "ratio"), label:= 'nolabel']

######################################################################################################
# ------------------------------------- 8. ACS & model data: rename data fields to their human-readable versions (display names) ------- #
######################################################################################################
#create vector of field names
acsoriginal <- names(acs)
modoriginal <- names(mod)

#enumerate field names vector and place in dt
acsnamesdt <- as.data.table(acsoriginal)
acsnamesdt <- acsnamesdt[,id:=row.names(acsnamesdt)]

modnamesdt <- as.data.table(modoriginal)
modnamesdt <- modnamesdt[,id:=row.names(modnamesdt)]

#join to meta data dictionary
acsnamesdt2 <- as.data.table(left_join(acsnamesdt, cdd, by=c("acsoriginal" = "varname")))
modnamesdt2 <- as.data.table(left_join(modnamesdt, mdd, by=c("modoriginal" = "varname")))

#create single new field for names
acsnamesdt2 <- acsnamesdt2[,newname:=display_name]
acsnamesdt2 <- acsnamesdt2[is.na(acsnamesdt2$display_name),newname:=acsoriginal]

modnamesdt2 <- modnamesdt2[,newname:=display_name]
modnamesdt2 <- modnamesdt2[is.na(modnamesdt2$display_name),newname:=modoriginal]

#get new vectors using 'newname' field
acsnewnames <- acsnamesdt2[["newname"]]
modnewnames <- modnamesdt2[["newname"]]

#rename vars 
acs2 <- setnames(acs, acsoriginal, acsnewnames)
mod2 <- setnames(mod, modoriginal, modnewnames)

######################################################################################################
# ------------------------------------- 9. ACS & model data: join data together -------------------- #
######################################################################################################
#join input datasets
modjoin <- copy(mod2)
modjoin$geom <- NULL
alldata <- full_join(acs2, modjoin, by = c('GEOID', 'year', 'state_geoid'))

#validate data behaving as should
check_state(alldata, "10")
check_state(alldata, "01")

######################################################################################################
# ------------------------------------- 10. ACS & model data: create county and state name fields ------- #
######################################################################################################
#create county name field
alldata$county_name <- alldata$NAME.y

#create state name field
alldata$state_name <- gsub('.*,\\s*', '', alldata$NAME.x)

#validate dataset still works as it should
check_state(alldata, "10")
check_state(alldata, "01")

######################################################################################################
# ------------------------------------- 11. ACS & model data: set CRS ------------------------------ #
######################################################################################################
alldatafin <- st_transform(alldata, crs = 4326)

#validate dataset still works as it should
check_state(alldatafin, "10")
check_state(alldatafin, "01")

######################################################################################################
# ------------------------------------- 12. ACS & model data: transform %s so all decimals --------- #
######################################################################################################
convert2decimals <- c("Teen birth rate","Percent women uninsured", "% Obese (2010-2015 only)",
                      "% Fair/Poor health (2010-2015 only)", "% Housing distress (2014-2015 only)")

for (c in 1:length(convert2decimals)){
  convertcol <- convert2decimals[c]
  alldatafin[[convertcol]] <- (alldatafin[[convertcol]]/100)
}

######################################################################################################
# ------------------------------------- 13. ACS & model data: save objects ------------------------- #
######################################################################################################
#validate dataset still works as it should
check_state(alldatafin, "10")
check_state(alldatafin, "01")

#save meta data
saveRDS(cdd, file = paste0(data_repo,"/app_inputs/contextual-metadata-20feb20.rds"))
saveRDS(mdd, file = paste0(data_repo,"/app_inputs/perinatal-metadata-20feb20.rds"))

#save map-years data
saveRDS(mapyears4, file = paste0(data_repo,"/app_inputs/mapyears-20feb20.rds"))

#save app input dataset
saveRDS(alldatafin, file = paste0(data_repo,"/app_inputs/all-data-20feb20.rds"))
