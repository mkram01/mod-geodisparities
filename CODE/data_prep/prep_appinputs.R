############################################
# code author: erin stearns
# script objective: prep new app inputs - contextual vars & model outcomes
# date: 3 november 2019
###########################################

rm(list = ls())

######################################################################################################
# -------------------------------------- set up environment ---------------------------------------- #
######################################################################################################
#load packages
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse)

# set code repo
repo <- Sys.getenv('mod_repo')
# set data repo
data_repo <- Sys.getenv('mod_data')

#setting working directory
#setwd(repo)
message(paste0("You have specified ", data_repo, " as the location of your data."))

#TO-DO -- define app input data for preparation to be pulled into app
#contextual data & associated data dictionary
contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-data-4november2019.rds")
contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata.rds")

#perinatal model data & associated data dictionary
model_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-data-2november2019.rds")
model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata.rds")

#base spatial layer
#base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")

######################################################################################################
# -------------------------------------- load data ------------------------------------------------- #
######################################################################################################
#load contextual data
acs <- readRDS(contextual_data)
#load meta data 
cdd <- readRDS(contextual_dd)

#load perinatal outcome model data
mod <- readRDS(model_data)
#load meta data
mdd <- readRDS(model_dd)

#load state fips codes to get state name in model data
states <- fread(paste0(data_repo,"/app_inputs/pre_processed_inputs/state_fips_codes.csv"), stringsAsFactors = F,
                colClasses = list(character=c("state_name", "fips_code")))

#load 2017 US counties spatial layer
#basegeo <- st_read(base)

######################################################################################################
# -------------------------------------- basic data cleanup ---------------------------------------- #
######################################################################################################
#remove rows with NA 'NAME' values

# change certain fields into particular classes
#context_spatial$`Maternity care access (2016)` <- as.character(context_spatial$`Maternity care access (2016)`)

#get rid of trailing whitespace in var display names
cdd$display_name <- trimws(cdd$display_name)
mdd$display_name <- trimws(mdd$display_name)

# -------------------------------------- create grouping var for dropdown menu -----------------------
cdd_dt <- as.data.table(cdd)
mdd_dt <- as.data.table(mdd)

# ---- contextual var groupings ----
c_access <- c("Maternity care access (2016)", "Physicians per capita",
              "OBYNS per capita", "Mental health providers per capita (2007-2015 only)",
              "Percent women uninsured")
c_health <- c("Teen birth rate", "% Obese (2010-2015 only)",
              "% Fair/Poor health (2010-2015 only)",
              "Opioid prescription rate (2007-2015 only)", 
              "Drug poisoning mortality rate (2007-2015 only)" 
              )
c_hh <- c("% Single parent households", "% Female headed household",
          "% Housing overcrowding"
          )
c_edu <- c("% No high school", 
           "% No high school, Black", "% No high school, Hispanic", "% No high school, White",
           "% College, Black", "% College, Hispanic", "% College, White"
           )
c_poverty <- c("% Poverty", 
               "% Poverty, Black", "% Poverty, Hispanic", "% Poverty, White",
               "Black-White disparity in poverty rate"
               )
c_income <- c("Median household income, Black", "Median household income, Hispanic",
              "Median household income, non-Hispanic White",
              "White-Black disparity in median HH income",
              "Income inequality",
              "Income inequality, Black", "Income inequality, White")
c_socsupport <- c("% Receiving public assistance", "Neighbor Deprivation Index",
                  "Index of concentration at extremes (ICE)", 
                  "Social associations per capita (2007-2015 only)",    
                  "Social Capital Index",
                  "Rural-Urban")
c_soccapital <- c("% Recently moved", "% Owner occupied housing",
                  "% Black owner occupied housing", "% Hispanic owner occupied housing", 
                  "% White owner occupied housing", 
                  "% Housing distress (2014-2015 only)",
                  "Violent crime rate (2007-2014 only)",
                  "Racial diversity (entropy; 2010)",
                  "% Black", "% Hispanic" 
                  )

#check that all vars assigned a group
c_veclength <-sum(length(c_access), length(c_health), length(c_hh), length(c_edu), 
                  length(c_poverty), length(c_income), length(c_socsupport), length(c_soccapital)
                  )
if(length(unique(cdd_dt$display_name)) == c_veclength){
  message("woohoo! all vars have a group assigned")
} else {
  message("erm...you didn't assign a group to all your vars...try again.")
}

#create & assign group var
cdd_dt <- cdd_dt[display_name %in% c_access, group:="Access to Care"]
cdd_dt <- cdd_dt[display_name %in% c_health, group:="Health"]
cdd_dt <- cdd_dt[display_name %in% c_hh, group:="Household"]
cdd_dt <- cdd_dt[display_name %in% c_edu, group:="Education"]
cdd_dt <- cdd_dt[display_name %in% c_poverty, group:="Poverty"]
cdd_dt <- cdd_dt[display_name %in% c_income, group:="Income"]
cdd_dt <- cdd_dt[display_name %in% c_socsupport, group:="Social support"]
cdd_dt <- cdd_dt[display_name %in% c_soccapital, group:="Social Capital"]

# ---- model var groupings ----
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

#check that all vars assigned a group
m_veclength <-sum(length(m_ptb), length(m_vptb), length(m_eptb), length(m_lptb), length(m_eptb))
if(length(unique(mdd_dt$display_name)) == m_veclength){
  message("woohoo! all model vars have a group assigned")
} else {
  message("erm...you didn't assign a group to all your model vars...try again.")
}

#create & assign group var
mdd_dt <- mdd_dt[display_name %in% m_ptb, group:="Preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_vptb, group:="Very preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_eptb, group:="Early preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_lptb, group:="Late preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_etb, group:="Early term birth"]

######################################################################################################
# -------------------------------------- Rename data fields to their human-readable versions ------- #
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
# -------------------------------------- create county and state name fields ----------------------- #
######################################################################################################
#copy
acs3 <- copy(acs2)
#create county name field
acs3$county_name <- gsub(" .*$", "", acs3$NAME)
#create state name field
acs3$state_name <- gsub('.*,\\s*', '', acs3$NAME)

#copy
mod3 <- copy(mod2)
#create county field in model data
mod3$county_name <- mod3$NAME
#create state fips
mod3$state_fips <- substr(mod3$GEOID, start = 1, stop = 2)
#join to state fips key
mod4 <- left_join(mod3, states, by = c("state_fips" = "fips_code"))

#drop puerto rico
acs4 <- (acs3[acs3$state_name != "Puerto Rico",])
mod5 <- (mod4[mod4$state_name != "Puerto Rico",])

#rework cols to keep
acs5 <- acs4[,c(1:2, 53, 54, 4:52)] #GEOID, year, county_name, state_name, ..., geom
mod6 <- mod5[,c(1, 3, 45, 47, 4:44)] #GEOID, year, county_name, state_name, ..., geom

#join together
mod6dt <- as.data.frame(mod6)
mod6dt$geom <- NULL
allin1 <- left_join(acs5, mod6dt)

######################################################################################################
# ------------------------------------------ simplify geometry ------------------------------------- #
######################################################################################################
#transform to Albers Equal Area projection
aeap <- "+proj=aea +lat_1=29.83333333333334 +lat_2=45.83333333333334 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#transform to projected CRS and try simplifying
# acs6 <- st_transform(acs5, aeap)
# mod7 <- st_transform(mod6, aeap)
all2 <- st_transform(allin1, aeap)

#geometry simplification
#acs_smpl <- st_simplify(acs6, dTolerance = 0.1, preserveTopology = T)
#mod_smpl <- st_simplify(mod7, dTolerance = 0.1, preserveTopology = T)
all_smpl <- st_simplify(all2, dTolerance = 0.1, preserveTopology = T)

######################################################################################################
# ------------------------------------------ set CRS ----------------------------------------------- #
######################################################################################################
#transform to wgs84
#acs_fin <- st_transform(acs_smpl, crs = 4326)
#mod_fin <- st_transform(mod_smpl, crs = 4326)
all_fin <- st_transform(all_smpl, crs = 4326)

#test
#plot(st_geometry(acs_fin[acs_fin$state_name == "Maine" & acs_fin$year=="2008",])) 
#plot(st_geometry(mod_fin[mod_fin$state_name == "Georgia" & mod_fin$year=="2008",]))
plot(st_geometry(all_fin[all_fin$state_name == "Maine" & all_fin$year=="2008",]))

# #need to turn geom into polygon geometry column -- may not need to do this  
# contextdata <- as.data.frame(acs_fin)
# contextgeom <- st_sfc(acs_fin$geom)
# context_sf <- st_sf(contextdata, geometry = contextgeom)
# 
# modeldata <- as.data.frame(mod_fin)
# modelgeom <- st_sfc(mod_fin$geom)
# model_sf <- st_sf(modeldata, geometry = modelgeom)
# 
# #drop geom columns
# context_sf$geom <- NULL
# model_sf$geom <- NULL

#need to change the sfc class to multipolygon to enable plotly plotting
#context_sf <- st_cast(acs_fin, "MULTIPOLYGON")
#mod_sf <- st_cast(mod_fin, "MULTIPOLYGON")
all_sf <- st_cast(all_fin, "MULTIPOLYGON")

#test
#class(st_geometry(context_sf))
#class(st_geometry(mod_sf))
class(st_geometry(all_sf))
######################################################################################################
# -------------------------------------- save spatial data objects ------------------------------- #
######################################################################################################
#save meta data
saveRDS(cdd, file = paste0(data_repo,"/app_inputs/contextual-metadata-9november2019.rds"))
saveRDS(mdd, file = paste0(data_repo,"/app_inputs/perinatal-metadata-9november2019.rds"))

#save app inputs
#saveRDS(context_sf, file = paste0(data_repo,"/app_inputs/contextual-data-5november2019.rds"))
#saveRDS(mod_sf, file = paste0(data_repo,"/app_inputs/perinatal-data-5november2019.rds"))
saveRDS(all_sf, file = paste0(data_repo,"/app_inputs/all-data-9november2019.rds"))


######################################################################################################
# ---------------------------------------------- SCRAP --------------------------------------------- #
######################################################################################################
# ------ make sure geographies & years overlap -- 11/3 same length ------
#acsgeos <- unique(acs2$GEOID) 
#modgeos <- unique(mod2$GEOID)
#mod2 <- mod2[,mod_name:=NAME]
#join data by geoid & year to eliminate mismatches & subset data accordingly
#acsgeoyr <- acs2[,c("GEOID", "year", "NAME")]
#modgeoyr <- mod2[,c("GEOID", "year", "mod_name")]
#join
# acsmod <- left_join(acsgeoyr, modgeoyr)
# #remove all rows where county_name is null
# acsmod2 <- acsmod[!is.na(acsmod$mod_name),]
# #create a keep dt to join back to each and filter
# keeprows <- acsmod2[,c("GEOID", "year")]
# keeprows$keep <- 1
# #join back to each table
# acsfiltered <- as.data.table(left_join(acs2, keeprows))
# acsfiltered2 <- acsfiltered[!is.na(acsfiltered$keep),]
# 
# modfiltered <- as.data.table(left_join(mod2, keeprows))
# modfiltered2 <- modfiltered[!is.na(modfiltered$keep),]
