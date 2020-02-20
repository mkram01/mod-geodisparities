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
pacman::p_load(data.table,sf,dplyr,rgeos, tmap, tidyverse, geojsonsf, geojsonio, tidyr)

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

# #TO-DO -- define app input data for preparation to be pulled into app
# #contextual data & associated data dictionary
# contextual_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-data-4December2019.rds")
# #contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata.rds")
# contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata-4December2019.rds")
# #output metadata as csv and added label field 
# #contextual_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata-17November.csv")
# 
# #perinatal model data & associated data dictionary
# model_data <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-data-2november2019.rds")
# #model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata.rds")
# model_dd <- paste0(data_repo,"/app_inputs/pre_processed_inputs/perinatal-metadata.csv")
# 
# #matrix of variables x years with data source year as cell value -- to be used to dynamically plug year of data source into captioning
# mapyears_data <- paste0(data_repo, "/app_inputs/pre_processed_inputs/map-years-4December2019.rds")
# 
# #base spatial layer
# base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")

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

#load matrix of vars with data source year
mapyears <- as.data.table(readRDS(mapyears_data))

#load state fips codes to get state name in model data --------------------------------------------------------------NTS:: use of this file might be part of issue
base <- paste0(data_repo,"/app_inputs/pre_processed_inputs/us_counties_2017.gpkg")
states <- fread(paste0(data_repo,"/app_inputs/pre_processed_inputs/state_fips_codes.csv"), stringsAsFactors = F,
                colClasses = list(character=c("state_name", "fips_code")))

#load 2017 US counties spatial layer
basegeo <- st_read(base)
# 
# #load contextual data
# acs <- readRDS(contextual_data)
# #load meta data 
# cdd <- readRDS(contextual_dd)
# #cdd <- fread(contextual_dd, stringsAsFactors = F)
# 
# #newcdd <- readRDS(newestcontext)
# 
# #for display
# #write.csv(newcdd, file = paste0(data_repo,"/app_inputs/pre_processed_inputs/contextual-metadata-17November.csv"))
# 
# #load perinatal outcome model data
# mod <- readRDS(model_data)
# #load meta data
# #mdd <- readRDS(model_dd)
# mdd <- fread(model_dd, stringsAsFactors = F)
# 
# #load matrix ov vars with data source year
# mapyears <- as.data.table(readRDS(mapyears_data))
# 
# #load state fips codes to get state name in model data
# states <- fread(paste0(data_repo,"/app_inputs/pre_processed_inputs/state_fips_codes.csv"), stringsAsFactors = F,
#                 colClasses = list(character=c("state_name", "fips_code")))
# 
# #load 2017 US counties spatial layer
# basegeo <- st_read(base)

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

#create plot_type & reverse_palette field in model metadata to match dimensions to context metadata
mdd_dt <- mdd_dt[, reverse_palette:=FALSE]
mdd_dt <- mdd_dt[palette == "BuPu",plot_type:="sequential"]
mdd_dt <- mdd_dt[palette == "PRGn",plot_type:="divergent"]
#change this once hear back from Michael about meaning
cdd_dt <- cdd_dt[varname == "MCD", palette := "#CDBCEC + #FFAD81 + #FF772E + #E8001F"]

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
mdd_dt <- mdd_dt[display_name %in% m_ptb, category_name:="Preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_vptb, category_name:="Very preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_eptb, category_name:="Early preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_lptb, category_name:="Late preterm birth"]
mdd_dt <- mdd_dt[display_name %in% m_etb, category_name:="Early term birth"] 

# ---- wrangling map-years ----
#fix columns named 2015.1 and 2015.2 to read as 2016 and 2017, respectively
setnames(mapyears, c("X2015.1", "X2015.2"), c("X2016","X2017"))
mapyears <- mapyears[,"X2016":="X2015.1"]
mapyears <- mapyears[,"X2017":="X2015.2"]

#remove X from col names
mapyrs_newnames <- as.character(seq(2007,2017,1))
mapyrs_oldnames <- names(mapyears)[!names(mapyears) %in% c("X", "varname")]
setnames(mapyears, mapyrs_oldnames, mapyrs_newnames)

#join to contextual metadata to get display name -- this is what will be used for matching in the app
mapyears2 <- merge(mapyears, cdd, by = "varname")

# -- reshape long -- 
#subset to only needed columns
mapyears3 <- mapyears2[,c("display_name", mapyrs_newnames), with=FALSE]

#reshape long
mapyears4 <- gather(mapyears3, key = "viz_year", value = "source_year", -display_name)

#convert viz year to integer
mapyears4$viz_year <- as.integer(mapyears4$viz_year)

# -------------------------------------- create 'label' field for in-app unit labeling ---------------
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

#create label var and define using categories above
cdd_dt <- cdd_dt[varname %in% num.vars, label:='number']
cdd_dt <- cdd_dt[varname %in% percent.vars, label:='percent']
cdd_dt <- cdd_dt[varname %in% dollars.vars, label:='dollars']
cdd_dt <- cdd_dt[varname %in% nolabel.vars, label := NA]

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
acsnamesdt2 <- as.data.table(left_join(acsnamesdt, cdd_dt, by=c("acsoriginal" = "varname")))
modnamesdt2 <- as.data.table(left_join(modnamesdt, mdd_dt, by=c("modoriginal" = "varname")))

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


#create state code
acs2$state_fips <- substr(acs2$GEOID, 0,2)
mod2$state_fips <- substr(mod2$GEOID, 0,2)
basegeo$state_fips <- substr(basegeo$GEOID, 0,2)

#delaware
de.acs <- acs2[acs2$state_fips == "10",]
de.mod <- mod2[mod2$state_fips == "10",]
de.basegeo <- basegeo[basegeo$state_fips == "10",]

#how many counties?
unique(de.acs$GEOID) #3
unique(de.mod$GEOID) #3
unique(de.basegeo$GEOID) #3

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
acs5 <- acs4[,c(1:2, 57, 58, 4:56)] #GEOID, year, county_name, state_name, ..., geom
mod6 <- mod5[,c(1, 3, 45, 47, 4:44)] #GEOID, year, county_name, state_name, ..., geom

#join together
# -- convert mod data to dataframe
mod6dt <- as.data.frame(mod6)
mod6dt$geom <- NULL

# -- convert acs data to dataframe
acs5dt <- as.data.frame(acs5)
acs5dt$geom <- NULL

# -- create simple sf object to join to -- check against original base layer


# -- join

allin1 <- left_join(acs5, mod6dt)



#create state code
allin1$state_fips <- substr(allin1$GEOID, 0,2)

#delaware
de.all <- allin1[allin1$state_fips == "10",]

#how many counties?
unique(de.all$GEOID) #3
unique(de.basegeo$GEOID) #3
######################################################################################################
# ------------------------------------------ simplify geometry ------------------------------------- #
######################################################################################################
#transform to Albers Equal Area projection
#aeap <- "+proj=aea +lat_1=29.83333333333334 +lat_2=45.83333333333334 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#transform to projected CRS and try simplifying
# acs6 <- st_transform(acs5, aeap)
# mod7 <- st_transform(mod6, aeap)

#all2 <- st_transform(allin1, aeap)

#geometry simplification
#acs_smpl <- st_simplify(acs6, dTolerance = 0.1, preserveTopology = T)
#mod_smpl <- st_simplify(mod7, dTolerance = 0.1, preserveTopology = T)
#all_smpl <- st_simplify(all2, dTolerance = 0.1, preserveTopology = T)

######################################################################################################
# ------------------------------------------ set CRS ----------------------------------------------- #
######################################################################################################
#transform to wgs84
#acs_fin <- st_transform(acs_smpl, crs = 4326)
#mod_fin <- st_transform(mod_smpl, crs = 4326)
all_fin <- st_transform(allin1, crs = 4326)

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

#convert to geojson
#alljson <- sf_geojson(all_sf, options(encoding = "utf8"))

######################################################################################################
# ------------------------------------------ transform %s so all decimals -------------------------- #
######################################################################################################
convert2decimals <- c("Percent women uninsured", "% Obese (2010-2015 only)", 
                      "% Fair/Poor health (2010-2015 only)", "% Housing distress (2014-2015 only)")

for (c in 1:length(convert2decimals)){
  convertcol <- convert2decimals[c]
  all_sf[[convertcol]] <- (all_sf[[convertcol]]/100)
}

######################################################################################################
# ------------------------------------------ remove context meta vars not in data ------------------ #
######################################################################################################
# cdd_dt2 <- cdd_dt[!display_name %in% c("% College", "Hispanic-Black disparity in poverty rate",
#                                        "White-Hispanic disparity in median HH income",
#                                        "Income inequality, Hispanic"),]

######################################################################################################
# ------------------------------------------ remove vars causing strange issues ------------------ #
######################################################################################################
# #problem vars
probvars <- c("Teen birth rate", "% Obese (2010-2015 only)", "% Fair/Poor health (2010-2015 only)")

#remove from app data
all_sf2 <- all_sf[!names(all_sf) %in% probvars]

#remove from context meta data
cdd_dt2 <- cdd_dt[!display_name %in% probvars]

######################################################################################################
# ------------------------------------------ add some validation code here ------------------------- #
######################################################################################################
#make sure number of vars in data match number of vars between meta data
#if dropping problem vars
datavarnum <- ncol(all_sf2) - 5
metavarnum <- sum(nrow(cdd_dt2), nrow(mdd_dt))
if(datavarnum == metavarnum){
  message("Awesome. You have the expected number of vars in your data and meta data")
} else {
  message("Eeeks. You do not have the same number of vars in your meta data and input data -- better check again,
          otherwise the app will have issues")
}

#not dropping
# datavarnum <- ncol(all_sf) - 5
# metavarnum <- sum(nrow(cdd_dt), nrow(mdd_dt))
# if(datavarnum == metavarnum){
#   message("Awesome. You have the expected number of vars in your data and meta data")
# } else {
#   message("Eeeks. You do not have the same number of vars in your meta data and input data -- better check again, 
#           otherwise the app will have issues")
# }
######################################################################################################
# -------------------------------------- save spatial data objects ------------------------------- #
######################################################################################################
#save meta data
#not removing problem vars
#saveRDS(cdd_dt, file = paste0(data_repo,"/app_inputs/contextual-metadata-3december2019.rds"))
#if removing problem vars
#********saveRDS(cdd_dt2, file = paste0(data_repo,"/app_inputs/contextual-metadata-23december2019.rds"))
#********saveRDS(mdd_dt, file = paste0(data_repo,"/app_inputs/perinatal-metadata-23december2019.rds"))

#save map-years data
#********saveRDS(mapyears4, file = paste0(data_repo,"/app_inputs/mapyears-23december2019.rds"))

#save app inputs
#saveRDS(context_sf, file = paste0(data_repo,"/app_inputs/contextual-data-5november2019.rds"))
#saveRDS(mod_sf, file = paste0(data_repo,"/app_inputs/perinatal-data-5november2019.rds"))
#not removing problem vars
#saveRDS(all_sf, file = paste0(data_repo,"/app_inputs/all-data-3december2019.rds"))
#if removing problem vars
#********saveRDS(all_sf2, file = paste0(data_repo,"/app_inputs/all-data-23december2019.rds"))

#save json
#geojson_write(alljson, file = paste0(data_repo,"/app_inputs/all-data-9november2019.geojson"))



#create state code
all_sf2$state_fips <- substr(all_sf2$GEOID, 0,2)

#delaware
de.allsf <- all_sf2[all_sf2$state_fips == "10",]

#how many counties?
unique(de.allsf$GEOID) #3

######################################################################################################
# ---------------------------------------------- test w/some mapping ------------------------------- #
######################################################################################################
basegeo2 <- st_transform(basegeo, crs = 4326)

plot_mapbox(basegeo2, split=~NAME) %>%
  layout(
    mapbox = list(style = "open-street-map")
  )

leaflet(basegeo2) %>%
  addPolygons()

leaflet(acs5) %>%
  addPolygons()

# able to successfully plot with leaflet
acs6 <- st_transform(acs5, crs = 4326)

pal <- colorBin("YlOrRd", domain = acs6$`% Poverty`, bins = 5)
leaflet(acs6) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(`% Poverty`),
    weight = 2, 
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7
  )
######################################################################################################
# ---------------------------------------------- SCRAP --------------------------------------------- #
######################################################################################################
# ---- contextual var groupings ----
# c_access <- c("Maternity care access (2016)", "Physicians per capita",
#               "OBYNS per capita", "Mental health providers per capita (2007-2015 only)",
#               "Percent women uninsured")
# c_health <- c("Teen birth rate", "% Obese (2010-2015 only)",
#               "% Fair/Poor health (2010-2015 only)",
#               "Opioid prescription rate (2007-2015 only)", 
#               "Drug poisoning mortality rate (2007-2015 only)" 
#               )
# c_hh <- c("% Single parent households", "% Female headed household",
#           "% Housing overcrowding"
#           )
# c_edu <- c("% No high school", 
#            "% No high school, Black", "% No high school, Hispanic", "% No high school, White",
#            "% College, Black", "% College, Hispanic", "% College, White"
#            )
# c_poverty <- c("% Poverty", 
#                "% Poverty, Black", "% Poverty, Hispanic", "% Poverty, White",
#                "Black-White disparity in poverty rate"
#                )
# c_income <- c("Median household income, Black", "Median household income, Hispanic",
#               "Median household income, non-Hispanic White",
#               "White-Black disparity in median HH income",
#               "Income inequality",
#               "Income inequality, Black", "Income inequality, White")
# c_socsupport <- c("% Receiving public assistance", "Neighbor Deprivation Index",
#                   "Index of concentration at extremes (ICE)", 
#                   "Social associations per capita (2007-2015 only)",    
#                   "Social Capital Index",
#                   "Rural-Urban")
# c_soccapital <- c("% Recently moved", "% Owner occupied housing",
#                   "% Black owner occupied housing", "% Hispanic owner occupied housing", 
#                   "% White owner occupied housing", 
#                   "% Housing distress (2014-2015 only)",
#                   "Violent crime rate (2007-2014 only)",
#                   "Racial diversity (entropy; 2010)",
#                   "% Black", "% Hispanic" 
#                   )
# 
# #check that all vars assigned a group
# c_veclength <-sum(length(c_access), length(c_health), length(c_hh), length(c_edu), 
#                   length(c_poverty), length(c_income), length(c_socsupport), length(c_soccapital)
#                   )
# if(length(unique(cdd_dt$display_name)) == c_veclength){
#   message("woohoo! all vars have a group assigned")
# } else {
#   message("erm...you didn't assign a group to all your vars...try again.")
# }
# 
# #create & assign group var
# cdd_dt <- cdd_dt[display_name %in% c_access, group:="Access to Care"]
# cdd_dt <- cdd_dt[display_name %in% c_health, group:="Health"]
# cdd_dt <- cdd_dt[display_name %in% c_hh, group:="Household"]
# cdd_dt <- cdd_dt[display_name %in% c_edu, group:="Education"]
# cdd_dt <- cdd_dt[display_name %in% c_poverty, group:="Poverty"]
# cdd_dt <- cdd_dt[display_name %in% c_income, group:="Income"]
# cdd_dt <- cdd_dt[display_name %in% c_socsupport, group:="Social support"]
# cdd_dt <- cdd_dt[display_name %in% c_soccapital, group:="Social Capital"]


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
