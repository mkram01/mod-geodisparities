# March of Dimes Analysis      #
# 02: Spatial Script           #
# Kevin Weiss                  #
# 02/04/2019                   #


# Data and Package Setup -----------------------------------
rm(list = ls())
library(sp) # for spatial data manipulation
library(spdep) # for adjacency matrix creation
library(tigris) # for TIGRIS county shapefile
library(colorspace) # for HCL color palette
library(grid) # for spplot customization
library(lattice)
library(tidycensus)
library(INLA)
library(ggplot2)
library(dplyr)
library(plyr)
library(sf)
library(mapview)
library(car)
library(maptools)

# Read in Data
model1 <- readRDS(file = "DATA/nchs_births/R/data/model1.rda")
model2 <- readRDS(file = "DATA/nchs_births/R/data/model2.rda")
model3 <- readRDS(file = "DATA/nchs_births/R/data/model3.rda")
model4 <- readRDS(file = "DATA/nchs_births/R/data/model4.rda")
model5 <- readRDS(file = "DATA/nchs_births/R/data/model5.rda")
model6 <- readRDS(file = "DATA/nchs_births/R/data/model6.rda")
gatestmodel1 <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1.rda")
gatestmodel1singleyear <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1singleyear.rda")
model1singleyear <- readRDS(file = "DATA/nchs_births/R/data/model1singleyear.rda")

# Create merged spatial data file -----------------------------------

# Read in county shapefiles (Based on 2010 County Boundaries)
tigris_spdf <- tigris::counties() %>%
  
  # Subset to contiguous for now (temporary)
  subset(!tigris_spdf$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

# Load health data frame
health_df <- model1 # with a "FIPS" column and "Case_Counts" column

# Merge case data with county shapefile
spdf <- tigris::geo_join(tigris_spdf, health_df, "GEOID", "combfips")
# tigris_spdf <- NULL
spdf$births[is.na(spdf$births)] <- 0 # Set NA counties to 0 
spdf$ptb[is.na(spdf$ptb)] <- 0 # Set NA counties to 0
spdf$lptb[is.na(spdf$lptb)] <- 0 # Set NA counties to 0
spdf$mptb[is.na(spdf$mptb)] <- 0 # Set NA counties to 0
spdf$vptb[is.na(spdf$vptb)] <- 0 # Set NA counties to 0

# Albers Coordinate Reference System for United States
cref1 <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +7_0=0 +a=6570997 +b=6370997 +units=m +no_defs"
spdf <- sp::spTransform(spdf, sp::CRS(cref1)) # transform US counties to Albers CRS

# Create ID variable (if not already in SPDF) ---------------
# Alternative: us_co$struct<-1:dim(us_co@data[1])
spdf$idx <- 1:nrow(spdf)

# Extract data.frame from the SDPF
dat_df <- as.data.frame(spdf) 

# Create adjacency matrix ----------------------------------
spdf_adj <- spdep::poly2nb(spdf, queen = T, row.names = spdf$idx) # set queen = F for rook
adj <- spdep::nb2mat(spdf_adj, style = "B", # create binary matrix
                     zero.policy = TRUE)  # added to avoid error
adj <- as(adj, "dgTMatrix") # create sparse matrix, memory conservation

# K-Nearest Neighbors contiguity matrix
# plot(spdf)
# nbs <- knearneigh(coordinates(spdf), k = 4, longlat = T) # k = 4 nearest neighbors
# nbs <- knn2nb(nbs, row.names = spdf$idx, sym = T) # force symmetry!!
# mat <- nb2mat(nbs, style = "B", zero.policy = TRUE)
# colnames(mat) <- rownames(mat) 
# mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
# wts <- nb2listw(nbs)
# plot(wts, coords = coordinates(spdf), add = T, col = 2)
# # Continue with nb2INLA
# nb2INLA("cl_graph",nbs) 
# nb2INLA(file="/Users/ozd504/Google Drive/dem7263/data/us.gra", nbs)
# am_adj <- paste(getwd(),"/cl_graph",sep = "")
# H <- inla.read.graph(filename = "cl_graph")
# image(inla.graph2matrix(H), xlab = "", ylab = "", main = "")

# Another matrix
# nbs<-poly2nb(us_co, queen = T, row.names = us_co$struct)
# mat <- nb2mat(nbs, style="B",zero.policy=TRUE)
# colnames(mat) <- rownames(mat)
# mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
# nb2INLA("am_graph",nbs)
# am_adj <-paste(getwd(),"/am_graph",sep="")
# H<-inla.read.graph(filename="am_graph")
# image(inla.graph2matrix(H), xlab="", ylab="", main="")
# 
# tx<-st_as_sf(us_co)
# tx%>%
#   ggplot()+geom_sf()
# 
# final.dat<-merge( tx,final.dat, by="struct")




# Create expected counts
# Expected cases (raw counts of events relative to some expected value, or population offset)
rates <- aggregate(ptb ~ 1, health_df, mean) #in this case, we will standardize to the average IMR for the period

health_df$E_d <- rates$ptb

health_df <- health_df[order(health_df$combfips, health_df$dob_yy),]
health_df$id <- 1:dim(health_df)[1]

head(health_df)
options(scipen = 999)


# Use scale() to get Z-scores

# # Plot geographies
# us_co <- st_as_sf(tigris_spdf)
# us_co$cofips <- paste(us_co$STATEFP, us_co$COUNTYFP, sep = "")
# us_co %>%
#   ggplot() + geom_sf() + coord_sf(crs = 102008)
# us_co
# 
# final.dat <- merge( us_co,final.dat, by = "cofips")
# final.dat <- final.dat[order(final.dat$cofips, final.dat$year),]


# Census data (for later) ------------------------------------------
# Census pop estimates
# popurl <- url("http://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv")
# pops <- read.csv(popurl)
# names(pops) <- tolower(names(pops))
# pops <- pops %>%
#   mutate(cofips = paste(sprintf(fmt = "%02d", state), sprintf(fmt = "%03d",county), sep = "")) %>%
#   filter(sumlev == 50, !state %in% c(2, 15))
# 
# head(pops)
# 
# # Data prep
# # Reshape pop estimates to long
# pops.long <- reshape(data = pops, idvar = "cofips", 
#                      varying = list(names(pops)[9:16]), 
#                      direction = "long", 
#                      drop = names(pops)[c(2,3,4,5,6,8,17,18,19,20)], 
#                      v.names = "population")
# pops.long$year <- pops.long$time + 1999
# head(pops.long)
# 
# 
# # Merge pop estimates with data frame
# dat.long <- merge(pops.long, df, by.x = c("cofips", "year"), 
#                   by.y = c("cofips", "YEAR"))
# head(dat.long)
# # Output is organized by County by year
# 
# # Get 2000 census data
# # Requires API key now
# cov_dat <- get_decennial(geography = "county", year = 2000, sumfile = "sf3",
#                          summary_var = "P001001",
#                          variables = c("P007003", "P007004","P007010","P053001", "P089001", "P089002" ),
#                          output = "wide")
# 
# cov_dat <- cov_dat %>%
#   mutate(cofips = GEOID,
#          pwhite = P007003/summary_value, 
#          pblack = P007004/summary_value, 
#          phisp = P007010/summary_value,
#          medhhinc = as.numeric(scale(P053001)), 
#          ppov = P089002/P089001)
# msaecon<-data.frame(gini=acsecon@estimate[, "B19083_001"],
# ppoverty=acsecon@estimate[, "B17001_002"]/acsecon@estimate[, "B17001_001"],
# pblack=acsecon@estimate[,"B03002_004"]/acsecon@estimate[, "B03002_001"],
# phisp=acsecon@estimate[,"B03002_012"]/acsecon@estimate[, "B03002_001"],
# giniz=scale(acsecon@estimate[, "B19083_001"]),
# ppovertyz=scale(acsecon@estimate[, "B17001_002"]/acsecon@estimate[, "B17001_001"]))
# 
# final.dat < -merge(dat.long, cov_dat, by="cofips")
# head(final.dat)
