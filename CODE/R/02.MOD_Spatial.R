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
library(magrittr)
library(tidycensus)
library(INLA)
library(ggplot2)
library(dplyr)
library(plyr)
library(sf)
library(mapview)
library(car)
library(maptools)
# source("http://bioconductor.org/biocLite.R") 
# biocLite("Rgraphviz")
library(Rgraphviz)
gpclibPermit()
options(scipen = 999)

# Read in Data
fn <- list.files(path = "DATA/nchs_births/R/data", 
                 pattern = ".rda")
# model1 <- readRDS(file = "DATA/nchs_births/R/data/model1.rda")
# model2 <- readRDS(file = "DATA/nchs_births/R/data/model2.rda")
# model3 <- readRDS(file = "DATA/nchs_births/R/data/model3.rda")
# model4 <- readRDS(file = "DATA/nchs_births/R/data/model4.rda")
# model5 <- readRDS(file = "DATA/nchs_births/R/data/model5.rda")
# model6 <- readRDS(file = "DATA/nchs_births/R/data/model6.rda")
# gatestmodel1 <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1.rda")
# gatestmodel1singleyear <- readRDS(file = "DATA/nchs_births/R/data/gatestmodel1singleyear.rda")
# model1singleyear <- readRDS(file = "DATA/nchs_births/R/data/model1singleyear.rda")

# Create merged spatial data file -----------------------------------

# Read in county shapefiles (Based on 2010 County Boundaries)
tigris_spdf <- tigris::counties()

  # Subset to contiguous for now (keep Alaska and Hawaii and Puerto Rico)
tigris_spdf <-  tigris_spdf[which(!(tigris_spdf$STATEFP %in% c("60", "66", "69", "78"))),]

# Read in files ----------------------------------------------------
for (i in fn) {
  
  # Load health data frame
  path <- paste0("DATA/nchs_births/R/data/", i)
  model <- readRDS(path)

  # Merge case data with county shapefile
  spdf <- tigris::geo_join(tigris_spdf, model, "GEOID", "combfips")
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
  spdf <- spdf[order(spdf$combfips, spdf$dob_yy),]
  spdf$idx <- 1:nrow(spdf)
  # spdf$id <- 1:dim(health_df)[1]
  head(spdf)
  
  # Extract data.frame from the SDPF
  dat_df <- as.data.frame(spdf) 
  
  # Create adjacency matrix ----------------------------------
  # spdf_adj <- spdep::poly2nb(spdf, queen = T, row.names = spdf$idx) # set queen = F for rook
  # adj <- spdep::nb2mat(spdf_adj, style = "B", # create binary matrix
  #                      zero.policy = TRUE)  # added to avoid error
  # adj <- as(adj, "dgTMatrix") # create sparse matrix, memory conservation
  # 
  # K-Nearest Neighbors contiguity matrix
  plot(spdf)
  nbs <- spdep::knearneigh(coordinates(spdf), k = 4)#, longlat = T) # k = 4 nearest neighbors
  nbs <- spdep::knn2nb(nbs, row.names = spdf$idx, sym = T) # force symmetry!!
  
  # Examine as matrix
  mat <- spdep::nb2mat(nbs, style = "B", zero.policy = TRUE)
  colnames(mat) <- rownames(mat)
  mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
  wts <- spdep::nb2listw(nbs)
  # plot(wts, coords = sp::coordinates(spdf), add = T, col = 2)
  
  # Create file path for saving adjacency
  filename <- substr(i, 1, 6)
  graphpath <- paste0("DATA/nchs_births/R/data/graphs/", filename, ".adj")
  
  # Continue with nb2INLA
  adj <- spdep::nb2INLA(graphpath, nbs)
  
  # Read and view graph
  # image(inla.graph2matrix(h), xlab = "", ylab = "", main = "")
  # summary(h)
  # plot(h)
  
  # tx <- st_as_sf(spdf)
  # tx %>%
  #   ggplot() + geom_sf()

  # Export file 
  savepath <- paste0("DATA/nchs_births/R/data/spatial/", i)
  saveRDS(spdf, file = savepath)
  cat("*")
}
