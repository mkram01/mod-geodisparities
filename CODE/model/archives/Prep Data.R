## Assign FIPS Code lists
# Census Regions
southfips <- c('10', '11', '12','13', '24', '37', '45', '51', '54', '01', '21', '28', '47', '05', '22', '40', '48')
westfips <- c('04', '08', '16', '30', '32', '35', '49', '56', '02', '06', '15', '41', '53')
midwest <- c('17', '18', '26', '39', '55', '19', '20', '27', '29', '31', '38', '46')
northeastfips <- c('09', '23', '25', '33', '44', '50', '34', '36', '42')

# Census Divisions
newenglandfips <- c('09', '23', '25', '33', '44', '50')
midatlanticfips <- c('34', '36', '42')
southatlanticfips <- c('10', '11', '12','13', '24', '37', '45', '51', '54')
escfips <- c('01', '21', '28', '47')
wscfips <- c('05', '22', '40', '48')
encfips <- c('17', '18', '26', '39', '55')
wncfips <- c('19', '20', '27', '29', '31', '38', '46')
pacfips <- c('02', '06', '15', '41', '53')
mountainfips <- c('04', '08', '16', '30', '32', '35', '49', '56')

# Create summarized dataset
southatlantic <- readRDS(paste0(data_repo, '/nchs_births/R/Data/model1.rda')) %>%
  filter(racehisp_recode %in% c(2,3),
         substr(combfips,1,2) %in% southatlanticfips,
         dob_yy %in% 2007:2016) %>%
  mutate(black = ifelse(racehisp_recode == 3, 1, 0),
         combfips = factor(combfips)) %>%
  group_by(dob_yy, combfips, black) %>%
  summarise(vptb = sum(vptb) + 1,
            ptb = sum(ptb) + 1,
            births = sum(births) + 1)


## Spatial data ----------------------------------------------------------------
## Prep spatial data for region only
#Read in national county shapefile and save in MOD folder as `.gpkg`.
cty_sf <- st_read('../../data/spatial/cb_2016_us_county_500k.shp') %>%
  filter(STATEFP %in% southatlanticfips) %>%
  st_transform(102003) # Albers Equal Area
st_write(cty_sf, '../../data/spatial/southatlantic_county.gpkg', delete_dsn = T)

## Read in spatial data
# This is the data saved above. It is saved as `sf` object which is useful for
# *long* format (e.g. multiple years), but also want an `sp` object for creating
# *neighbor* objects and simpler *wide* representations.

southatlantic_sp <- st_read('../../data/spatial/southatlantic_county.gpkg') %>%
  inner_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  group_by(GEOID) %>%
  summarise(vptb = sum(vptb),
            ptb = sum(ptb),
            births = sum(births),
            rawvptb = vptb / births * 1000,
            rawptb = ptb / births * 1000) %>%
  as('Spatial')

# Create an ordered ID specific to ordering in sp (e.g. aligns with nb object)
southatlantic_sp$ID <- seq_len(nrow(southatlantic_sp))

# Create long version (e.g. repeated rows for each year within county) as an
# `sf` object useful for facet printing of year x race.

southatlantic_sf <- southatlantic_sp %>%
  st_as_sf() %>%
  select(GEOID, ID) %>%
  right_join(southatlantic, by = c('GEOID' = 'combfips')) %>%
  mutate(ID3 = ID, # ID and ID3 will be for f() in INLA
         ID2 = ID, # ID and ID2 will be for f() in INLA
         year_c = dob_yy - 2007)  %>%# scale year so intercept interpretable
  arrange(ID)


## Create adjacency matrix using k-nearest neighbors
## I'm going to use k-nearest neighbors, with $k=6$. I will force symmetry so
## that if $a$ is neighbors with $b$, then $b$ must also be neighbors with $a$.

# Create knn neighbor object
southatlantic_knn <- southatlantic_sp %>%
  coordinates() %>%  # get centroids
  knearneigh(k = 6) %>% # calculate the 6 nearest neighbors
  knn2nb(sym = T)  # knn neighbor object

# Write an INLA adjacency file
nb2INLA('../../data/spatial/southatlantic_knn6.adj', southatlantic_knn)

# Look at KNN object
summary(southatlantic_knn)
#plot(southatlantic_knn, coords = coordinates(as(cty_sf, 'Spatial')))
#So counties have from 6 to 11 neighbors (>6 because of forced symmetry).

#View INLA object
g <- inla.read.graph('../../data/spatial/southatlantic_knn6.adj')
summary(g)
plot(g)

# Summary of object
# * n is the size of the graph (e.g. the number of areal units)
# * nnbs are the number of neighbors for each node.
# The summary (names) tells us that there are either $4, 5, 6$ or $7$
# neighbors per node
# (count) says how many counties have each number of neighbors. __Note that
# the * symmetry = T above is what forced some counties to have > 4 neighbors__
# * ncc is about _connected components_ of the graph and this tells us how many are in the list (e.g. only 1)


# Create basemap of raw (pooled) rates
# These are rates pooled across all years and simply serve a reference for
# when we start getting modeled estimates.
tm_shape(southatlantic_sp) +
  tm_fill('rawptb',
          style = 'quantile') +
  tm_borders()

# INLA Models
# Use regional object object as input data (`nrow(region)`
# rows = `length(unique(region$combfips))` # counties x
# `length(unique(region$dob_yy))` years x
# `length(unique(southatlantic$black))` race groups age categories).
