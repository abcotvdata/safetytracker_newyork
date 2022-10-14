library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
library(sf)


# UPDATED AND VETTED 9/28/22
# OPEN WORK TO AUTOMATE THIS STEP IN GH ACTIONS
# OPEN WORK ON NUMERIC OF PRECINCT 13 SHOWING UP IN SCIENTIFIC FORMAT

# GEOGRAPHY
# downloading geojson and csv of nypd precincts from city open data market
download.file("https://data.cityofnewyork.us/api/geospatial/78dh-3ptz?method=export&format=GeoJSON","data/source/geo/precinctmap.geojson")
# download.file("https://data.cityofnewyork.us/api/views/kmub-vria/rows.csv?accessType=DOWNLOAD","data/source/geo/precinctmap.csv")

# Read in geojson and then transform to sf format
precincts_geo <- st_read("data/source/geo/precinctmap.geojson") %>% st_transform(3857)

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of NYPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                       year = 2020,
                       output = 'wide',
                       variables = "P1_001N", 
                       state = "NY",
                       county = c("Bronx","Richmond","Queens","New York","Kings"),
                       geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

# Calculate the estimated population of beat geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
precincts_withpop <- st_interpolate_aw(blocks, precincts_geo, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
precincts_withpop <- st_drop_geometry(precincts_withpop)
# Binds that new population column to the table
precincts_geo <- cbind(precincts_geo,precincts_withpop)
# Cleans up unneeded calculation file
rm(precincts_withpop, blocks)

# Check total population assigned/estimated across all precincts
sum(precincts_geo$population) # tally is 8,801,940 # city's reported pop is 8,804,190 in 2020
# OPEN WORK TO DETERMINE HOW TO DO RATES IN PARTS OF CITY WITH
# WILDLY DIFFERENT DAYTIME POPULATIONS

# Round the population figure; rounded to nearest thousand
precincts_geo$population <- round(precincts_geo$population,-3)

precincts_geo <- precincts_geo %>% st_transform(4326)
precincts_geo <- st_make_valid(precincts_geo)

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/source/geo/precincts.geojson")
st_write(precincts_geo,"data/source/geo/precincts.geojson")
saveRDS(precincts_geo,"scripts/rds/precincts.rds")
# add line  below when uploading data for pages
# beats <- st_read("data/source/geo/beats.geojson")



# BARE PRECINCT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
# popbins <- c(0,1000, 10000,25000,50000,100000, Inf)
# poppal <- colorBin("YlOrRd", precincts_geo$population, bins = popbins)
# poplabel <- paste(sep = "<br>", precincts_geo$precinct,prettyNum(precincts_geo$population, big.mark = ","))

# nyc_precincts_map <- leaflet(precincts_geo) %>%
#  setView(-73.9, 40.7, zoom = 10) %>% 
#  addProviderTiles(provider = "Esri.WorldImagery") %>%
#  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
#              opacity = 0.5, fillOpacity = 0.3,
#              fillColor = ~poppal(`population`))
# nyc_precincts_map
