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

# Import placenames from Frank E
# Adding placenames for headers in place of the beats
# A little more work to do here; may want to replicate this with zips too
precincts_geo$placename <- case_when(precincts_geo$precinct == "1" ~ "World Trade Center, SOHO, Tribeca and Wall Street",
                                     precincts_geo$precinct == "5" ~ "Chinatown Little Italy and Bowery",
                                     precincts_geo$precinct == "6" ~ "Greenwich Village, West Village and Christopher Street",
                                     precincts_geo$precinct == "7" ~ "Lower East Side and Orchard Street",
                                     precincts_geo$precinct == "9" ~ "East Village",
                                     precincts_geo$precinct == "10" ~ "Chelsea, Clinton, Hell's Kitchen South and Hudson Yards",
                                     precincts_geo$precinct == "13" ~ "Flatiron District, Kips Bay and Rose Hill",
                                     precincts_geo$precinct == "14" ~ "Times Square, Grand Central and Koreatown",
                                     precincts_geo$precinct == "17" ~ "Sutton Area, Beekman Place and Murray Hill",
                                     precincts_geo$precinct == "18" ~ "Diamond District, Theatre District and Rockefeller Plaza",
                                     precincts_geo$precinct == "19" ~ "Upper East Side",
                                     precincts_geo$precinct == "20" ~ "Upper West Side",
                                     precincts_geo$precinct == "22" ~ "Central Park",
                                     precincts_geo$precinct == "23" ~ "East Harlem and El Barrio",
                                     precincts_geo$precinct == "24" ~ "Manhattan Valley",
                                     precincts_geo$precinct == "25" ~ "East Harlem",
                                     precincts_geo$precinct == "26" ~ "Morningside Heights and Columbia University",
                                     precincts_geo$precinct == "28" ~ "Central Harlem",
                                     precincts_geo$precinct == "30" ~ "Hamilton Heights, Sugar Hill and West Harlem",
                                     precincts_geo$precinct == "103" ~ "Hollis, Lakewood and Jamaica",
                                     precincts_geo$precinct == "104" ~ "Ridgewood, Glendale, Middle Village and Maspeth",
                                     precincts_geo$precinct == "105" ~ "Queens Village, Cambria Heights, Laurelton and Springfield Gardens",
                                     precincts_geo$precinct == "106" ~ "Ozone Park, South Ozone Park and Lindenwood",
                                     precincts_geo$precinct == "32" ~ "Northeast Harlem",
                                     precincts_geo$precinct == "33" ~ "Washington Heights",
                                     precincts_geo$precinct == "34" ~ "Washington Heights and Inwood",
                                     precincts_geo$precinct == "40" ~ "Port Morris, Mott Haven and Melrose",
                                     precincts_geo$precinct == "41" ~ "Hunts Point and Longwood",
                                     precincts_geo$precinct == "42" ~ "Morrisania and Claremont",
                                     precincts_geo$precinct == "43" ~ "Bronx River, Unionport and Parkchester",
                                     precincts_geo$precinct == "44" ~ "Grand Concourse, Bronx Terminal Market and Yankee Stadium",
                                     precincts_geo$precinct == "45" ~ "Co-op City and City Island",
                                     precincts_geo$precinct == "46" ~ "Fordham, University Heights, Morris Heights and Mount Hope",
                                     precincts_geo$precinct == "47" ~ "Woodlawn, Wakefield, Williamsbridge and Baychester",
                                     precincts_geo$precinct == "48" ~ "Belmont, East Tremont and West Farms",
                                     precincts_geo$precinct == "49" ~ "Allerton, Morris Park, Van Nest and Pelham Parkway",
                                     precincts_geo$precinct == "50" ~ "Marble Hill, Spuyten Duyvil and Van Cortlandt Park",
                                     precincts_geo$precinct == "52" ~ "Kingsbridge, Norwood and University Heights",
                                     precincts_geo$precinct == "60" ~ "Coney Island, Brighton Beach and Sea Gate",
                                     precincts_geo$precinct == "61" ~ "Kings Bay, Gravesend and Sheepshead Bay",
                                     precincts_geo$precinct == "62" ~ "Bensonhurst, Mapleton and Bath Beach",
                                     precincts_geo$precinct == "63" ~ "Flatlands, Georgetown and Bergen Beach",
                                     precincts_geo$precinct == "66" ~ "Parkville, Borough Park and Mapleton",
                                     precincts_geo$precinct == "67" ~ "East Flatbush and Remsen Village",
                                     precincts_geo$precinct == "68" ~ "Bay Ridge and Dyker Heights",
                                     precincts_geo$precinct == "69" ~ "Canarsie",
                                     precincts_geo$precinct == "70" ~ "Midwood, Fiske Terrace and Ditmas Park",
                                     precincts_geo$precinct == "71" ~ "Crown Heights, Wingate and Prospect Lefferts",
                                     precincts_geo$precinct == "72" ~ "Sunset Park and Windsor Terrace",
                                     precincts_geo$precinct == "73" ~ "Brownsville and Ocean Hill",
                                     precincts_geo$precinct == "75" ~ "East New York and Cypress Hills",
                                     precincts_geo$precinct == "76" ~ "Carroll Gardens, Red Hook and Cobble Hill",
                                     precincts_geo$precinct == "77" ~ "Crown Heights and Prospect Heights",
                                     precincts_geo$precinct == "78" ~ "Park Slope",
                                     precincts_geo$precinct == "79" ~ "Bedford Stuyvesant",
                                     precincts_geo$precinct == "81" ~ "Bedford Stuyvesent and Stuyvesant Heights",
                                     precincts_geo$precinct == "83" ~ "Bushwick",
                                     precincts_geo$precinct == "84" ~ "Brooklyn Heights, Boerum Hill and Dumbo",
                                     precincts_geo$precinct == "88" ~ "Clinton Hill, Brooklyn Navy Yard, Fort Greene",
                                     precincts_geo$precinct == "90" ~ "Williamsburg",
                                     precincts_geo$precinct == "94" ~ "Greenpoint",
                                     precincts_geo$precinct == "100" ~ "Southern Rockaway Peninsula",
                                     precincts_geo$precinct == "101" ~ "Eastern Rockaway Peninsula",
                                     precincts_geo$precinct == "102" ~ "Kew Gardens, Richmond Hill and Woodhaven",
                                     precincts_geo$precinct == "107" ~ "Fresh Meadows, Cunningham Heights and Hilltop Village",
                                     precincts_geo$precinct == "108" ~ "Long Island City, Sunnyside and Woodside",
                                     precincts_geo$precinct == "109" ~ "Downtown Flushing, Whitestone, Queensboro Hill and College Point",
                                     precincts_geo$precinct == "110" ~ "Corona, Elmhurst and Citi Field",
                                     precincts_geo$precinct == "111" ~ "Bayside, Douglaston, Little Neck and Auburndale",
                                     precincts_geo$precinct == "112" ~ "Forest Hills, Rego Park and Forest Hills Gardens",
                                     precincts_geo$precinct == "113" ~ "Jamaica, Queens, St. Albans, Hollis, Springfield Gardens and JFK Airport",
                                     precincts_geo$precinct == "114" ~ "Astoria, Long Island City, Woodside and Jackson Heights",
                                     precincts_geo$precinct == "115" ~ "East Elmhurst, North Corona and LaGuardia Airport",
                                     precincts_geo$precinct == "120" ~ "North Shore of Staten Island",
                                     precincts_geo$precinct == "121" ~ "Northwestern Shore of Staten Island",
                                     precincts_geo$precinct == "122" ~ "South Shore of Staten Island",
                                     precincts_geo$precinct == "123" ~ "South Shore of Staten Island",
                             TRUE ~ "Unknown")


# transforming for mapping
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
