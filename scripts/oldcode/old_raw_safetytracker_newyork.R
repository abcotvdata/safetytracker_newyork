library(tidyverse)
library(tidycensus)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
library(sf)
library(lubridate)

# Save for backup the archived files from NYPD
download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/seven-major-felony-offenses-2000-2021.xls",
              "nyc_major_felonies.xls")
download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/non-seven-major-felony-offenses-2000-2021.xls",
              "nyc_other_felonies.xls")
download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/seven-major-felony-offenses-by-precinct-2000-2021.xls",
              "precinct_major_felonies.xls")
download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/non-seven-major-felony-offenses-by-precinct-2000-2021.xls",
              "precinct_other_felonies.xls")




# first test at importing the precinct major felonies file
precinct_major_felonies <- read_excel("precinct_major_felonies.xls",
skip = 2) %>% janitor::clean_names() %>% rename("precinct"="pct")
# fill in precinct numbers from merged cells in original spreadsheet
precinct_major_felonies <- precinct_major_felonies %>% fill(1)
# only keep rows of data, dropping 12 rows at bottom that are data footnotes
precinct_major_felonies <- precinct_major_felonies[1:624,]
# calculate increases
precinct_major_felonies$increase2yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2019*100-100,1)
precinct_major_felonies$increase5yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2016*100-100,1)
precinct_major_felonies$increase10yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2011*100-100,1)
precinct_major_felonies$increase20yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2001*100-100,1)

precinct_major_felonies$crime <- case_when(precinct_major_felonies$crime == "MURDER & NON NEGL. MANSLAUGHTER" ~ "Murder",
                                           precinct_major_felonies$crime == "TOTAL SEVEN MAJOR FELONY OFFENSES" ~ "Total Major Felonies",
                                           precinct_major_felonies$crime == "GRAND LARCENY OF MOTOR VEHICLE" ~ "Motor Vehicle Theft",
                                           TRUE ~ str_to_title(precinct_major_felonies$crime, locale = "en"))
precinct_major_felonies$type <- case_when(precinct_major_felonies$crime == "Murder" ~ "Violent",
                                           precinct_major_felonies$crime == "Rape" ~ "Violent",
                                           precinct_major_felonies$crime == "Felony Assault" ~ "Violent",
                                          precinct_major_felonies$crime == "Total Major Felonies" ~ "Combined Total",
                                           TRUE ~ "Property")


# first test at importing the precinct other felonies file
precinct_other_felonies <- read_excel("precinct_other_felonies.xls",
                                      skip = 2) %>% janitor::clean_names() %>% rename("precinct"="pct")
# fill in precinct numbers from merged cells in original spreadsheet
precinct_other_felonies <- precinct_other_felonies %>% fill(1)
# only keep rows of data, dropping 12 rows at bottom that are data footnotes
precinct_other_felonies <- precinct_other_felonies[1:702,]
# calculate increases
precinct_other_felonies$increase2yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2019*100-100,1)
precinct_other_felonies$increase5yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2016*100-100,1)
precinct_other_felonies$increase10yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2011*100-100,1)
precinct_other_felonies$increase20yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2001*100-100,1)

precinct_other_felonies$crime <- case_when(precinct_other_felonies$crime == "FORGERY/THEFT-FRAUD/IDENTITY THEFT" ~ "Forgery, Fraud and Identity Theft",
                                           precinct_other_felonies$crime == "TOTAL NON-SEVEN MAJOR FELONY OFFENSES" ~ "Total Non-Major Felonies",
                                           precinct_other_felonies$crime == "FELONY SEX CRIMES (3)" ~ "Sex Crimes",
                                           precinct_other_felonies$crime == "FELONY DANGEROUS DRUGS  (1)" ~ "Drug Crimes",
                                           precinct_other_felonies$crime == "FELONY DANGEROUS WEAPONS (2)" ~ "Weapons Crimes",
                                           precinct_other_felonies$crime == "FEL. CRIMINAL MISCHIEF & RELATED OFFENSES" ~ "Criminal Mischief & Related Crimes",
                                           precinct_other_felonies$crime == "OTHER FELONIES (4)" ~ "Other Felonies",
                                           TRUE ~ str_to_title(precinct_other_felonies$crime, locale = "en"))
precinct_other_felonies$type <- case_when(precinct_other_felonies$crime == "Forgery, Fraud and Identity Theft" ~ "Property",
                                          precinct_other_felonies$crime == "Sex Crimes" ~ "Violent",
                                          precinct_other_felonies$crime == "Total Non-Major Felonies" ~ "Combined Total",
                                          TRUE ~ "Other")


# murders by precinct file, plus rate column
murders_precinct <- precinct_crime %>% filter(crime=="Murder")
murders_precinct <- merge(precincts_geo, murders_precinct, by="precinct")
murders_precinct$last12mos <- (murders_precinct$x2021-murders_precinct$yeartodate2021)+murders_precinct$yeartodate2022
murders_precinct$rate21 <- round((murders_precinct$x2021/murders_precinct$population)*100000,1)
murders_precinct$avg_prior3years <- (murders_precinct$x2019+
  murders_precinct$x2020+
  murders_precinct$x2021)/3
murders_precinct$rate_prior3years <- 
  round((murders_precinct$avg_prior3years/murders_precinct$population)*100000,1)
murders_precinct$rate_last12 <- 
  round(((murders_precinct$last12mos/murders_precinct$population))*100000,1)



# auto thefts by precinct file, plus rate column
autothefts_precinct <- precinct_crime %>% filter(crime=="Motor Vehicle Theft")
autothefts_precinct <- merge(precincts_geo, autothefts_precinct, by="precinct")
autothefts_precinct$last12mos <- (autothefts_precinct$x2021-autothefts_precinct$yeartodate2021)+autothefts_precinct$yeartodate2022
autothefts_precinct$rate21 <- round((autothefts_precinct$x2021/autothefts_precinct$population)*100000,1)
autothefts_precinct$avg_prior3years <- (autothefts_precinct$x2019+
                                       autothefts_precinct$x2020+
                                       autothefts_precinct$x2021)/3
autothefts_precinct$rate_prior3years <- 
  round((autothefts_precinct$avg_prior3years/autothefts_precinct$population)*100000,1)
autothefts_precinct$rate_last12 <- 
  round(((autothefts_precinct$last12mos/autothefts_precinct$population))*100000,1)



# MURDERS MAP
# Create quick labels for murders map
murderlabel <- paste("<b>Precinct #",murders_precinct$precinct,
                     "<br></b>Estimated Population: ",murders_precinct$population,
                     "<br><br>Last 12 months homicide rate: ",murders_precinct$rate_last12,
                     "<br>2021 homicide rate: ",murders_precinct$rate21,
                     "<br>2019-2021 3 year homicide rate: ",murders_precinct$rate_prior3years,
                     "<br><br>Last 12 months homicides: ",murders_precinct$last12mos,
                     "<br><br>2021 homicides: ",murders_precinct$x2021,
                     "<br>2020 homicides: ",murders_precinct$x2020,
                     "<br>2019 homicides: ",murders_precinct$x2019)
# Set bins for numbers of crimes for murders map
murderbins <- c(0,1,5,10,15,20,25,Inf)
murderpal <- colorBin("viridis", murders_precinct$rate_last12, bins = murderbins)

# Create rapid prototype of murders map
nyc_murder_map <- leaflet(murders_precinct) %>%
  setView(-73.9, 40.7, zoom = 11) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = murderlabel, weight = 1, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0.4,
              fillColor = ~murderpal(rate_last12)) %>% 
  addLegend(pal = murderpal, 
            values = murders_precinct$rate_last12, 
            position = "bottomleft", 
            title = "Homicide Rates<br>Last 12 Months")
nyc_murder_map


# AUTO THEFT
# Create quick labels for auto theft map
autotheftslabel <- paste("<b>Precinct #",autothefts_precinct$precinct,
                     "<br></b>Estimated Population: ",autothefts_precinct$population,
                     "<br><br>Last 12 months auto thefts rate: ",autothefts_precinct$rate_last12,
                     "<br>2021 auto thefts rate: ",autothefts_precinct$rate21,
                     "<br>2019-2021 3 year auto theft rate: ",autothefts_precinct$rate_prior3years,
                     "<br><br>Last 12 months auto thefts: ",autothefts_precinct$last12mos,
                     "<br><br>2021 auto thefts: ",autothefts_precinct$x2021,
                     "<br>2020 auto thefts: ",autothefts_precinct$x2020,
                     "<br>2019 auto thefts: ",autothefts_precinct$x2019)
# Set bins for numbers of crimes for auto thefts map
autotheftsbins <- c(0,10,50,100,150,200,250,Inf)
autotheftspal <- colorBin("viridis", autothefts_precinct$rate_last12, bins = autotheftsbins)

# Create rapid prototype of murders map
nyc_autothefts_map <- leaflet(autothefts_precinct) %>%
  setView(-73.9, 40.7, zoom = 11) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = autotheftslabel, weight = 1, smoothFactor = 0.5,
              opacity = 1, fillOpacity = 0.4,
              fillColor = ~autotheftspal(rate_last12)) %>% 
  addLegend(pal = autotheftspal, 
            values = autothefts_precinct$rate_last12, 
            position = "bottomleft", 
            title = "Auto Thefts Per 100K<br>People Over the Last 12 Months")
nyc_autothefts_map





