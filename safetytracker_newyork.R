library(tidyverse)
library(tidycensus)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
library(sf)

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
murders <- precinct_major_felonies %>% filter(crime=="MURDER & NON NEGL. MANSLAUGHTER")
murders_by_precinct <- merge(precincts, murders, by="precinct")
murders_by_precinct$rate21 <- 
  round((murders_by_precinct$x2021/murders_by_precinct$population)*100000,1)

# murders by precinct file, plus rate column
autothefts <- precinct_major_felonies %>% filter(crime=="GRAND LARCENY OF MOTOR VEHICLE")
autothefts_by_precinct <- merge(precincts, autothefts, by="precinct")
autothefts_by_precinct$rate21 <- 
  round((autothefts_by_precinct$x2021/autothefts_by_precinct$population)*100000,1)



# MURDERS
# Create quick labels for murders map
murderlabel <- paste("Precinct #",murders_by_precinct$precinct,
                     "<br>2021 homicide rate: ",murders_by_precinct$rate21,
                     "<br><br>2021 homicides: ",murders_by_precinct$x2021,
                     "<br>2020 homicides: ",murders_by_precinct$x2020,
                     "<br>2019 homicides: ",murders_by_precinct$x2019)
# Set bins for numbers of crimes for murders map
murderbins <- c(0,1,2,3,5,10,15,20,25,Inf)
murderpal <- colorBin("YlOrRd", murders_by_precinct$x2021, bins = murderbins)

# Create rapid prototype of murders map
nyc_murder_map <- leaflet(murders_by_precinct) %>%
  setView(-73.9, 40.7, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "#444444", popup = murderlabel, weight = 0.5, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~murderpal(x2021)) %>% 
  addLegend(pal = murderpal, 
            values = murders_by_precinct$x2021, 
            position = "bottomleft", 
            title = "Homicides in 2021")
nyc_murder_map


# AUTO THEFT
# Create quick labels for auto theft map
autotheftlabel <- paste("Precinct #",autothefts_by_precinct$precinct,
                     "<br>2021 auto theft rate: ",autothefts_by_precinct$rate21,
                     "<br><br>2021 auto thefts: ",autothefts_by_precinct$x2021,
                     "<br>2020 auto thefts: ",autothefts_by_precinct$x2020,
                     "<br>2019 auto thefts: ",autothefts_by_precinct$x2019)
# Set bins for numbers of crimes for auto theft map
autotheftbins <- c(0,10,25,50,100,150,200,250,300,Inf)
autotheftpal <- colorBin("YlOrRd", autothefts_by_precinct$rate21, bins = autotheftbins)

# Create rapid prototype of murders map
nyc_autothefts_map <- leaflet(autothefts_by_precinct) %>%
  setView(-73.9, 40.7, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "#444444", popup = autotheftlabel, weight = 0.5, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~autotheftpal(rate21)) %>% 
  addLegend(pal = autotheftpal, 
            values = autothefts_by_precinct$rate21, 
            position = "bottomleft", 
            title = "Auto Thefts<br>per 100K people in 2021")
nyc_autothefts_map




