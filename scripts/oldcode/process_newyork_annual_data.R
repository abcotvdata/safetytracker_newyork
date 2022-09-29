library(tidyverse)
library(tidycensus)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
library(sf)
library(lubridate)

# Save for backup the archived annual crime totals files from NYPD
# download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/seven-major-felony-offenses-2000-2021.xls",
#              "data/source/annual/nyc_major_felonies.xls")
#download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/non-seven-major-felony-offenses-2000-2021.xls",
#              "data/source/annual/nyc_other_felonies.xls")
#download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/seven-major-felony-offenses-by-precinct-2000-2021.xls",
#              "data/source/annual/precinct_major_felonies.xls")
#download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/historical-crime-data/non-seven-major-felony-offenses-by-precinct-2000-2021.xls",
#              "data/source/annual/precinct_other_felonies.xls")


# Read in the precinct major felonies file and format col names
precinct_major_felonies <- read_excel("data/source/annual/precinct_major_felonies.xls",
skip = 2) %>% janitor::clean_names() %>% rename("precinct"="pct")
# fill in precinct numbers from merged cells in original spreadsheet
precinct_major_felonies <- precinct_major_felonies %>% fill(1)
# only keep rows of data, dropping 12 rows at bottom that are data footnotes
precinct_major_felonies <- precinct_major_felonies[1:624,]
# rename columns with year numbers to total00, total01, etc., format for consistent use in tracker
names(precinct_major_felonies) <- c("precinct","crime","total00",
                                    "total01","total02","total03",
                                    "total04","total05","total06",
                                    "total07","total08","total09",
                                    "total10","total11","total12",
                                    "total13","total14","total15",
                                    "total16","total17","total18",
                                    "total19","total20","total21")
# calculate increases for periods existing in the raw data
# precinct_major_felonies$increase2yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2019*100-100,1)
# precinct_major_felonies$increase5yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2016*100-100,1)
# precinct_major_felonies$increase10yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2011*100-100,1)
# precinct_major_felonies$increase20yr <- round(precinct_major_felonies$x2021/precinct_major_felonies$x2001*100-100,1)
# clean and standardize crime names and types for merging later with newer/recent data files
precinct_major_felonies$crime <- case_when(precinct_major_felonies$crime == "MURDER & NON NEGL. MANSLAUGHTER" ~ "Murder",
                                           precinct_major_felonies$crime == "TOTAL SEVEN MAJOR FELONY OFFENSES" ~ "Total Major Felonies",
                                           precinct_major_felonies$crime == "GRAND LARCENY OF MOTOR VEHICLE" ~ "Motor Vehicle Theft",
                                           TRUE ~ str_to_title(precinct_major_felonies$crime, locale = "en"))
precinct_major_felonies$type <- case_when(precinct_major_felonies$crime == "Murder" ~ "Violent",
                                           precinct_major_felonies$crime == "Rape" ~ "Violent",
                                           precinct_major_felonies$crime == "Felony Assault" ~ "Violent",
                                          precinct_major_felonies$crime == "Total Major Felonies" ~ "Combined Total",
                                           TRUE ~ "Property")


# Read in the precinct major felonies file and format col names
# We are not sure how much we'll use this data, but will process
# at same time so we have consistent data across all crime levels
precinct_other_felonies <- read_excel("data/source/annual/precinct_other_felonies.xls",
                                      skip = 2) %>% janitor::clean_names() %>% rename("precinct"="pct")
# fill in precinct numbers from merged cells in original spreadsheet
precinct_other_felonies <- precinct_other_felonies %>% fill(1)
# only keep rows of data, dropping 12 rows at bottom that are data footnotes
precinct_other_felonies <- precinct_other_felonies[1:702,]
# rename columns with year numbers to total00, total01, etc., format for consistent use in tracker
names(precinct_other_felonies) <- c("precinct","crime","total00",
                                    "total01","total02","total03",
                                    "total04","total05","total06",
                                    "total07","total08","total09",
                                    "total10","total11","total12",
                                    "total13","total14","total15",
                                    "total16","total17","total18",
                                    "total19","total20","total21")
# calculate increases for periods existing in the raw data
# precinct_other_felonies$increase2yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2019*100-100,1)
# precinct_other_felonies$increase5yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2016*100-100,1)
# precinct_other_felonies$increase10yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2011*100-100,1)
# precinct_other_felonies$increase20yr <- round(precinct_other_felonies$x2021/precinct_other_felonies$x2001*100-100,1)
# clean and standardize crime names and types in case of merger later with newer/recent data files
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







