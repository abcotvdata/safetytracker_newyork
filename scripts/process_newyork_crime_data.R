library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(rvest)
library(XML)
library(sf)

# The source of the NYPD's weekly precinct level crime data is here at this link
# Second url is just a sample of the xls file naming convention they're using
# https://www1.nyc.gov/site/nypd/stats/crime-statistics/borough-and-precinct-crime-stats.page
# https://www1.nyc.gov/assets/nypd/downloads/excel/crime_statistics/cs-en-us-114pct.xlsx

### BUILDING A LIST OF LINKS TO WEEKLY DATA FILES

# Prepare to add citywide files; updated by NYPD weekly on same schedule as precinct files
citywide_weekly_url <- "https://www1.nyc.gov/assets/nypd/downloads/excel/crime_statistics/cs-en-us-city.xlsx"
# Potential redundant archive of citywide file; may delete
download.file(citywide_weekly_url,paste0(
  "data/source/archive/cs-en-us-city",
  Sys.Date(),".xlsx"))

# Set url for the main page where the precinct Excel links are based
url <- "https://www1.nyc.gov/site/nypd/stats/crime-statistics/borough-and-precinct-crime-stats.page"
# Scrape all links on the page
links <- readLines(url) %>% getHTMLLinks
# Extract a list of just the links that include the Excel files
xlsxFiles <- grep("\\.xlsx", links)
# Replaces the list of values with just the desired Excel links
links <- links[xlsxFiles]
rm(xlsxFiles)
# Prepend the urls in the list to get full urls for batch download
links <- paste0("https://www1.nyc.gov",links,sep="")
# Run function to download every file from the list of urls
# append the one citywide file for consistent processing
links <- append(links, citywide_weekly_url)

# into new directory called precinctdata
for (link in links) {
  download.file(link, destfile = paste0("data/source/weekly/",basename(link),sep="")) }

### COLLECT AND RE-FORMAT THE WEEKLY FILES

# Import one file, to get single row that contains the week
# extract from that string two values: text noting week of the report and latest date
grabdatedf <- read_excel("data/source/weekly/cs-en-us-040pct.xlsx")[c(8),c(3)]
update_week <- as.character(grabdatedf[1,1])
update_date <- sub(".*\\s", "", trimws(update_week))
update_date <- format(as.Date.character(update_date, "%m/%d/%Y"), "%B %d")
rm(grabdatedf)
# extract from that string two values: text noting week NUMBER of the report out of 52 weeks
# for potential multiplier in projecting annualized 2022 rates and multiyear averaged rates
grabweekdf <- read_excel("data/source/weekly/cs-en-us-040pct.xlsx")[c(7),c(1)]
update_weeknum <- as.character(grabweekdf[1,1])
update_weeknum <- sub(".*\\s", "", trimws(update_weeknum))
# OPEN WORK: is this necessary to keep
update_weekshare <- as.numeric(update_weeknum)/52
# cleanup
rm(grabweekdf)


# Create list of filenames of all the mass-downloaded Excel files for function
filenames <- list.files(path = "data/source/weekly", pattern = '.xlsx', full.names = TRUE)
filenames <- set_names(filenames)

# Function that will import and process the weekly files from list
# including formatting all cols as text and naming cols
precinct_fun <- function(filename) {

precinct_process <- read_excel(filename, 
                              col_types = "text",
                              col_names = c("crime","delete","lastweek2022", "lastweek2021",
                                            "week_pct_change", "lastmonth2022","lastmonth2021","month_pct_change",
                                            "yeartodate2022", "yeartodate2021","yeartodate_pct_change","ytd_2yr_pct_change",
                                            "ytd_12yr_pct_change","ytd_29yr_pct_change"), skip = 13) %>% head(17)
}

# Map a dataframe; function loops through the 'list' of imported Excel tables
# Appends each of the 'tables' within the list into a single df
precincts <- purrr::map_df(.x = filenames, .f = ~precinct_fun(.x), .id="source")

# trims the source file down to the precinct/place identifier, then renames field
precincts$source <- gsub(".xlsx","",precincts$source)
precincts$source <- gsub("data/source/weekly/cs-en-us-","",precincts$source)
precincts <- precincts %>% rename("precinct"="source")
# remove extraneous col
precincts <- precincts %>% select(-3)
# make numeric cols numeric
precincts[3:14] <- lapply(precincts[3:14],as.numeric)
# round specific pct calculated cols
precincts <- precincts %>% mutate(across(c(5,8,11:14), round, 1))
# add the latest date for data we have from NYPD from value scraped earlier
precincts$lastdate <- as.Date.character(update_date,"%m/%d/%Y")

precincts$crime <- case_when(precincts$crime == "Fel. Assault" ~ "Felony Assault",
                             precincts$crime == "TOTAL" ~ "Total Major Felonies",
                             precincts$crime == "Gr. Larceny" ~ "Grand Larceny",
                             precincts$crime == "G.L.A." ~ "Motor Vehicle Theft",
                             precincts$crime == "Transit" ~ "Transit System Crimes",
                             precincts$crime == "Housing" ~ "Housing System Crimes",
                             precincts$crime == "Misd. Assault" ~ "Misdemeanor Assault",
                             precincts$crime == "UCR Rape*" ~ "Rape (FBI/UCR Definition)",
                             precincts$crime == "Shooting Vic." ~ "Shooting Victims",
                             precincts$crime == "Shooting Inc." ~ "Shooting Incidents",
                             precincts$crime == "Petit Larceny" ~ "Petty Theft",
                             TRUE ~ str_to_title(precincts$crime, locale = "en"))
precincts$type <- case_when(precincts$crime == "Murder" ~ "Violent",
                            precincts$crime == "Rape" ~ "Violent",
                            precincts$crime == "Felony Assault" ~ "Violent",
                            precincts$crime == "Rape (FBI/UCR Definition)" ~ "Violent",
                            precincts$crime == "Misdemeanor Assault" ~ "Violent",
                            precincts$crime == "Motor Vehicle Theft" ~ "Property",
                            precincts$crime == "Grand Larceny" ~ "Property",
                            precincts$crime == "Petty Theft" ~ "Property",
                            precincts$crime == "Robbery" ~ "Property",
                            precincts$crime == "Burglary" ~ "Property",
                            precincts$crime == "Total Major Felonies" ~ "Combined Total",                            
                                          TRUE ~ "Other/Mixed Category")

# remove pct AND leading 0s from precinct ID field to match field in map file
precincts$precinct <- gsub("pct","",precincts$precinct)
precincts$precinct <- sub("^0+", "", precincts$precinct) 

# save a dated archive file as csv
write_csv(precincts,paste0("data/output/weekly/precincts",update_weeknum,".csv",sep=""))

# separate the files into precincts and the larger geography precinct_bureaus
citywide <- precincts %>% filter(str_detect(precinct,"city")) %>% 
  rename("city"="precinct")
precinct_bureaus <- precincts %>% filter(str_detect(precinct,"pb")) %>% 
  rename("precinct_bureau"="precinct")
precincts <- precincts %>% filter(!str_detect(precinct,"pb") & !str_detect(precinct,"city"))

# rename and reformat the precinct col to bureau name to match historic crime data files
precinct_bureaus$precinct_bureau <- case_when(precinct_bureaus$precinct_bureau == "pbsi" ~ "Staten Island",
                                              precinct_bureaus$precinct_bureau == "pbbn" ~ "Brooklyn North",
                                              precinct_bureaus$precinct_bureau == "pbbs" ~ "Brooklyn South",
                                              precinct_bureaus$precinct_bureau == "pbbx" ~ "Bronx",
                                              precinct_bureaus$precinct_bureau == "pbqs" ~ "Queens South",
                                              precinct_bureaus$precinct_bureau == "pbqn" ~ "Queens North",
                                              precinct_bureaus$precinct_bureau == "pbms" ~ "Manhattan South",
                                              precinct_bureaus$precinct_bureau == "pbmn" ~ "Manhattan North",
                                              TRUE ~ "Other")


### ADD ANNUAL ARCHIVE FILES
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

# Read in the citywide major felonies history file and format col names
citywide_major_felonies <- read_excel("data/source/annual/nyc_major_felonies.xls",
                                      skip = 2) %>% janitor::clean_names() 
# add a city row and re-order to match format of other files
citywide_major_felonies$city <- "city"
citywide_major_felonies <- citywide_major_felonies %>% select(24,1:23)
# only keep rows of data, dropping excess rows at bottom that are data footnotes
citywide_major_felonies <- citywide_major_felonies[1:8,]
# rename columns with year numbers to total00, total01, etc., format for consistent use in tracker
names(citywide_major_felonies) <- c("city","crime","total00",
                                    "total01","total02","total03",
                                    "total04","total05","total06",
                                    "total07","total08","total09",
                                    "total10","total11","total12",
                                    "total13","total14","total15",
                                    "total16","total17","total18",
                                    "total19","total20","total21")
citywide_major_felonies$crime <- case_when(citywide_major_felonies$crime == "MURDER & NON-NEGL. MANSLAUGHTER" ~ "Murder",
                                           citywide_major_felonies$crime == "TOTAL SEVEN MAJOR FELONY OFFENSES" ~ "Total Major Felonies",
                                           citywide_major_felonies$crime == "GRAND LARCENY OF MOTOR VEHICLE" ~ "Motor Vehicle Theft",
                                           TRUE ~ str_to_title(citywide_major_felonies$crime, locale = "en"))
citywide_major_felonies$type <- case_when(citywide_major_felonies$crime == "Murder" ~ "Violent",
                                          citywide_major_felonies$crime == "Rape" ~ "Violent",
                                          citywide_major_felonies$crime == "Felony Assault" ~ "Violent",
                                          citywide_major_felonies$crime == "Total Major Felonies" ~ "Combined Total",
                                          TRUE ~ "Property")



# Read in the precinct other felonies file and format col names
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

# merge into tables for tracker's major crimes history file
# we are going to combine annuals + last week, last month, ytd
precinct_crime <- left_join(precinct_major_felonies,precincts %>% select(1:15),by=c("precinct","crime"))
citywide_crime <- left_join(citywide_major_felonies,citywide %>% select(1:15),by=c("city","crime"))

# Read in and merge the precincts file with geography and populations
precincts_geo <- readRDS("scripts/rds/precincts.rds")
precinct_crime <- full_join(precincts_geo, precinct_crime, by="precinct")

# # # OPEN WORK IS PROCESSING SOME CHANGE FIELDS WE NEED FOR TRACKERS
# add last 12 mos calculations to do comparable annualized rates
precinct_crime$last12mos <- (precinct_crime$total21-precinct_crime$yeartodate2021)+precinct_crime$yeartodate2022

# add 3-year totals and annualized averages
precinct_crime$total_prior3years <- precinct_crime$total19+
                                            precinct_crime$total20+
                                            precinct_crime$total21
precinct_crime$avg_prior3years <- round(((precinct_crime$total19+
                                            precinct_crime$total20+
                                            precinct_crime$total21)/3),1)
# now add the increases or change percentages
precinct_crime$inc_19to21 <- round(precinct_crime$total21/precinct_crime$total19*100-100,1)
precinct_crime$inc_19tolast12 <- round(precinct_crime$last12mos/precinct_crime$total19*100-100,1)
precinct_crime$inc_21tolast12 <- round(precinct_crime$last12mos/precinct_crime$total21*100-100,1)
precinct_crime$inc_prior3yearavgtolast12 <- round((precinct_crime$last12mos/precinct_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
precinct_crime$rate19 <- round((precinct_crime$total19/precinct_crime$population)*100000,1)
precinct_crime$rate20 <- round((precinct_crime$total20/precinct_crime$population)*100000,1)
precinct_crime$rate21 <- round((precinct_crime$total21/precinct_crime$population)*100000,1)
precinct_crime$rate_last12 <- round((precinct_crime$last12mos/precinct_crime$population)*100000,1)
precinct_crime$rate_prior3years <- 
  round((precinct_crime$avg_prior3years/precinct_crime$population)*100000,1)

# create a quick long-term annual table
precinct_yearly <- precinct_crime %>% select(1,5:27,35) %>% st_drop_geometry()
write_csv(precinct_yearly,"data/output/yearly/precinct_yearly.csv")

# Now reduce the precinct down to just the columns we likely need for the tracker pages
precinct_crime <- precinct_crime %>% select(1,4,5,25:27,35:39,43:54,28,41)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
precinct_crime <- precinct_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
precinct_crime <- precinct_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))



# set value of nyc_population
nyc_population <- 8804190
# add last 12 mos calculations to citywide data to do comparable annualized rates
citywide_crime$last12mos <- (citywide_crime$total21-citywide_crime$yeartodate2021)+citywide_crime$yeartodate2022
# add 3-year annualized averages
citywide_crime$total_prior3years <- citywide_crime$total19+
                                            citywide_crime$total20+
                                            citywide_crime$total21
citywide_crime$avg_prior3years <- round(((citywide_crime$total19+
                                            citywide_crime$total20+
                                            citywide_crime$total21)/3),1)
# now add the increases or change percentages
citywide_crime$inc_19to21 <- round(citywide_crime$total21/citywide_crime$total19*100-100,1)
citywide_crime$inc_19tolast12 <- round(citywide_crime$last12mos/citywide_crime$total19*100-100,1)
citywide_crime$inc_21tolast12 <- round(citywide_crime$last12mos/citywide_crime$total21*100-100,1)
citywide_crime$inc_prior3yearavgtolast12 <- round((citywide_crime$last12mos/citywide_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
citywide_crime$rate19 <- round((citywide_crime$total19/nyc_population)*100000,1)
citywide_crime$rate20 <- round((citywide_crime$total20/nyc_population)*100000,1)
citywide_crime$rate21 <- round((citywide_crime$total21/nyc_population)*100000,1)
citywide_crime$rate_last12 <- round((citywide_crime$last12mos/nyc_population)*100000,1)
# 3 yr rate
citywide_crime$rate_prior3years <- 
  round((citywide_crime$avg_prior3years/nyc_population)*100000,1)

### OPEN WORK STOP POINT #####

# create a quick long-term annual table
citywide_yearly <- citywide_crime %>% select(2:24,32)
write_csv(citywide_yearly,"data/output/yearly/citywide_yearly.csv")

# add a series of csv tables for annual tracking of each crime with a tracker page
citywide_yearly %>% filter(crime=="Murder") %>% write_csv("data/output/yearly/murder_yearly.csv")
citywide_yearly %>% filter(crime=="Rape") %>% write_csv("data/output/yearly/rape_yearly.csv")
citywide_yearly %>% filter(crime=="Robbery") %>% write_csv("data/output/yearly/robbery_yearly.csv")
citywide_yearly %>% filter(crime=="Burglary") %>% write_csv("data/output/yearly/burglary_yearly.csv")
citywide_yearly %>% filter(crime=="Felony Assault") %>% write_csv("data/output/yearly/assault_yearly.csv")
citywide_yearly %>% filter(crime=="Grand Larceny") %>% write_csv("data/output/yearly/larceny_yearly.csv")
citywide_yearly %>% filter(crime=="Motor Vehicle Theft") %>% write_csv("data/output/yearly/autotheft_yearly.csv")

# Now make individual crime/beat geo files for each of the trackers' landing pages
# filter precinct versions - using beat instead of precinct just for code consistency
murders_beat <- precinct_crime %>% filter(crime=="Murder" & precinct!="DOC")
sexassaults_beat <- precinct_crime %>% filter(crime=="Rape" & precinct!="DOC")
robberies_beat <- precinct_crime %>% filter(crime=="Robbery" & precinct!="DOC")
assaults_beat <- precinct_crime %>% filter(crime=="Felony Assault" & precinct!="DOC")
burglaries_beat <- precinct_crime %>% filter(crime=="Burglary" & precinct!="DOC")
larcenies_beat <- precinct_crime %>% filter(crime=="Grand Larceny" & precinct!="DOC")
autothefts_beat <- precinct_crime %>% filter(crime=="Motor Vehicle Theft" & precinct!="DOC")
# filter citywide versions
murders_city <- citywide_crime %>% filter(crime=="Murder")
sexassaults_city <- citywide_crime %>% filter(crime=="Rape")
robberies_city <- citywide_crime %>% filter(crime=="Robbery")
assaults_city <- citywide_crime %>% filter(crime=="Felony Assault")
burglaries_city <- citywide_crime %>% filter(crime=="Burglary")
larcenies_city <- citywide_crime %>% filter(crime=="Grand Larceny")
autothefts_city <- citywide_crime %>% filter(crime=="Motor Vehicle Theft")

# make the death rate comparables file unique to this state
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="NY")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")

#### 
# Archive latest files as csv and rds store for use in trackers
# First save the weekly files as output csvs for others to use
write_csv(precinct_crime,"data/output/weekly/precinct_crime.csv")
write_csv(citywide_crime,"data/output/weekly/citywide_crime.csv")
# Archive a year's worth of week-numbered files from the weekly updates
write_csv(precinct_crime,paste0("data/output/archive/precinct_crime_week",update_weeknum,".csv"))
write_csv(citywide_crime,paste0("data/output/archive/citywide_crime_week",update_weeknum,".csv"))
# Now save the files needed for trackers into RDS store in scripts for GH Actions
# precinct versions
saveRDS(precinct_crime,"scripts/rds/precinct_crime.rds")
saveRDS(murders_beat,"scripts/rds/murders_beat.rds")
saveRDS(sexassaults_beat,"scripts/rds/sexassaults_beat.rds")
saveRDS(robberies_beat,"scripts/rds/robberies_beat.rds")
saveRDS(assaults_beat,"scripts/rds/assaults_beat.rds")
saveRDS(burglaries_beat,"scripts/rds/burglaries_beat.rds")
saveRDS(larcenies_beat,"scripts/rds/larcenies_beat.rds")
saveRDS(autothefts_beat,"scripts/rds/autothefts_beat.rds")
# city versions
saveRDS(citywide_crime,"scripts/rds/citywide_crime.rds")
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(larcenies_city,"scripts/rds/larcenies_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")