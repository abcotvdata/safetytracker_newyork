library(tidyverse)
library(readxl)
library(purr)
library(lubridate)
library(sf)
library(zoo)



# Separate file mass downloads the precinct data files from NYPD
# They're updated through Sunday at some point each week; confirming date

# source("massdownloadprecincts.R")

# add citywide files; updated by NYPD on same schedule
download.file("https://www1.nyc.gov/assets/nypd/downloads/excel/crime_statistics/cs-en-us-city.xlsx",
              "nyc_2022_citywide.xlsx")

# quick import of one file, to get single row that contains the week
# extract from that string two values: text noting week of the report and the latest date
grabdatedf <- read_excel("precinctdata/cs-en-us-040pct.xlsx")[c(8),c(3)]
update_week <- as.character(grabdatedf[1,1])
update_date <- sub(".*\\s", "", trimws(update_week))
rm(grabdatedf)

# make list of filenames of all the mass-downloaded Excel files for loop
filenames <- list.files(path = "precinctdata", pattern = '.xlsx', full.names = TRUE) %>% set_names(filenames)

# write a function that will import and process those files
precinct_fun <- function(filename) {

precinct_process <- read_excel(filename, 
                              col_types = "text",
                              col_names = c("crime","delete","lastweek2022", "lastweek2021",
                                            "week_pct_change", "lastmonth2022","lastmonth2021","month_pct_change",
                                            "yeartodate2022", "yeartodate2021","yeartodate_pct_change","ytd_2yr_pct_change",
                                            "ytd_12yr_pct_change","ytd_29yr_pct_change"), skip = 13) %>% head(17)
}

# mapping a dataframe using function to loop through the list of Excels
# appends the results of each file to the dataframe we've built
precincts <- purrr::map_df(.x = filenames, .f = ~precinct_fun(.x), .id="source")


# trims the source file down to the precinct identifier, then renames field
precincts$source <- gsub(".xlsx","",precincts$source)
precincts$source <- gsub("precinctdata/cs-en-us-","",precincts$source)
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

# save a dated archive file
write_csv(precincts,paste("precincts",Sys.Date(),sep=""))

# separate the files into precincts and the larger geography precinct_bureaus
precinct_bureaus <- precincts %>% filter(str_detect(precinct,"pb")) %>% rename("precinct_bureau"="precinct")
precincts <- precincts %>% filter(!str_detect(precinct,"pb"))

# build out precinct major crimes history file
# we are going for annuals + last week, last month, ytd
precinct_crime <- left_join(precinct_major_felonies,precincts %>% ,by=c("precinct","crime"))


