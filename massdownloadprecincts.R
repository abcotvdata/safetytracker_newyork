library(rvest)
library(tidyverse)
library(readxl)
library(XML)

# The source of the NYPD's precinct level crime data is here at this link
# Second url is just a sample of the xls file naming convention they're using
# https://www1.nyc.gov/site/nypd/stats/crime-statistics/borough-and-precinct-crime-stats.page
# https://www1.nyc.gov/assets/nypd/downloads/excel/crime_statistics/cs-en-us-114pct.xlsx


# Set url for the main page where the links are based
url <- "https://www1.nyc.gov/site/nypd/stats/crime-statistics/borough-and-precinct-crime-stats.page"
# Several quick steps: start with scraping all links on the page
links <- readLines(url) %>% getHTMLLinks
# Extract a list of just the links that include the Excel files
xlsxFiles <- grep("\\.xlsx", links)
# Replaces the list of values with just the desired Excel links
links <- links[xlsxFiles]
# Prepend the urls in the list to get full urls for batch download
links <- paste0("https://www1.nyc.gov",links,sep="")
# Run function to download every file from the list of urls
# into new directory called precinctdata
for (link in links) {
  download.file(link, destfile = paste0("precinctdata/",basename(link),sep="")) }
  

