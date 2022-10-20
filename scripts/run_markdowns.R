library(rmarkdown)

# Code to build each of the trackers
# Includes loading pre-processed and stored dfs
# Grouped by each page to allow for individual or mass processing

# MURDERS
# Load RDS
murders_beat <- readRDS("scripts/rds/murders_beat.rds")
murders_city <- readRDS("scripts/rds/murders_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'New_York_Safety_Tracker.html')

# BURGLARIES
# Load RDS
burglaries_beat <- readRDS("scripts/rds/burglaries_beat.rds")
burglaries_city <- readRDS("scripts/rds/burglaries_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker_Burglaries.Rmd', 
                  output_dir = "docs",
                  output_file = "New_York_Safety_Tracker_Burglaries.html")

# LARCENIES
# Load RDS
larcenies_beat <- readRDS("scripts/rds/larcenies_beat.rds")
larcenies_city <- readRDS("scripts/rds/larcenies_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker_Larcenies.Rmd', 
                  output_dir = "docs",
                  output_file = 'New_York_Safety_Tracker_Larcenies.html')

# AUTO THEFTS
# Load RDS
autothefts_beat <- readRDS("scripts/rds/autothefts_beat.rds")
autothefts_city <- readRDS("scripts/rds/autothefts_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker_AutoThefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'New_York_Safety_Tracker_AutoThefts.html')

# ROBBERIES
# Load RDS
robberies_beat <- readRDS("scripts/rds/robberies_beat.rds")
robberies_city <- readRDS("scripts/rds/robberies_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker_Robberies.Rmd', 
                  output_dir = "docs",
                  output_file = 'New_York_Safety_Tracker_Robberies.html')

# ASSAULTS
# Load RDS
assaults_beat <- readRDS("scripts/rds/assaults_beat.rds")
assaults_city <- readRDS("scripts/rds/assaults_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker_Assaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'New_York_Safety_Tracker_Assaults.html')

# SEXUAL ASSAULTS
# Load RDS
sexassaults_beat <- readRDS("scripts/rds/sexassaults_beat.rds")
sexassaults_city <- readRDS("scripts/rds/sexassaults_city.rds")
# Render page
rmarkdown::render('scripts/New_York_Safety_Tracker_SexualAssaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'New_York_Safety_Tracker_SexualAssaults.html')

