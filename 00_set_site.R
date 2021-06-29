
##' Download the packages required to process data and run FLAREr
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, naniar, Amelia, dplyr,
               mice, FactoMineR, broom, aws.s3, scattermore, reshape2, duckdb)

##' Manually download packages from Github
remotes::install_github("cboettig/neonstore", force = T)
remotes::install_github("eco4cast/EFIstandards", force = T)
remotes::install_github("FLARE-forecast/noaaGEFSpoint", force = T)
remotes::install_github("FLARE-forecast/GLM3r", force = T)
remotes::install_github("FLARE-forecast/FLAREr", force = T)


##' Set up Lake Barco
siteID = c("BARC", "SUGG", "CRAM", "LIRO", "PRPO", "PRLA")
siteID_neon = c("BARC", "SUGG", "CRAM", "LIRO", "PRPO", "PRLA", "OSBS", "UNDE", "DCFS")
ECtower = c("OSBS", "UNDE", "DCFS")

##' Set up the directories and databases for processing files
lake_directory <- getwd()
noaa_directory <- file.path(getwd(), "data_processed", "NOAA_data")
neon_database <- file.path("/Volumes/Seagate Backup Plus Drive/neonstore")
noaa_data_location <- file.path(getwd(),"data","NOAA_data","noaa","NOAAGEFS_1hr",siteID)
forecast_location <- file.path(getwd(), "flare_tempdir")

##' Choose the site
# forecast_site = "BARC"
# forecast_site = "SUGG"
# forecast_site = "CRAM"
# forecast_site = "LIRO"
# forecast_site = "PRPO"
forecast_site = "PRLA"

# Specify the products to download
products = c("DP1.00098.001",
             "DP1.00002.001",
             "DP1.00023.001",
             "DP1.00006.001",
             "DP1.00001.001",
             "DP1.00004.001")

buoy_products = c("DP1.20264.001",
                  "DP1.20252.001",
                  "DP1.20254.001")
