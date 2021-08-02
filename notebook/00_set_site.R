#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*
#~*#~*#~*#~*#~*#~* PLEASE READ THIS #~*#~*#~*#~*#~*#~*#~*#~*#~*
#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*

# Before running through and hitting source for each script. Please refer to the
# run_configuration.yml file located in "NEON-forecast-code/configuration/FLAREr/...

# You can open the file directly in R by clicking the file in your panel. When opened,
# two lines of the YAML need to be adapted to meet your needs. This includes...

# sim_name: BARC_LAKE #SUGG_LAKE CRAM_LAKE LIRO_LAKE PRLA_LAKE PRPO_LAKE

# and

# forecast_site: BARC #SUGG CRAM LIRO PRLA PRPO

# As a default, it will run BARC_LAKE and BARC. However, if you wish to run another
# site just move that site in place of BARC_LAKE and BARC and make sure both names are not
# hashed out. Make sure NOT to mix up sites (e.g., no SUGG_LAKE & CRAM) and DO NOT
# change these name conventions. Doing so will propagate errors throughout this whole workflow.

#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*
#~*#~*#~*#~*#~*#~* THANKS FOR READING #~*#~*#~*#~*#~*#~*#~*#~*#
#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*

##' Download the packages required to process data and run FLAREr
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, naniar, Amelia, dplyr,
               mice, FactoMineR, broom, aws.s3, scattermore,
               reshape2, duckdb, RCurl, here)

##' Just in case, run here function from here package to set your project root as the wd
setwd(here::here())

##' Manually download packages from Github
remotes::install_github("cboettig/neonstore", force = F)
remotes::install_github("eco4cast/EFIstandards", force = F)
remotes::install_github("rqthomas/noaaGEFSpoint", force = F)
remotes::install_github("FLARE-forecast/GLM3r", force = F)
remotes::install_github("FLARE-forecast/FLAREr", force = F)

##' Set up the sites for downloading
siteID = c("BARC", "SUGG", "CRAM", "LIRO", "PRPO", "PRLA")
siteID_neon = c("BARC", "SUGG", "CRAM", "LIRO", "PRPO", "PRLA", "OSBS", "UNDE", "DCFS")
ECtower = c("OSBS", "UNDE", "DCFS")

##' Set up the directories and databases for processing files
lake_directory <- getwd()
noaa_directory <- file.path(lake_directory, "data_processed", "NOAA_data")
neon_database <- file.path("/Volumes/Seagate Backup Plus Drive/neonstore")
noaa_data_location <- file.path(lake_directory,"data","NOAA_data","noaa","NOAAGEFS_1hr",siteID)
forecast_location <- file.path(lake_directory, "flare_tempdir")

##' Set up the NEON site that you wish to forecast

# NOTE: you will need to update this .yml file directly to set the NEON site you wish to forecast
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "run_configuration.yml")))
forecast_site <- run_config$forecast_site


##' Specify the NEON products to download
# Meteorological products
# products = c("DP1.00098.001", # Relative humidity
#              "DP1.00002.001", # Air temperature
#              "DP1.00023.001", # Shortwave and longwave radiation
#              "DP1.00006.001", # Precipitation
#              "DP1.00001.001", # Wind speed and direction
#              "DP1.00004.001") # Barometric pressure

# Temperature products
buoy_products = c("DP1.20264.001", # Buoy
                  "DP1.20252.001", # Sonde profiles
                  "DP1.20254.001") # Secchi

