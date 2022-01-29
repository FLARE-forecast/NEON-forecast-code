##'
# Load in the required functions for processing the data
#renv::restore()
library(tidyverse)
library(lubridate)

lake_directory <- here::here()
message(lake_directory)

source(file.path(lake_directory, "R/buoy_qaqc.R"))
#source(file.path(lake_directory, "R/glmtools.R"))

configure_run_file <- "configure_run.yml"

##'
# Set up configurations for the data processing
config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

use_s3 <- TRUE

buoy_products <- c("DP1.20264.001",
                   "DP1.20252.001",
                   "DP1.20254.001")

neon_database <-  file.path(lake_directory, "data_raw","neonstore")

if (file.exists(file.path(neon_database))){
  Sys.setenv("NEONSTORE_DB" = neon_database)
  Sys.setenv("NEONSTORE_HOME" = neon_database)
  neonstore::neon_dir()
} else {
  dir.create(neon_database)
  Sys.setenv("NEONSTORE_HOME" = neon_database)
  Sys.setenv("NEONSTORE_DB" = neon_database)
  neonstore::neon_dir()
}

download_neon_files(siteID = config$location$site_id,
                    buoy_products = buoy_products,
                    start_date = NA,
                    raw_data_directory = config$file_path$data_directory)

##'
# Download the latest "early release" data from Bobby Hensley at NEON

prop_neon <- readr::read_csv("https://raw.githubusercontent.com/FLARE-forecast/NEON-proprietary-data/master/surface_sonde_NEON_raw.csv")

##'
# Process the NEON data for the site selected in the original .yml file

cleaned_insitu_file <- buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
                                 prop_neon = prop_neon,
                                 input_file_tz = "UTC",
                                 local_tzone = "UTC",
                                 forecast_site = config$location$site_id,
                                 processed_filename = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                                 depth_bins = config$default_init$temp_depths)
##'
# Stack first day to use as met 'obs' for the forecasts.

message("Successfully generated targets")

FLAREr::put_targets(site_id = config$location$site_id,
                    cleaned_insitu_file,
                    use_s3 = config$run_config$use_s3)

message("Successfully moved targets to s3 bucket")
