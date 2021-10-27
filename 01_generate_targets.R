##'
# Load in the required functions for processing the data
renv::restore()
s3_mode <- TRUE

library(tidyverse)
library(lubridate)

lake_directory <- here::here()
message(lake_directory)

source(file.path(lake_directory, "R/process_functions/average_historical_stacked.R"))
source(file.path(lake_directory, "R/process_functions/buoy_qaqc.R"))
source(file.path(lake_directory, "R/process_functions/glmtools.R"))
source(file.path(lake_directory, "R/download_functions/NOAA_downloads.R"))
source(file.path(lake_directory, "R/download_functions/NEON_downloads.R"))
source(file.path(lake_directory, "R/download_functions/s3_functions.R"))

##'
# Set up configurations for the data processing

run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))


#Get updated run_config from bucket if one exists
if(s3_mode){
  restart_exists <- aws.s3::object_exists(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart")
  if(restart_exists){
    aws.s3::save_object(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart", file = file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  }
  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  forecast_site <- run_config$forecast_site
}

config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "drivers", "noaa")
raw_data_directory <- file.path(lake_directory, "data_raw")
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

download_neon_files(siteID = forecast_site,
                    buoy_products = buoy_products,
                    start_date = as.Date("2021-01-01"),
                    raw_data_directory = raw_data_directory)

##'
# Download the latest "early release" data from Bobby Hensley at NEON

prop_neon <- readr::read_csv("https://raw.githubusercontent.com/FLARE-forecast/NEON-proprietary-data/master/surface_sonde_NEON_raw.csv")

##'
# Process the NEON data for the site selected in the original .yml file

processed_filename <- file.path(config$file_path$qaqc_data_directory, paste0(forecast_site, "-targets-insitu.csv"))
buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
          prop_neon = prop_neon,
          input_file_tz = "UTC",
          local_tzone = "UTC",
          forecast_site = forecast_site,
          processed_filename = processed_filename)

##'
# Stack first day to use as met 'obs' for the forecasts.

#dates <- list.files(path = file.path(config$file_path$noaa_directory, config$met$forecast_met_model, siteID))

Sys.setenv("AWS_DEFAULT_REGION" = "data",
           "AWS_S3_ENDPOINT" = "ecoforecast.org",
           "AWS_ACCESS_KEY_ID" = "",
           "AWS_SECRET_ACCESS_KEY"= "")
download_s3_objects(lake_directory, bucket = "drivers", prefix = file.path("noaa", "NOAAGEFS_1hr_stacked", forecast_site))
readRenviron(path = "~/.Renviron")


average_stacked_forecasts(forecast_dates = seq.Date(as.Date(run_config$start_datetime), as.Date(run_config$forecast_start_datetime), by = 'day'), # cycle through historical dates
                          site = forecast_site, #four digit name in lowercase
                          noaa_stacked_directory = file.path(lake_directory, "drivers", "noaa", "NOAAGEFS_1hr_stacked"),
                          output_directory = file.path(lake_directory, "data_processed"),
                          outfile_name = paste0("observed-met-noaa_",forecast_site, ".nc"),
                          noaa_hour = 1)

if(s3_mode){
  aws.s3::put_object(file = processed_filename, object = file.path(forecast_site, paste0(forecast_site, "-targets-insitu.csv")), bucket = "targets")
  aws.s3::put_object(file = file.path(config$file_path$qaqc_data_directory, paste0("observed-met-noaa_",forecast_site, ".nc")), object = file.path(forecast_site, paste0("observed-met-noaa_",forecast_site,".nc")), bucket = "targets")
}
