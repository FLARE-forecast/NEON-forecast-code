##' Load in the required functions for processing the data
source(file.path(lake_directory, "R/process_functions/average_historical_stacked.R"))
source(file.path(lake_directory, "R/process_functions/buoy_qaqc.R"))


##' Set up configurations for the data processing
lake_directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "data_processed","NOAA_data","noaa",config$met$forecast_met_model)
config$run_config <- run_config

##' Download the latest "early release" data from Bobby Hensley at NEON
prop_neon <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/NEON-proprietary-data/master/surface_sonde_NEON_raw.csv")

##' Process the NEON data for the site selected in the original .yml file
buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
          prop_neon = prop_neon,
          input_file_tz = "UTC",
          local_tzone = "UTC",
          forecast_site = forecast_site)

##' get NOAA met forecasts and stack first day to use as met 'obs'
dates <- seq.Date(as.Date('2021-04-13'), as.Date(config$run_config$forecast_start_datetime), by = 'day') # cycle through historical dates
cycle <- c('00','06','12','18')
outfile <- config$file_path$qaqc_data_directory

lake_directory <- here::here()


average_stacked_forecasts(forecast_dates <- seq.Date(as.Date('2021-04-13'), as.Date('2021-05-30'), by = 'day'), # cycle through historical dates
                          site <- siteID, #four digit name in lowercase
                          noaa_stacked_directory <- file.path(lake_directory, "data_raw", "NOAA_data", "noaa", "NOAAGEFS_1hr_stacked"),
                          output_directory <- file.path(lake_directory, "data_processed"),
                          outfile_name = paste0("observed-met_",config$location$site_id),
                          noaa_hour = 1)

