source(file.path(lake_directory, "R/process_functions/met_qaqc.R"))
source(file.path(lake_directory, "R/process_functions/buoy_qaqc.R"))


buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
          realtime_sonde_file = file.path(lake_directory,"data_raw","surface_sonde_NEON_raw.csv"),
          input_file_tz = "UTC",
          local_tzone = "UTC",
          forecast_site = forecast_site)

lake_directory <- getwd()
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "data_processed","NOAA_data","noaa",config$met$forecast_met_model)
#
# # set up run config settings
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "run_configuration_",forecast_site,".yml")))
config$run_config <- run_config

# get NOAA met forecasts and stack first day to use as met 'obs'
dates <- seq.Date(as.Date('2021-04-13'), as.Date(config$run_config$forecast_start_datetime), by = 'day') # cycle through historical dates
cycle <- c('00','06','12','18')
outfile <- config$file_path$qaqc_data_directory
stack_noaa_forecasts(dates = dates,
                     outfile = outfile,
                     config = config)
