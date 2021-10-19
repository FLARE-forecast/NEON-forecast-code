
check_noaa_present <- function(lake_directory){
  config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))
  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  config$run_config <- run_config


  library(tidyverse)
  library(lubridate)

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }
  forecast_hour <- lubridate::hour(forecast_start_datetime)
  if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
  noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)


  forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)

  noaa_forecasts_ready <- FALSE
  if(length(forecast_files) == 31){
    noaa_forecasts_ready <- TRUE
  }else if(length(forecast_files) != 31){
    if(Sys.Date() > as_date(run_config$forecast_start_datetime)){
      if(update_run_config){
        run_config$forecast_start_datetime <- as.character(paste0((Sys.Date())," 00:00:00"))
        yaml::write_yaml(run_config, file = file.path(config$file_path$run_config))
      }
    }

  }
  return(noaa_forecasts_ready)
}
