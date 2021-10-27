
check_noaa_present <- function(lake_directory, s3_mode, forecast_site, configuration_file){

  config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configuration_file))

  if(s3_mode){
    restart_exists <- aws.s3::object_exists(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart")
    if(restart_exists){
      aws.s3::save_object(object = file.path(forecast_site, "configure_run.yml"), bucket = "restart", file = file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
    }
    run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  }else{
    run_config <- yaml::read_yaml(config$file_path$run_config)
  }

  start_datetime <- lubridate::as_datetime(run_config$start_datetime)
  if(is.na(run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(run_config$forecast_horizon)
  }
  forecast_hour <- lubridate::hour(forecast_start_datetime)
  if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
  if(run_config$forecast_horizon > 0){
    noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
  }else{
    noaa_forecast_path <- NULL
  }


  if(run_config$forecast_horizon > 0){
    if(s3_mode){
      Sys.setenv("AWS_DEFAULT_REGION" = "data",
                 "AWS_S3_ENDPOINT" = "ecoforecast.org",
                 "AWS_ACCESS_KEY_ID" = "",
                 "AWS_SECRET_ACCESS_KEY"= "")
      noaa_files = aws.s3::get_bucket(bucket = "drivers", prefix = file.path("noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour))
      readRenviron(path = "~/.Renviron")
      noaa_forecast_path <- file.path(lake_directory,"drivers/noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
      keys <- vapply(noaa_files, `[[`, "", "Key", USE.NAMES = FALSE)
      empty <- grepl("/$", keys)
      forecast_files <- keys[!empty]
    }else{
      if(run_config$forecast_horizon > 0){
        local_noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
        noaa_forecast_path <- file.path(lake_directory, "drivers/noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
        forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)
      }
    }
    noaa_forecasts_ready <- FALSE
  }else{
    forecast_files <- NULL
    noaa_forecasts_ready <- TRUE
  }

  if(length(forecast_files) == 31){
    noaa_forecasts_ready <- TRUE
  }else{
    if(run_config$forecast_horizon > 0){
      message(paste0("waiting for NOAA forecast: ", run_config$forecast_start_datetime))
    }
  }
  #else if(length(forecast_files) != 31){
  # if(Sys.Date() > as_date(run_config$forecast_start_datetime)){
  #    if(update_run_config){
  #      run_config$forecast_start_datetime <- as.character(paste0((Sys.Date())," 00:00:00"))
  #      yaml::write_yaml(run_config, file = file.path(config$file_path$run_config))
  #    }
  #}
  #}
  return(noaa_forecasts_ready)
}
