library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
configure_run_file <- "configure_run.yml"
update_run_config <- TRUE
config_set_name <- "glm_prpo"

run_config <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name,"configure_run.yml"))
config <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name,run_config$configure_flare))
forecast_site <- config$location$site_id

message("Checking for NOAA forecasts")
noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                         configure_run_file,
                                         config_set_name = config_set_name)

if(!noaa_ready){
  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  lapsed_time <- as.numeric(as.duration(Sys.time() - lubridate::as_datetime(config$run_config$forecast_start_datetime)))/(60*60)
  if(lapsed_time > 24){
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file = NA, new_horizon = NA, day_advance = 1, new_start_datetime = FALSE)
  }
}

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
if(!is.null(config$run_config$forecast_fails)){
  if(config$run_config$forecast_fails > 0){
    config$run_config$forecast_fails <- 0
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file = NA, new_horizon = NA, day_advance = 1, new_start_datetime = FALSE)
    noaa_ready <- FLAREr::check_noaa_present(lake_directory,
                                             configure_run_file,
                                             config_set_name = config_set_name)
  }
}


if(noaa_ready){

  if(!is.null(config$run_config$forecast_fails)){
    config$run_config$forecast_fails <- config$run_config$forecast_fails + 1
  }else{
    config$run_config$forecast_fails <- 1
  }
  FLAREr::update_run_config(config, lake_directory, configure_run_file, new_start_datetime = FALSE)

  message("Generating targets")
  source(file.path("workflows", config_set_name, "01_generate_targets.R"))

  setwd(lake_directory)

  message("Generating inflow forecast")
  #source(file.path("workflows", config_set_name, "02_run_inflow_forecast.R"))

  setwd(lake_directory)

  message("Generating forecast")
  source(file.path("workflows", config_set_name, "03_run_flarer_forecast.R"))

  setwd(lake_directory)

  message("Generating plots")
  source(file.path("workflows", config_set_name, "04_visualize.R"))

  config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name= config_set_name)
  config$run_config$forecast_fails <- 0
  FLAREr::update_run_config(config, lake_directory, configure_run_file, new_start_datetime = FALSE)

}
