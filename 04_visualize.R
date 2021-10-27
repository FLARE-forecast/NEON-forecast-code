renv::restore()

library(tidyverse)
library(lubridate)

lake_directory <- here::here()

source(file.path(lake_directory,"R/post_forecast_functions/plotting.R"))

s3_mode <- TRUE

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
  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
  if(!is.na(run_config$restart_file)){
    restart_file <- basename(run_config$restart_file)
  }else{
    restart_file <- NA
  }
  if(!is.na(restart_file)){
    aws.s3::save_object(object = file.path(forecast_site, restart_file),
                        bucket = "forecasts",
                        file = file.path(lake_directory, "forecasts", restart_file))
    restart_file <- basename(run_config$restart_file)
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", restart_file)
  }
  if(!is.na(run_config$restart_file)){
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", restart_file)
  }
  config$run_config <- run_config
}else{
  if(!is.na(run_config$restart_file)){
    file.copy(from = run_config$restart_file, to = config$file_path$forecast_output_directory)
  }
}

target_directory <- file.path(lake_directory, "data_processed")

if(s3_mode){
  aws.s3::save_object(object = file.path(forecast_site, paste0(forecast_site, "-targets-insitu.csv")),
                      bucket = "targets",
                      file = file.path(target_directory, paste0(forecast_site, "-targets-insitu.csv")))
}

pdf_file <- FLAREr::plotting_general_2(file_name = config$run_config$restart_file,
                                     target_file = file.path(target_directory, paste0(forecast_site, "-targets-insitu.csv")))

if(s3_mode){
  success <- aws.s3::put_object(file = pdf_file, object = file.path(forecast_site, basename(pdf_file)), bucket = "analysis")
  if(success){
    unlink(pdf_file)
  }
}

if(s3_mode){
  unlink(file.path(target_directory, paste0(forecast_site, "-targets-insitu.csv")))
  unlink(restart_file)
}

