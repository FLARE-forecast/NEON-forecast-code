#renv::restore()

library(tidyverse)
library(lubridate)

lake_directory <- here::here()

s3_mode <- TRUE
bucket <- "drivers"
update_run_config <- TRUE

source(file.path(lake_directory, "R/download_functions/s3_functions.R"))

if(file.exists("~/.aws")){
  warning(paste("Detected existing AWS credentials file in ~/.aws,",
                "Consider renaming these so that automated upload will work"))
}

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

if(!exists("update_run_config")){
  stop("Missing update_run_config variable")
}

run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
forecast_site <- run_config$forecast_site
sim_name <- run_config$sim_name
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))

#Get updated run_config from bucket if one exists
if(s3_mode){
  restart_exists <- aws.s3::object_exists(object = file.path(forecast_site, sim_name, "configure_run.yml"), bucket = "restart")
  if(restart_exists){
    aws.s3::save_object(object = file.path(forecast_site, sim_name, "configure_run.yml"), bucket = "restart", file = file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
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

#Note: lake_directory need to be set prior to running this script
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$run_config <- file.path(lake_directory, "configuration", "FLAREr","configure_run.yml")
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts")

#THIS SITS OUTSIDE THE BUCKET AND REPO. NEED TO MOVE
config$file_path$noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")

# Set up timings
#Weather Drivers
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

if(config$run_config$forecast_horizon > 0){
  noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
}else{
  noaa_forecast_path <- NULL
}

if(s3_mode){
  aws.s3::save_object(object = file.path(forecast_site, paste0(forecast_site, "-targets-insitu.csv")), bucket = "targets", file = file.path(config$file_path$qaqc_data_directory, paste0(forecast_site, "-targets-insitu.csv")))
  #aws.s3::save_object(object = file.path(forecast_site, paste0(forecast_site, "-targets-inflow.csv")), bucket = "targets", file = file.path(config$file_path$qaqc_data_directory, paste0(forecast_site, "-targets-inflow.csv")))
  aws.s3::save_object(object = file.path(forecast_site, paste0("observed-met-noaa_",forecast_site,".nc")), bucket = "targets", file = file.path(config$file_path$qaqc_data_directory, paste0("observed-met-noaa_",forecast_site,".nc")))

  if(config$run_config$forecast_horizon > 0){
    noaa_forecast_path <- file.path(lake_directory,"drivers/noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)

    Sys.setenv("AWS_DEFAULT_REGION" = "data",
               "AWS_S3_ENDPOINT" = "ecoforecast.org",
               "AWS_ACCESS_KEY_ID" = "",
               "AWS_SECRET_ACCESS_KEY"= "")
    download_s3_objects(lake_directory,
                        bucket = "drivers",
                        prefix = file.path("noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour))
    readRenviron(path = "~/.Renviron")
  }
}else{
  if(config$run_config$forecast_horizon > 0){
    local_noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
    noaa_forecast_path <- file.path(lake_directory, "drivers/noaa", config$met$forecast_met_model,config$location$site_id,lubridate::as_date(forecast_start_datetime),forecast_hour)
    files <- list.files(noaa_forecast_path, full.names = TRUE)
    for(i in 1:length(files)){
      dir.create(noaa_forecast_path)
      file.copy(from = files[i], to = noaa_forecast_path)
    }
  }

  forecast_files <- list.files(noaa_forecast_path, full.names = TRUE)

}

if(!dir.exists(config$file_path$execute_directory)){
  dir.create(config$file_path$execute_directory)
}

pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$states_config_file), col_types = readr::cols())


#Download and process observations (already done)

cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,paste0(forecast_site, "-targets-insitu.csv"))
cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, paste0(forecast_site, "-targets-inflow.csv"))
observed_met_file <- file.path(config$file_path$qaqc_data_directory, paste0("observed-met-noaa_",forecast_site,".nc"))

met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                          out_dir = config$file_path$execute_directory,
                                          forecast_dir = noaa_forecast_path,
                                          config = config)

met_file_names <- met_out$met_file_names
historical_met_error <- met_out$historical_met_error

#Inflow and Outflow

inflow_file_names <- NULL
outflow_file_names <- NULL
management <- NULL


#Create observation matrix
obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                 obs_config,
                                 config)

states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

model_sd <- FLAREr::initiate_model_error(config, states_config)

init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            restart_file = config$run_config$restart_file,
                                            historical_met_error = met_out$historical_met_error)
#Run EnKF
da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                              pars_init = init$pars,
                                              aux_states_init = init$aux_states_init,
                                              obs = obs,
                                              obs_sd = obs_config$obs_sd,
                                              model_sd = model_sd,
                                              working_directory = config$file_path$execute_directory,
                                              met_file_names = met_out$filenames,
                                              inflow_file_names = inflow_file_names,
                                              outflow_file_names = outflow_file_names,
                                              config = config,
                                              pars_config = pars_config,
                                              states_config = states_config,
                                              obs_config = obs_config,
                                              management,
                                              da_method = config$da_setup$da_method,
                                              par_fit_method = config$da_setup$par_fit_method)

# Save forecast

saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = config$file_path$forecast_output_directory,
                                            use_short_filename = TRUE)

#Create EML Metadata
eml_file_name <- FLAREr::create_flare_metadata(file_name = saved_file,
                                               da_forecast_output = da_forecast_output)

#Clean up temp files and large objects in memory
#unlink(config$file_path$execute_directory, recursive = TRUE)

if(s3_mode){
  unlink(noaa_forecast_path, recursive = TRUE)
  success <- aws.s3::put_object(file = saved_file, object = file.path(forecast_site, basename(saved_file)), bucket = "forecasts")
  if(success){
    unlink(saved_file)
  }
  success <- aws.s3::put_object(file = eml_file_name, object = file.path(forecast_site, basename(eml_file_name)), bucket = "forecasts")
  if(success){
    unlink(eml_file_name)
  }
}

rm(da_forecast_output)
gc()

if(update_run_config){
  run_config$start_datetime <- run_config$forecast_start_datetime
  if(run_config$forecast_horizon == 0){
    run_config$forecast_horizon <- 16
  }
  run_config$forecast_start_datetime <- as.character(lubridate::as_datetime(run_config$forecast_start_datetime) + lubridate::days(1))
  if(lubridate::hour(run_config$forecast_start_datetime) == 0){
    run_config$forecast_start_datetime <- paste(run_config$forecast_start_datetime, "00:00:00")
  }
  run_config$restart_file <- saved_file
  yaml::write_yaml(run_config, file = file.path(config$file_path$run_config))
  if(s3_mode){
    aws.s3::put_object(file = file.path(lake_directory,"configuration","FLAREr","configure_run.yml"), object = file.path(forecast_site, sim_name, "configure_run.yml"), bucket = "restart")
  }
}

message(paste0("successfully generated flare forecats for: ", run_config$restart_file))
