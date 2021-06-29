

############## set up config directories ##############
lake_directory <- getwd() # Captures the project directory
forecast_site = forecast_site
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))

# Set working directories for your system
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "data_processed","NOAA_data","noaa",config$met$forecast_met_model)
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$run_config <- file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "run_configuration_",forecast_site,".yml"))
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecast_output")
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "run_configuration_",forecast_site,".yml")))
config$run_config <- run_config

# Create directories if not present
if(!dir.exists(config$file_path$execute_directory)) {
  dir.create(config$file_path$execute_directory)
}
if(!dir.exists(config$file_path$forecast_output_directory)) {
  dir.create(config$file_path$forecast_output_directory)
}


observed_met_file <- file.path(paste0(config$file_path$qaqc_data_directory, "/observed-met_",forecast_site,".nc"))

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

noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$location$site_id,
                                lubridate::as_date(forecast_start_datetime), "00")


# convert NOAA forecasts to GLM format
met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                          out_dir = config$file_path$execute_directory,
                                          forecast_dir = noaa_forecast_path,
                                          config = config)

met_file_names <- met_out$filenames
historical_met_error <- met_out$historical_met_error

#Create observation matrix
cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,paste0("observations_postQAQC_long_",forecast_site,".csv"))
obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$obs_config_file), col_types = readr::cols())

obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                 obs_config,
                                 config)

states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$states_config_file), col_types = readr::cols())
states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

model_sd <- FLAREr::initiate_model_error(config = config, states_config = states_config)

# generate initial conditions
pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
#config$da_setup$ensemble_size <- 27
init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            restart_file = config$run_config$restart_file,
                                            historical_met_error = met_out$historical_met_error)

#Run EnKF
# states_init = init$states
# pars_init = init$pars
# aux_states_init = init$aux_states_init
# obs = obs
# obs_sd = obs_config$obs_sd
# model_sd = model_sd
# working_directory = config$file_path$execute_directory
# met_file_names = met_out$filenames
# config = config
# pars_config = pars_config
# states_config = states_config
# obs_config = obs_config
# da_method = config$da_setup$da_method
# par_fit_method = config$da_setup$par_fit_method

da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                              pars_init = init$pars,
                                              aux_states_init = init$aux_states_init,
                                              obs = obs,
                                              obs_sd = obs_config$obs_sd,
                                              model_sd = model_sd,
                                              working_directory = config$file_path$execute_directory,
                                              met_file_names = met_out$filenames,
                                              config = config,
                                              pars_config = pars_config,
                                              states_config = states_config,
                                              obs_config = obs_config,
                                              da_method = config$da_setup$da_method,
                                              par_fit_method = config$da_setup$par_fit_method)

# Save forecast
saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = config$file_path$forecast_output_directory)
