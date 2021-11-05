##'
# Source the plotting function

source(file.path(lake_directory, "R/post_forecast_functions/plotting.R"))

##'
# Set up configurations for the forecasts

lake_directory <- here::here()
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_run.yml")))
forecast_site <- run_config$forecast_site
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "drivers", "noaa", config$met$forecast_met_model)
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts", forecast_site)
config$run_config <- run_config



##'
# Create directories if not present

if(!dir.exists(config$file_path$execute_directory)) {
  dir.create(config$file_path$execute_directory)
}
if(!dir.exists(config$file_path$forecast_output_directory)) {
  dir.create(config$file_path$forecast_output_directory)
}

##'
# Configure the NOAA met data from data processing

observed_met_file <- file.path(paste0(config$file_path$qaqc_data_directory, "/observed-met_",forecast_site,".nc"))
start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
if(is.na(config$run_config$forecast_start_datetime)){
  end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
  forecast_start_datetime <- end_datetime
}else{
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_datetime <- forecast_start_datetime + lubridate::days(config$forecast_horizon)
}
forecast_hour <- lubridate::hour(forecast_start_datetime)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$location$site_id,
                                lubridate::as_date(forecast_start_datetime), "00")

##'
# Convert NOAA forecasts to GLM format

met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                          out_dir = config$file_path$execute_directory,
                                          forecast_dir = noaa_forecast_path,
                                          config = config)
historical_met_error <- met_out$historical_met_error


##'
# Create observation matrix

cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,paste0("observations_postQAQC_long_",forecast_site,".csv"))
obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$obs_config_file), col_types = readr::cols())
obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                 obs_config,
                                 config)
states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$states_config_file), col_types = readr::cols())
states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
model_sd <- FLAREr::initiate_model_error(config = config, states_config = states_config)

##'
# Generate initial conditions

pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            restart_file = config$run_config$restart_file,
                                            historical_met_error = met_out$historical_met_error)


##'
# Run the "da" forecasts

print("Starting Data Assimilation and Forecasting")
print("-----------------------------------")
da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                              pars_init = init$pars,
                                              aux_states_init = init$aux_states_init,
                                              obs = obs,
                                              obs_sd = obs_config$obs_sd,
                                              model_sd = model_sd,
                                              working_directory = config$file_path$execute_directory,
                                              met_file_names = met_out$filenames[2:31],
                                              config = config,
                                              pars_config = pars_config,
                                              states_config = states_config,
                                              obs_config = obs_config,
                                              da_method = config$da_setup$da_method,
                                              par_fit_method = config$da_setup$par_fit_method)




print("Writing output file")
saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = config$file_path$forecast_output_directory)

print("Creating metadata")
FLAREr::create_flare_metadata(file_name = saved_file,
                              da_forecast_output = da_forecast_output)

rm(da_forecast_output)
gc()

##'
# Generate the plots from the forecast output

print("Generating plot")
print("-----------------------------------")
plotting_general_2(file_name = saved_file,
                   qaqc_data_directory = config$file_path$qaqc_data_directory, ncore = 5)

print(paste0("Metadata and plots generated! Go to NEON-forecast/forecast_output/",forecast_site," folder to view"))
