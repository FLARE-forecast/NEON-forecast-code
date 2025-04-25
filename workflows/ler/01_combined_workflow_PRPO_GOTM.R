Sys.setenv("AWS_DEFAULT_REGION" = "amnh1",
           "AWS_S3_ENDPOINT" = "osn.mghpcc.org",
           "USE_HTTPS" = TRUE)

# get the arguments from the workflow file
DA_use <- commandArgs(trailingOnly = T)

if (length(DA_use) != 1)  {
  DA_use <- T
} 

library(tidyverse)
library(lubridate)

options(future.globals.maxSize= 891289600)
FLAREr::ignore_sigpipe()

lake_directory <- here::here()
setwd(lake_directory)

forecast_site <- "PRPO"
model <- "GOTM"

message(paste0("Running site: ", forecast_site))
config_set_name <- file.path("ler", forecast_site)

# switch to turn DA on or off
if (DA_use == F) {
  configure_run_file <- paste0("configure_run_",forecast_site,'_',model,"noDA.yml")
  message('using run_config with no data assimilation')
} else {
  configure_run_file <- paste0("configure_run_",forecast_site,'_',model,".yml")
  
}

config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)


cuts <- tibble::tibble(cuts = as.integer(factor(config$model_settings$modeled_depths)),
                       depth = config$model_settings$modeled_depths)

cleaned_insitu_file <- file.path(lake_directory, "targets", config$location$site_id, config$da_setup$obs_filename)
readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-expanded-observations.csv.gz", show_col_types = FALSE) |> 
  filter(site_id == forecast_site) |> 
  dplyr::mutate(cuts = cut(depth, breaks = config$model_settings$modeled_depths, include.lowest = TRUE, right = FALSE, labels = FALSE)) |>
  dplyr::filter(lubridate::hour(datetime) == 0) |>
  dplyr::group_by(cuts, variable, datetime, site_id) |>
  dplyr::summarize(observation = mean(observation, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(cuts, by = "cuts") |>
  dplyr::select(site_id, datetime, variable, depth, observation) |>
  write_csv(cleaned_insitu_file)

#' Move targets to s3 bucket

message("Successfully generated targets")

FLAREr::put_targets(site_id =  config$location$site_id,
                    cleaned_insitu_file = cleaned_insitu_file,
                    cleaned_met_file = NA,
                    cleaned_inflow_file = NA,
                    use_s3 = config$run_config$use_s3,
                    config = config)

if(config$run_config$use_s3){
  message("Successfully moved targets to s3 bucket")
}

#=============================================#

noaa_ready <- TRUE
start <- Sys.time()
run_duration <- 0

max_runtime <- 5*60*60

while(run_duration < max_runtime & noaa_ready == T){
  # Run FLARE
  config <- FLAREr::set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr::get_restart_file(config, lake_directory)
  
  message(paste0("     Running forecast that starts on: ", config$run_config$start_datetime))
  
  # switch to turn DA on or off check
  if (DA_use == F) {
    config$da_setup$use_obs_constraint <- FALSE
  } else {
    config$da_setup$use_obs_constraint <- TRUE
  }
  
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())
  
  if(model != "GLM"){
    use_ler_vars = TRUE
  }else{
    use_ler_vars = FALSE
  }
  
  
  if(!"temp" %in% states_config$state_names) stop("missing temp as a state name in states config")
  if(!"salt" %in% states_config$state_names) stop("missing salt as a state name in states config")
  
  
  if(!config$met$use_observed_met){
    obs_met_file = NULL
  }else{
    obs_met_file <- file.path(config$file_path$qaqc_data_directory, config$met$observed_met_filename)
    if(!fs::file_exists(obs_met_file)){
      stop(paste0(obs_met_file, " is not found"))
    }
  }
  
  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  
  if(config$run_config$forecast_horizon > 16 & config$met$use_forecasted_met){
    met_forecast_start_datetime <- met_forecast_start_datetime - lubridate::days(config$met$forecast_lag_days)
    if(met_forecast_start_datetime < met_start_datetime){
      met_start_datetime <- met_forecast_start_datetime
      message("horizon is > 16 days so adjusting forecast_start_datetime in the met file generation to use yesterdays forecast. But adjusted forecast_start_datetime < start_datetime")
    }
  }
  
  
  
  # generate met files
  met_out <- FLAREr::generate_met_files_arrow(obs_met_file = NULL,
                                              out_dir = config$file_path$execute_directory,
                                              start_datetime = met_start_datetime,
                                              end_datetime = config$run_config$end_datetime,
                                              forecast_start_datetime = met_forecast_start_datetime,
                                              forecast_horizon =  config$run_config$forecast_horizon,
                                              site_id = config$location$site_id,
                                              use_s3 = TRUE,
                                              bucket = config$s3$drivers$bucket,
                                              endpoint = config$s3$drivers$endpoint,
                                              local_directory = NULL,
                                              use_forecast = TRUE,
                                              use_ler_vars = use_ler_vars)
  
  
  met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "31")]
  
  # No inflows or outflows
  inflow_file_names <- NULL
  outflow_file_names <- NULL
  
  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                   obs_config = obs_config,
                                   config)
  
  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
  
  model_sd <- FLAREr::initiate_model_error(config, states_config)
  
  
  
  # Keep in the if statements
  if(model != "GLM"){
    init <- FLARErLER::generate_initial_conditions_ler(states_config,
                                                       obs_config,
                                                       pars_config,
                                                       obs,
                                                       config,
                                                       historical_met_error = met_out$historical_met_error)
  }else{
    init <- FLAREr::generate_initial_conditions(states_config,
                                                obs_config,
                                                pars_config,
                                                obs,
                                                config,
                                                historical_met_error = met_out$historical_met_error)
  }
  
  if(model != "GLM"){ #GOTM and Simstrat have different diagnostics
    config$output_settings$diagnostics_names <- NULL
  }
  
  
  #Run EnKF
  if(model != "GLM"){
    da_forecast_output <- FLARErLER::run_da_forecast_ler(states_init = init$states,
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
                                                         management = NULL,
                                                         da_method = config$da_setup$da_method,
                                                         par_fit_method = config$da_setup$par_fit_method,
                                                         debug = FALSE)
  }else{
    config$model_settings$model_name <- "GLM"
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
                                                  management = NULL,
                                                  da_method = config$da_setup$da_method,
                                                  par_fit_method = config$da_setup$par_fit_method,
                                                  debug = FALSE)
  }
  
  # Save forecast
  if(model != "GLM"){
    message("writing netcdf")
    saved_file <- FLARErLER::write_forecast_netcdf_ler(da_forecast_output = da_forecast_output,
                                                       forecast_output_directory = config$file_path$forecast_output_directory,
                                                       use_short_filename = TRUE)
    message("writing arrow forecast")
    forecast_df <- FLARErLER::write_forecast_arrow_ler(da_forecast_output = da_forecast_output,
                                                       use_s3 = config$run_config$use_s3,
                                                       bucket = config$s3$forecasts_parquet$bucket,
                                                       endpoint = config$s3$forecasts_parquet$endpoint,
                                                       local_directory = file.path(lake_directory, config$s3$forecasts_parquet$bucket))
    
  }else{
    message("writing netcdf")
    saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                                forecast_output_directory = config$file_path$forecast_output_directory,
                                                use_short_filename = TRUE)
    message("writing arrow forecast")
    forecast_df <- FLAREr::write_forecast_arrow(da_forecast_output = da_forecast_output,
                                                use_s3 = config$run_config$use_s3,
                                                bucket = config$s3$forecasts_parquet$bucket,
                                                endpoint = config$s3$forecasts_parquet$endpoint,
                                                local_directory = file.path(lake_directory, config$s3$forecasts_parquet$bucket))
    
  }
  
  message("Scoring forecast")
  
  if(config$output_settings$evaluate_past){
    reference_datetime_format <- "%Y-%m-%d %H:%M:%S"
    past_days <- strftime(lubridate::as_datetime(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon), tz = "UTC")
    
    vars <- FLAREr:::arrow_env_vars()
    s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint)
    past_forecasts <- arrow::open_dataset(s3) |>
      dplyr::filter(model_id == forecast_df$model_id[1],
                    site_id == forecast_df$site_id[1],
                    reference_datetime > past_days) |>
      dplyr::collect()
    FLAREr:::unset_arrow_vars(vars)
  }else{
    past_forecasts <- NULL
  }
  
  FLAREr::generate_forecast_score_arrow(targets_file = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                        forecast_df = forecast_df,
                                        use_s3 = config$run_config$use_s3,
                                        bucket = config$s3$scores$bucket,
                                        endpoint = config$s3$scores$endpoint,
                                        local_directory = file.path(lake_directory, config$s3$scores$bucket),
                                        variable_types = c("state","parameter"))
  
  message("Generating plot")
  FLAREr::plotting_general_2(file_name = saved_file,
                             target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                             ncore = 2,
                             obs_csv = FALSE)
  
  FLAREr::put_forecast(saved_file, eml_file_name = NULL, config)
  #=======================================#
  
  # set up restart files
  
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(1)
  start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime) - lubridate::days(5)
  restart_file <- paste0(config$location$site_id,"-", (lubridate::as_date(forecast_start_datetime)- days(1)), "-",config$run_config$sim_name ,".nc")
  
  FLAREr::update_run_config2(lake_directory = lake_directory,
                             configure_run_file = configure_run_file, 
                             restart_file = restart_file, 
                             start_datetime = start_datetime, 
                             end_datetime = NA, 
                             forecast_start_datetime = forecast_start_datetime,  
                             forecast_horizon = config$run_config$forecast_horizon,
                             sim_name = config$run_config$sim_name, 
                             site_id = config$location$site_id,
                             configure_flare = config$run_config$configure_flare, 
                             configure_obs = config$run_config$configure_obs, 
                             use_s3 = config$run_config$use_s3,
                             bucket = config$s3$warm_start$bucket,
                             endpoint = config$s3$warm_start$endpoint,
                             use_https = TRUE)
  
  # check that it should run again...
  noaa_ready <- FLAREr::check_noaa_present_arrow(lake_directory = lake_directory,
                                                 configure_run_file = configure_run_file,
                                                 config_set_name = config_set_name)
  finish_time <- Sys.time()
  run_duration <- round(difftime(finish_time, start, units = 'secs'))
  message(paste('current runtime:', round(run_duration), '/', max_runtime))
}
