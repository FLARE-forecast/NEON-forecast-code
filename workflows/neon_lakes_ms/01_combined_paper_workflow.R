#remotes::install_github("FLARE-forecast/GLM3r", ref = "FLARErv1")
#remotes::install_github("FLARE-forecast/FLAREr", ref = "v2.2.0")

##'
# Load in the required functions for processing the data
#renv::restore()
library(tidyverse)
library(lubridate)
set.seed(100)
config_set_name <- "neon_lakes_ms"
run_glm_flare <- TRUE
run_clim_null <- TRUE
run_persistence_null <- FALSE
#Set use_archive = FALSE unless you have read/write credentials for the remote
#s3 bucket that is set up for running FLARE.
use_archive <- FALSE 
lake_directory <- here::here()

if(use_archive){
  
  dir.create(file.path(lake_directory, "drivers"), showWarnings = FALSE)
  
  download.file(url = 'https://zenodo.org/record/5918357/files/noaa.zip',
                destfile = file.path(lake_directory, 'drivers', 'noaa.zip'),
                method = 'curl')
  zip::unzip(file.path(lake_directory, 'drivers', 'noaa.zip'),
             exdir = file.path(lake_directory, 'drivers', 'noaa'))
  
  unlink(file.path(lake_directory, 'drivers', 'noaa.zip'))
  
  dir.create(file.path(lake_directory, "data_raw"), showWarnings = FALSE)
  
  download.file(url = 'https://zenodo.org/record/5918679/files/neonstore.zip',
                destfile = file.path(lake_directory, 'data_raw', 'neonstore.zip'),
                method = "curl")
  zip::unzip(file.path(lake_directory, 'data_raw', 'neonstore.zip'),
             exdir = file.path(lake_directory, 'data_raw','neonstore'))
  
  unlink(file.path(lake_directory, 'data_raw', 'neonstore.zip'))
  
  use_s3 <- FALSE
}else{
  Sys.setenv('AWS_DEFAULT_REGION' = 's3', 
             'AWS_S3_ENDPOINT' = 'flare-forecast.org', 
             'USE_HTTPS' = TRUE)
  use_s3 <- TRUE
}


source(file.path(lake_directory, "R/buoy_qaqc.R"))

sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG")

edi_url <- c("https://pasta.lternet.edu/package/data/eml/edi/1071/1/7f8aef451231d5388c98eef889332a4b",
             "https://pasta.lternet.edu/package/data/eml/edi/1071/1/2c8893684d94b9a52394060a76cab798", 
             "https://pasta.lternet.edu/package/data/eml/edi/1071/1/770e2ab9d957991a787a2f990d5a2fad",
             "https://pasta.lternet.edu/package/data/eml/edi/1071/1/2e52d63ba4dc2040d1e5e2d11114aa93",
             "https://pasta.lternet.edu/package/data/eml/edi/1071/1/60df35a34bb948c0ca5e5556d129aa98", 
             "https://pasta.lternet.edu/package/data/eml/edi/1071/1/004857d60d6fe7587b112d714e0380d0")

site_edi_profile <- c("NEON.D03.BARC.DP0.20005.001.01378.csv",
                      "NEON.D05.CRAM.DP0.20005.001.01378.csv",
                      "NEON.D05.LIRO.DP0.20005.001.01378.csv",
                      "NEON.D09.PRLA.DP0.20005.001.01378.csv",
                      "NEON.D09.PRPO.DP0.20005.001.01378.csv",
                      "NEON.D03.SUGG.DP0.20005.001.01378.csv")

start_from_scratch <- TRUE
time_start_index <- 1

sim_names <- "ms2_glm_flare"
config_files <- paste0("configure_flare_",sites,".yml")

#num_forecasts <- 20
num_forecasts <- 23 * 7 - 1
days_between_forecasts <- 1
forecast_horizon <- 35 #32
starting_date <- as_date("2021-04-18")
second_date <- starting_date + months(1) - days(days_between_forecasts)

start_dates <- rep(NA, num_forecasts)
start_dates[1:2] <- c(starting_date, second_date)
for(i in 3:num_forecasts){
  start_dates[i] <- as_date(start_dates[i-1]) + days(days_between_forecasts)
}

start_dates <- as_date(start_dates)
forecast_start_dates <- start_dates + days(days_between_forecasts)
forecast_start_dates <- as_date(c(NA, forecast_start_dates[-1]))

configure_run_file <- "configure_run.yml"

for(j in 1:length(sites)){
  
  message(paste0("Running site: ", sites[j]))
  
  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  run_config$configure_flare <- config_files[j]
  run_config$sim_name <- sim_names
  run_config$use_s3 <- use_s3
  yaml::write_yaml(run_config, file = file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  
  if(start_from_scratch){
    if(use_s3){
      FLAREr::delete_restart(sites[j], sim_names)
    }
    if(file.exists(file.path(lake_directory, "restart", sites[j], sim_names, configure_run_file))){
      unlink(file.path(lake_directory, "restart", sites[j], sim_names, configure_run_file))
    }
    config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
    config$run_config$start_datetime <- as.character(paste0(start_dates[1], " 00:00:00"))
    config$run_config$forecast_start_datetime <- as.character(paste0(start_dates[2], " 00:00:00"))
    config$run_config$forecast_horizon <- 0
    config$run_config$restart_file <- NA
    run_config <- config$run_config
    yaml::write_yaml(run_config, file = file.path(config$file_path$configuration_directory, configure_run_file))
  }else{
    config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
  }
  
  depth_bins <- config$model_settings$modeled_depths
  
  if(!use_archive){
    message("    Downloading NEON data")
    neonstore::neon_download(product = "DP1.20264.001", site = sites[j], start_date = NA)
  }
  
  neonstore_dir <-  file.path(lake_directory, "data_raw","neonstore")
  
  dir.create(neonstore_dir,showWarnings = FALSE)
  Sys.setenv("NEONSTORE_DB" = neonstore_dir)
  Sys.setenv("NEONSTORE_HOME" = neonstore_dir)
  
  ##'
  # Process the NEON data for the site selected in the original .yml file
  
  message("    Processing NEON data")
  
  edi_file <- site_edi_profile[str_detect(site_edi_profile, sites[j])]
  
  FLAREr::get_edi_file(edi_https = edi_url[str_detect(site_edi_profile, sites[j])],
                       file = edi_file,
                       lake_directory)
  
  profiler_data <- readr::read_csv(file.path(lake_directory,"data_raw",edi_file))
  
  cleaned_insitu_file <- buoy_qaqc(forecast_site = config$location$site_id,
                                   processed_filename = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                                   depth_bins,
                                   profiler_data = profiler_data,
                                   release = NA)
  
  FLAREr::put_targets(sites[j],
                      cleaned_insitu_file = cleaned_insitu_file, use_s3 = use_s3)
  
  ##` Download NOAA forecasts`
  
  message("    Downloading NOAA data")
  
  cycle <- "00"
  
  if(!use_archive){
    FLAREr::get_stacked_noaa(lake_directory, config, averaged = TRUE)
  }
  
  for(i in time_start_index:length(forecast_start_dates)){
    
    config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)
    
    config <- FLAREr::get_restart_file(config, lake_directory)
    
    message(paste0("     Running forecast that starts on: ", config$run_config$start_datetime, " with index: ",i))
    
    if(config$run_config$forecast_horizon > 0){
      noaa_forecast_path <- FLAREr::get_driver_forecast_path(config,
                                                             forecast_model = config$met$forecast_met_model)
      if(!use_archive){
        FLAREr::get_driver_forecast(lake_directory, forecast_path = noaa_forecast_path)
      }
      forecast_dir <- file.path(config$file_path$noaa_directory, noaa_forecast_path)
    }else{
      forecast_dir <- NULL
    }
    
    met_out <- FLAREr::generate_glm_met_files(obs_met_file = file.path(config$file_path$noaa_directory, "noaa", "NOAAGEFS_1hr_stacked_average", config$location$site_id, paste0("observed-met-noaa_",config$location$site_id,".nc")),
                                              out_dir = config$file_path$execute_directory,
                                              forecast_dir = forecast_dir,
                                              config = config)
    
    
    #Need to remove the 00 ensemble member because it only goes 16-days in the future
    met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "ens00")]
    
    ##' Create observation matrix
    cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv"))
    obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
    obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                     obs_config,
                                     config)
    
    states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())
    states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
    
    model_sd <- FLAREr::initiate_model_error(config = config, states_config = states_config)
    
    
    ##' Generate initial conditions
    pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
    init <- FLAREr::generate_initial_conditions(states_config,
                                                obs_config,
                                                pars_config,
                                                obs,
                                                config,
                                                restart_file = config$run_config$restart_file,
                                                historical_met_error = met_out$historical_met_error)
    
    ##' Run the forecasts
    message("    Starting Data Assimilation and Forecasting")
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
                                                  par_fit_method = config$da_setup$par_fit_method,
                                                  debug = TRUE)
    
    saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                                forecast_output_directory = config$file_path$forecast_output_directory,
                                                use_short_filename = TRUE)
    
    #Create EML Metadata
    eml_file_name <- FLAREr::create_flare_metadata(file_name = saved_file,
                                                   da_forecast_output = da_forecast_output)
    
    rm(da_forecast_output)
    gc()
    message("Generating plot")
    pdf_file <- FLAREr::plotting_general_2(file_name = saved_file,
                                           target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
                                           ncore = 2,
                                           obs_csv = FALSE)
    
    FLAREr::put_forecast(saved_file, eml_file_name, config)
    
    if(config$run_config$use_s3){
      success <- aws.s3::put_object(file = pdf_file, object = file.path(config$location$site_id, basename(pdf_file)), bucket = "analysis")
      if(success){
        unlink(pdf_file)
      }
    }
    
    FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file, new_horizon = forecast_horizon, day_advance = days_between_forecasts)
    
    unlink(config$run_config$restart_file)
    unlink(forecast_dir, recursive = TRUE)
    setwd(lake_directory)
    unlink(file.path(lake_directory, "flare_tempdir", config$location$site_id, run_config$sim_name), recursive = TRUE)
    
    if(i > 1){
      
      forecast_dates <- seq(as_date(config$run_config$forecast_start_datetime) + days(1),
                            as_date(config$run_config$forecast_start_datetime) + days(forecast_horizon), "1 day")
      forecast_doy <- yday(forecast_dates)
      
      curr_month <- month(config$run_config$forecast_start_datetime)
      if(curr_month < 10){
        curr_month <- paste0("0", curr_month)
      }
      curr_year <- year(config$run_config$forecast_start_datetime)
      start_date <- as_date(paste(curr_year,curr_month, "01", sep = "-"))
      
      target <- read_csv(cleaned_observations_file_long, show_col_types = FALSE) %>%
        filter(hour == 0) %>%
        group_by(date, hour, depth, variable) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        select(date, hour, depth, value, variable)
      
      message("   creating climatology forecast")
      if(run_clim_null){
        
        target_clim <- target %>%
          filter(date < as_date(config$run_config$forecast_start_datetime),
                 variable == "temperature") %>%
          mutate(doy = yday(date)) %>%
          group_by(doy, depth) %>%
          summarise(temp_clim = mean(value, na.rm = TRUE),
                    temp_sd = sd(value, na.rm = TRUE), .groups = "drop") %>%
          mutate(temp_sd = mean(temp_sd, na.rm = TRUE)) %>%
          mutate(temp_clim = ifelse((is.nan(temp_clim)), NA, temp_clim)) %>%
          na.omit()
        
        clim_forecast <- target_clim %>%
          mutate(doy = as.integer(doy)) %>%
          filter(doy %in% forecast_doy) %>%
          mutate(time = as_date((doy-1), origin = paste("2021", "01", "01", sep = "-"))) %>%
          select(time, depth , temp_clim, temp_sd) %>%
          rename(mean = temp_clim,
                 sd = temp_sd) %>%
          pivot_longer(c("mean", "sd"),names_to = "statistic", values_to = "temperature") %>%
          mutate(forecast = ifelse(time >= as_date(config$run_config$forecast_start_datetime), 1, 0)) %>%
          select(time, depth, statistic, forecast, temperature) %>%
          arrange(depth, time, statistic)
        
        forecast_file <- paste(sites[j], as_date(config$run_config$forecast_start_datetime), "ms2_doymean.csv.gz", sep = "-")
        
        saved_file <- file.path(config$file_path$forecast_output_directory, forecast_file)
        write_csv(clim_forecast, file = saved_file)
        
        if (config$run_config$use_s3) {
          success <- aws.s3::put_object(file = saved_file, object = file.path(config$location$site_id,
                                                                              basename(saved_file)), bucket = "forecasts")
          if (success) {
            unlink(saved_file)
          }
        }
      }
      
      if(run_persistence_null){
        message("   creating persistence forecast")
        
        most_recent_obs <- target %>%
          filter(date < as_date(config$run_config$forecast_start_datetime),
                 variable == "temperature") %>%
          na.omit() %>%
          arrange(rev(date))
        
        ens_size <- config$da_setup$ensemble_size
        walk_sd <- 0.25
        dates <- unique(clim_forecast$time)
        ndates <- length(dates) + 1
        depths <- unique(target$depth)
        random_walk <- array(NA, dim = c(ens_size, ndates, length(depths)))
        persist_forecast <- NULL
        
        for(k in 1:length(depths)){
          
          most_recent_obs_depth <- most_recent_obs %>%
            filter(depth == depths[k])
          
          random_walk[ ,1,k] <- unlist(most_recent_obs_depth$value[1])
          for(m in 2:ndates){
            random_walk[ ,m, k] <- rnorm(ens_size, mean = random_walk[ ,m - 1,k], walk_sd)
          }
          
          depth_tibble <-  suppressMessages(as_tibble(t(random_walk[,2:ndates ,k]), .name_repair = "unique"))
          
          names(depth_tibble) <- as.character(seq(1,ens_size, 1))
          
          depth_tibble <- bind_cols(time = dates,depth_tibble) %>%
            pivot_longer(cols = -time, names_to = "ensemble", values_to = "temperature") %>%
            mutate(forecast = 1,
                   depth = depths[k]) %>%
            select(time, depth, ensemble, forecast, temperature)
          
          persist_forecast <- bind_rows(persist_forecast, depth_tibble)
        }
        
        forecast_file <- paste(sites[j], as_date(config$run_config$forecast_start_datetime), "ms_persistence.csv.gz", sep = "-")
        saved_file <- file.path(config$file_path$forecast_output_directory, forecast_file)
        write_csv(persist_forecast, file = file.path(config$file_path$forecast_output_directory, forecast_file))
        
        if (config$run_config$use_s3) {
          success <- aws.s3::put_object(file = saved_file, object = file.path(config$location$site_id,
                                                                              basename(saved_file)), bucket = "forecasts")
          if (success) {
            unlink(saved_file)
          }
        }
      }
    }
  }
}