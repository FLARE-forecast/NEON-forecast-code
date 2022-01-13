#remotes::install_github("FLARE-forecast/GLMr")
#remotes::install_github("FLARE-forecast/FLAREr")

##'
# Load in the required functions for processing the data
#renv::restore()
library(tidyverse)
library(lubridate)
set.seed(100)

Sys.setenv("AWS_DEFAULT_REGION" = "s3",
           "AWS_S3_ENDPOINT" = "flare-forecast.org")

lake_directory <- here::here()

sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG")
#sites <- c("LIRO","PRLA")

#sites <- "BARC"


start_from_scratch <- TRUE
time_start_index <- 1


sim_names <- "ms_glm_flare"

#num_forecasts <- 20
num_forecasts <- 19 * 7 + 1
days_between_forecasts <- 1
forecast_horizon <- 34 #32
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

for(j in 1:length(sites)){

  #function(i, sites, lake_directory, sim_names, config_files, )

  message(paste0("Running site: ", sites[j]))


  cleaned_observations_file_long <- file.path(lake_directory,"targets",sites[j], paste0(sites[j], "-targets-insitu.csv"))

  for(i in time_start_index:length(forecast_start_dates)){

    if(i > 1){



      message("   creating climatology forecast")

      forecast_dates <- seq(forecast_start_dates[i] + days(1),
                            forecast_start_dates[i] + days(34), "1 day")
      forecast_doy <- yday(forecast_dates)

      #curr_month <- month(Sys.Date())
      curr_month <- month(forecast_dates[i])
      if(curr_month < 10){
        curr_month <- paste0("0", curr_month)
      }
      #curr_year <- year(Sys.Date())
      curr_year <- year(forecast_dates[i])
      start_date <- as_date(paste(curr_year,curr_month, "01", sep = "-"))

      target <- read_csv(cleaned_observations_file_long, show_col_types = FALSE) %>%
        filter(hour == 0) %>%
        group_by(date, hour, depth, variable) %>%
        summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        select(date, hour, depth, value, variable)

      target_clim <- target %>%
        filter(date < as_date(forecast_start_dates[i]),
               variable == "temperature") %>%
        mutate(doy = yday(date)) %>%
        group_by(doy, depth) %>%
        summarise(temp_clim = mean(value, na.rm = TRUE),
                  temp_count = n(),
                  temp_sd = sd(value, na.rm = TRUE), .groups = "drop") %>%
        mutate(temp_sd = mean(temp_sd, na.rm = TRUE)) %>%
        mutate(temp_clim = ifelse((is.nan(temp_clim)), NA, temp_clim)) %>%
        na.omit()

      clim_forecast <- target_clim %>%
        mutate(doy = as.integer(doy)) %>%
        filter(doy %in% forecast_doy) %>%
        mutate(time = as_date((doy-1), origin = paste(year(Sys.Date()), "01", "01", sep = "-"))) %>%
        select(time, depth , temp_clim, temp_sd, temp_count) %>%
        rename(mean = temp_clim,
               sd = temp_sd,
               count = temp_count) %>%
        pivot_longer(c("mean", "sd", "count"),names_to = "statistic", values_to = "temperature") %>%
        mutate(forecast = ifelse(time >= as_date(forecast_start_dates[i]), 1, 0)) %>%
        select(time, depth, statistic, forecast, temperature) %>%
        arrange(depth, time, statistic)

      forecast_file <- paste(sites[j], as_date(config$run_config$forecast_start_datetime), "ms_climatology.csv.gz", sep = "-")

      saved_file <- file.path(config$file_path$forecast_output_directory, forecast_file)
      write_csv(clim_forecast, file = saved_file)

      if (config$run_config$use_s3) {
        success <- aws.s3::put_object(file = saved_file, object = file.path(config$location$site_id,
                                                                            basename(saved_file)), bucket = "forecasts")
        if (success) {
          unlink(saved_file)
        }
      }

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



