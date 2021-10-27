combine_forecast_obs <- function (file_name, qaqc_data_directory, extra_historical_days,
                                  ncore = 5)
{
  nc <- ncdf4::nc_open(file_name)
  t <- ncdf4::ncvar_get(nc, "time")
  local_tzone <- ncdf4::ncatt_get(nc, 0)$local_time_zone_of_simulation
  full_time <- as.POSIXct(t, origin = "1970-01-01 00:00.00 UTC",
                          tz = "UTC")
  full_time_day <- lubridate::as_date(full_time)
  nsteps <- length(full_time_day)
  forecast <- ncdf4::ncvar_get(nc, "forecast")
  depths <- round(ncdf4::ncvar_get(nc, "depth"), 2)
  var_names <- names(nc$var)
  combined_states <- list()
  combined_states_conversion <- list()
  obs_methods <- list()
  output_type <- rep(NA, length(var_names))
  target_variable <- rep(NA, length(var_names))
  time_threshold <- rep(NA, length(var_names))
  distance_threshold <- rep(NA, length(var_names))
  for (i in 1:length(var_names)) {
    tmp <- ncdf4::ncatt_get(nc, varid = var_names[i], attname = "long_name")$value
    output_type[i] <- stringr::str_split(tmp, ":")[[1]][1]
    combined_states[i] <- c(stringr::str_split(stringr::str_split(tmp,
                                                                  ":")[[1]][3], "-")[1])
    combined_states_conversion[i] <- list(as.numeric(unlist(stringr::str_split(stringr::str_split(tmp,
                                                                                                  ":")[[1]][4], "-")[1])))
    target_variable[i] <- list((unlist(stringr::str_split(stringr::str_split(tmp,
                                                                             ":")[[1]][5], "-")[1])))
    distance_threshold[i] <- list(as.numeric(unlist(stringr::str_split(stringr::str_split(tmp,
                                                                                          ":")[[1]][6], "-")[1])))
  }
  wq_names <- var_names[output_type == "state"]
  combined_states_conversion_index <- which(stringr::str_detect(var_names,
                                                                "total") | stringr::str_detect(var_names, "PHY_TCHLA_observed"))
  if (length(combined_states_conversion_index) > 0) {
    combined_states <- combined_states[combined_states_conversion_index]
    combined_states_conversion <- combined_states_conversion[combined_states_conversion_index]
    combined_states_names <- stringr::str_split(var_names[combined_states_conversion_index],
                                                "_")
    for (i in 1:length(combined_states)) {
      tmp <- combined_states_names[[i]][which(combined_states_names[[i]] !=
                                                "observed")]
      combined_states_names[i] <- tmp[1]
      if (length(tmp) > 1) {
        for (j in 2:length(tmp)) {
          combined_states_names[i] <- paste0(combined_states_names[i],
                                             "_", tmp[j])
        }
      }
    }
    names(combined_states) <- combined_states_names
    names(combined_states_conversion) <- combined_states_names
    state_names <- c(wq_names, names(combined_states))
  }
  else {
    combined_states <- NULL
    combined_states_conversion <- NULL
    state_names <- wq_names
  }
  diagnostics_names <- var_names[output_type == "diagnostic"]
  obs_names <- stringr::str_split(var_names[output_type ==
                                              "observed"], "_")
  for (i in 1:length(obs_names)) {
    tmp <- obs_names[[i]][which(obs_names[[i]] != "observed")]
    obs_names[i] <- tmp[1]
    if (length(tmp) > 1) {
      for (j in 2:length(tmp)) {
        obs_names[i] <- paste0(obs_names[i], "_", tmp[j])
      }
    }
  }
  obs_methods <- obs_methods[output_type == "observed"]
  target_variable <- target_variable[output_type == "observed"]
  time_threshold <- time_threshold[output_type == "observed"]
  distance_threshold <- distance_threshold[output_type ==
                                             "observed"]
  par_names <- var_names[output_type == "parameter"]
  if (length(which(forecast == 1)) > 0) {
    forecast_index <- which(forecast == 1)[1]
  }
  else {
    forecast_index <- 0
  }
  par_list <- list()
  if (length(par_names) > 0) {
    for (par in 1:length(par_names)) {
      par_list[[par]] <- ncdf4::ncvar_get(nc, par_names[par])
    }
  }
  state_list <- list()
  for (s in 1:length(wq_names)) {
    state_list[[s]] <- ncdf4::ncvar_get(nc, wq_names[s])
  }
  if (length(combined_states) > 0) {
    for (i in 1:length(combined_states)) {
      for (s in 1:length(combined_states[[i]])) {
        if (s > 1) {
          tmp_list <- tmp_list + ncdf4::ncvar_get(nc,
                                                  combined_states[[i]][s]) * combined_states_conversion[[i]][s]
        }
        else {
          tmp_list <- ncdf4::ncvar_get(nc, combined_states[[i]][s]) *
            combined_states_conversion[[i]][s]
        }
      }
      state_list[[length(wq_names) + i]] <- tmp_list
    }
  }
  names(state_list) <- state_names
  diagnostic_list <- list()
  for (s in 1:length(diagnostics_names)) {
    diagnostic_list[[s]] <- ncdf4::ncvar_get(nc, diagnostics_names[s])
  }
  names(diagnostic_list) <- diagnostics_names
  ncdf4::nc_close(nc)
  cleaned_observations_file_long <- paste0(qaqc_data_directory,
                                           "/observations_postQAQC_long_",forecast_site,".csv")
  d <- readr::read_csv(cleaned_observations_file_long, col_types = readr::cols())
  full_time_extended <- seq(full_time[1] - lubridate::days(extra_historical_days),
                            max(full_time), by = "1 day")
  switch(Sys.info()[["sysname"]], Linux = {
    machine <- "unix"
  }, Darwin = {
    machine <- "mac"
  }, Windows = {
    machine <- "windows"
  })
  if (machine == "windows") {
    cl <- parallel::makeCluster(ncore, setup_strategy = "sequential")
    parallel::clusterEvalQ(cl, library(magrittr))
  }
  else {
    cl <- parallel::makeCluster(ncore, setup_strategy = "sequential")
    parallel::clusterEvalQ(cl, library(magrittr))
  }
  on.exit({
    tryCatch({
      parallel::stopCluster(cl)
    }, error = function(e) {
      return(NA)
    })
  })
  parallel::clusterExport(cl, varlist = list("obs_names",
                                             "full_time", "target_variable", "depths", "full_time_extended",
                                             "d", "distance_threshold"), envir = environment())
  obs_list <- parallel::parLapply(cl, 1:length(obs_names),
                                  function(i) {
                                    message(paste0("Extracting ", target_variable[i]))
                                    obs_tmp <- array(NA, dim = c(length(full_time_extended),
                                                                 length(depths)))
                                    for (k in 1:length(full_time_extended)) {
                                      for (j in 1:length(depths)) {
                                        d1 <- d %>% dplyr::filter(variable == target_variable[i],
                                                                  date == lubridate::as_date(full_time_extended[k]),
                                                                  (is.na(hour) | hour == lubridate::hour(full_time_extended[k])),
                                                                  abs(depth - depths[j]) < distance_threshold[i])
                                        if (nrow(d1) == 1) {
                                          obs_tmp[k, j] <- d1$value
                                        }
                                        else if (nrow(d1) > 1) {
                                          obs_tmp[k, j] <- mean(d1$value, na.rm = TRUE)
                                        }
                                        else {
                                          obs_tmp[k, j] <- NA
                                        }
                                      }
                                    }
                                    return(obs_tmp)
                                  })
  obs <- array(NA, dim = c(length(full_time_extended), length(depths),
                           length(obs_names)))
  for (i in 1:length(obs_names)) {
    obs[, , i] <- obs_list[[i]]
  }
  return(list(obs = obs, full_time_extended = full_time_extended,
              diagnostic_list = diagnostic_list, state_list = state_list,
              forecast = forecast, par_list = par_list, obs_list = obs_list,
              state_names = state_names, par_names = par_names, diagnostics_names = diagnostics_names,
              full_time = full_time, obs_long = d, depths = depths,
              obs_names = obs_names))
}

plotting_general_3 <- function (file_name, qaqc_data_directory, ncore = 5, plot_profile = TRUE,
                                obs_csv = TRUE)
{
  pdf_file_name <- paste0(tools::file_path_sans_ext(file_name),
                          ".pdf")
  csv_file_name <- paste0(tools::file_path_sans_ext(file_name),
                          ".csv")
  output <- combine_forecast_obs(file_name,
                                 qaqc_data_directory = qaqc_data_directory, extra_historical_days = 0,
                                 ncore = ncore)
  obs <- output$obs
  full_time_extended <- output$full_time_extended
  diagnostic_list <- output$diagnostic_list
  state_list <- output$state_list
  forecast <- output$forecast
  par_list <- output$par_list
  obs_list <- output$obs_list
  state_names <- output$state_names
  par_names <- output$par_names
  diagnostics_names <- output$diagnostics_names
  full_time <- output$full_time
  obs_long <- output$obs_long
  depths <- output$depths
  obs_names <- output$obs_names
  if (length(which(forecast == 1)) > 0) {
    forecast_index <- which(forecast == 1)[1]
  }
  else {
    forecast_index <- 0
  }
  focal_depths_plotting <- depths
  if (length(focal_depths_plotting) < 4) {
    plot_height <- 3
  }
  else {
    plot_height <- 8
  }
  pdf(pdf_file_name, width = 11, height = plot_height)
  evaluation_df <- NULL
  for (i in 1:length(state_names)) {
    curr_var <- state_list[[i]]
    message(state_names[i])
    mean_var <- array(NA, dim = c(length(depths), length(full_time)))
    upper_var <- array(NA, dim = c(length(depths), length(full_time)))
    lower_var <- array(NA, dim = c(length(depths), length(full_time)))
    sd_var <- array(NA, dim = c(length(depths), length(full_time)))
    for (j in 1:length(full_time)) {
      for (ii in 1:length(depths)) {
        mean_var[ii, j] <- mean(curr_var[j, ii, ], na.rm = TRUE)
        sd_var[ii, j] <- sd(curr_var[j, ii, ], na.rm = TRUE)
        upper_var[ii, j] <- quantile(curr_var[j, ii,
        ], 0.1, na.rm = TRUE)
        lower_var[ii, j] <- quantile(curr_var[j, ii,
        ], 0.9, na.rm = TRUE)
      }
    }
    date <- c()
    for (j in 1:length(full_time)) {
      date <- c(date, rep(full_time[j], length(depths)))
    }
    if (state_names[i] %in% unlist(obs_names)) {
      obs_index <- which(obs_names == state_names[i])
      obs_curr <- as.numeric(c(t(obs[, , obs_index])))
    }
    else {
      obs_curr <- as.numeric(rep(NA, length(date)))
    }
    if (forecast_index > 0) {
      forecast_start_day <- full_time[forecast_index -
                                        1]
      forecast_start_day_alpha <- 1
    }
    else {
      forecast_start_day <- dplyr::last(full_time)
      forecast_start_day_alpha <- 0
    }
    curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
                                  forecast_mean = round(c(mean_var), 4), forecast_sd = round(c(sd_var),
                                                                                             4), forecast_upper_95 = round(c(upper_var),
                                                                                                                           4), forecast_lower_95 = round(c(lower_var),
                                                                                                                                                         4), observed = round(obs_curr, 4), depth = rep(depths,
                                                                                                                                                                                                        length(full_time)), state = state_names[i],
                                  forecast_start_day = forecast_start_day) %>% dplyr::filter(depth %in%
                                                                                               focal_depths_plotting)
    if (obs_csv) {
      only_with_obs <- curr_tibble %>% dplyr::filter(!is.na(observed))
      evaluation_df <- dplyr::bind_rows(evaluation_df,
                                        only_with_obs)
    }
    p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
      ggplot2::facet_wrap(~depth) + ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast_lower_95,
                                                                      ymax = forecast_upper_95), alpha = 0.7, fill = "gray") +
      ggplot2::geom_line(ggplot2::aes(y = forecast_mean),
                         size = 0.5) + ggplot2::geom_vline(xintercept = forecast_start_day,
                                                           alpha = forecast_start_day_alpha) + ggplot2::geom_point(ggplot2::aes(y = observed),
                                                                                                                   size = 0.5, color = "red") + ggplot2::theme_light() +
      ggplot2::labs(x = "Date", y = state_names[i], title = state_names[i]) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                         size = 10))
    print(p)
    if (plot_profile) {
      p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(y = depth,
                                                     x = forecast_mean)) + ggplot2::facet_wrap(~factor(date)) +
        ggplot2::geom_ribbon(ggplot2::aes(xmin = forecast_upper_95,
                                          xmax = forecast_lower_95), alpha = 0.7, fill = "gray") +
        ggplot2::geom_path() + ggplot2::geom_point(ggplot2::aes(x = observed),
                                                   size = 0.5, color = "red") + ggplot2::scale_y_reverse() +
        ggplot2::theme_light() + ggplot2::labs(y = "Depth(m)",
                                               x = state_names[i], title = state_names[i])
      print(p)
    }
  }
  if (obs_csv) {
    readr::write_csv(evaluation_df, csv_file_name)
  }
  if (length(par_names) > 0) {
    plist <- list()
    for (i in 1:length(par_names)) {
      message(par_names[i])
      curr_var <- par_list[[i]]
      mean_var <- array(NA, dim = c(length(full_time)))
      upper_var <- array(NA, dim = c(length(full_time)))
      lower_var <- array(NA, dim = c(length(full_time)))
      for (j in 1:length(full_time)) {
        mean_var[j] <- mean(curr_var[j, ])
        upper_var[j] <- quantile(curr_var[j, ], 0.1,
                                 na.rm = TRUE)
        lower_var[j] <- quantile(curr_var[j, ], 0.9,
                                 na.rm = TRUE)
      }
      date <- full_time
      if (forecast_index > 0) {
        forecast_start_day <- full_time[forecast_index -
                                          1]
        forecast_start_day_alpha <- 1
      }
      else {
        forecast_start_day <- dplyr::last(full_time)
        forecast_start_day_alpha <- 0
      }
      curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
                                    curr_var = c(mean_var), upper_var = c(upper_var),
                                    lower_var = c(lower_var))
      plist[[i]] <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var,
                                          ymax = upper_var), alpha = 0.7, fill = "gray") +
        ggplot2::geom_line(ggplot2::aes(y = curr_var),
                           size = 0.5) + ggplot2::geom_vline(xintercept = forecast_start_day,
                                                             alpha = forecast_start_day_alpha) + ggplot2::theme_bw() +
        ggplot2::labs(x = "Date", y = par_names[i]) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           size = 10))
    }
    print(patchwork::wrap_plots(plist))
  }
  if (length(diagnostics_names) > 0)
    for (i in 1:length(diagnostics_names)) {
      message(diagnostics_names[i])
      curr_var <- diagnostic_list[[i]]
      mean_var <- array(NA, dim = c(length(depths), length(full_time)))
      upper_var <- array(NA, dim = c(length(depths), length(full_time)))
      lower_var <- array(NA, dim = c(length(depths), length(full_time)))
      for (j in 1:length(full_time)) {
        for (ii in 1:length(depths)) {
          mean_var[ii, j] <- mean(curr_var[j, ii, ],
                                  na.rm = TRUE)
          upper_var[ii, j] <- quantile(curr_var[j, ii,
          ], 0.1, na.rm = TRUE)
          lower_var[ii, j] <- quantile(curr_var[j, ii,
          ], 0.9, na.rm = TRUE)
        }
      }
      date <- c()
      for (j in 1:length(full_time)) {
        date <- c(date, rep(full_time[j], length(depths)))
      }
      curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
                                    curr_var = c(mean_var), upper_var = c(upper_var),
                                    lower_var = c(lower_var), depth = rep(depths,
                                                                          length(full_time))) %>% dplyr::filter(depth %in%
                                                                                                                  focal_depths_plotting)
      if (forecast_index > 0) {
        forecast_start_day <- full_time[forecast_index -
                                          1]
        forecast_start_day_alpha <- 1
      }
      else {
        forecast_start_day <- dplyr::last(full_time)
        forecast_start_day_alpha <- 0
      }
      p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
        ggplot2::facet_wrap(~depth) + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var,
                                                                        ymax = upper_var), alpha = 0.7, fill = "gray") +
        ggplot2::geom_line(ggplot2::aes(y = curr_var),
                           size = 0.5) + ggplot2::geom_vline(xintercept = forecast_start_day,
                                                             alpha = forecast_start_day_alpha) + ggplot2::theme_light() +
        ggplot2::labs(x = "Date", y = diagnostics_names[i],
                      title = diagnostics_names[i]) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                                                         size = 10))
      print(p)
    }
  if ("extc_coef" %in% diagnostics_names) {
    message("secchi")
    obs_date <- tibble::tibble(date = lubridate::as_date(full_time)) %>%
      dplyr::mutate(date = as.character(date))
    obs_secchi <- obs_long %>% dplyr::filter(variable ==
                                               "secchi") %>% dplyr::mutate(date = as.character(date))
    if (nrow(obs_secchi) > 0) {
      obs_curr <- dplyr::left_join(obs_date, obs_secchi,
                                   by = "date")
      obs_curr <- obs_curr$value
    }
    else {
      obs_curr <- rep(NA, length(obs_date))
    }
    i <- which(diagnostics_names == "extc_coef")
    ii <- which.min(abs(depths - 1))
    curr_var <- diagnostic_list[[i]]
    mean_var <- array(NA, dim = c(length(full_time)))
    upper_var <- array(NA, dim = c(length(full_time)))
    lower_var <- array(NA, dim = c(length(full_time)))
    for (j in 1:length(full_time)) {
      sechi <- 1.7/curr_var[j, ii, ]
      mean_var[j] <- mean(sechi, na.rm = TRUE)
      upper_var[j] <- quantile(sechi, 0.1, na.rm = TRUE)
      lower_var[j] <- quantile(sechi, 0.9, na.rm = TRUE)
    }
    curr_tibble <- tibble::tibble(date = lubridate::as_datetime(full_time),
                                  curr_var = c(mean_var), upper_var = c(upper_var),
                                  lower_var = c(lower_var), observed = unlist(obs_curr))
    if (forecast_index > 0) {
      forecast_start_day <- full_time[forecast_index -
                                        1]
      forecast_start_day_alpha <- 1
    }
    else {
      forecast_start_day <- dplyr::last(full_time)
      forecast_start_day_alpha <- 0
    }
    p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var,
                                        ymax = upper_var), alpha = 0.7, fill = "gray") +
      ggplot2::geom_line(ggplot2::aes(y = curr_var), size = 0.5) +
      ggplot2::scale_y_reverse() + ggplot2::geom_vline(xintercept = forecast_start_day,
                                                       alpha = forecast_start_day_alpha) + ggplot2::geom_point(ggplot2::aes(y = observed),
                                                                                                               size = 1, color = "red") + ggplot2::theme_light() +
      ggplot2::labs(x = "Date", y = "Sechi depth (m)",
                    title = "Sechi depth") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                                                size = 10))
    print(p)
  }
  dev.off()
}
