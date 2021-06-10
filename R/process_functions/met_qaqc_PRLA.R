met_qaqc_prla <- function(realtime_file,
                          cleaned_met_file_dir,
                          input_file_tz,
                          local_tzone){

  d1 <- readr::read_csv(realtime_file)
  d1$time <- lubridate::force_tz(d1$time, tzone = "UTC")
  # Impute Prairie Lake
  f <- d1 %>% filter(siteID == "PRLA")
  f <- as.data.frame(f)

  #ShortWave
  amelia.sw <- amelia(f, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave", idvars = "siteID")
  sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
    select(time, ShortWave)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))

  #LongWave
  amelia.lw <- amelia(f, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave", idvars = "siteID")
  lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
    select(time, LongWave)%>%
    group_by(time)%>%
    summarise_all(funs(mean))

  #AirTemp
  amelia.at <- amelia(f, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp", idvars = "siteID")
  at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
    select(time, AirTemp)%>%
    group_by(time)%>%
    summarise_all(funs(mean))

  #Himidity
  amelia.rh <- amelia(f, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum", idvars = "siteID")
  rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
    select(time, RelHum)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))

  #Rain
  amelia.pr <- amelia(f, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain", idvars = "siteID")
  pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
    select(time, Rain)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(Rain = ifelse(Rain <= 0, 0, Rain))

  #WindSpeed
  amelia.ws <- amelia(f, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed", idvars = "siteID")
  ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
    select(time, WindSpeed)%>%
    group_by(time)%>%
    summarise_all(funs(mean))%>%
    mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))

  #Pressure
  amelia.p <- amelia(f, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure", idvars = "siteID")
  p_imputations <- bind_rows(unclass(amelia.p$imputations), .id = "m") %>%
    select(time, Pressure)%>%
    group_by(time)%>%
    summarise_all(funs(mean))

  imputed <- left_join(sw_imputations, lw_imputations, by = "time")%>%
    left_join(., at_imputations, by = "time")%>%
    left_join(., rh_imputations, by = "time")%>%
    left_join(., pr_imputations, by = "time")%>%
    left_join(., ws_imputations, by = "time")%>%
    left_join(., p_imputations, by = "time")

  met_new <- left_join(f, imputed, by = "time")

  f <- met_new %>%
    mutate(ShortWave.x = ifelse(is.na(ShortWave.x), ShortWave.y, ShortWave.x))%>%
    mutate(LongWave.x = ifelse(is.na(LongWave.x), LongWave.y, LongWave.x))%>%
    mutate(AirTemp.x = ifelse(is.na(AirTemp.x), AirTemp.y, AirTemp.x))%>%
    mutate(RelHum.x = ifelse(is.na(RelHum.x), RelHum.y, RelHum.x))%>%
    mutate(WindSpeed.x = ifelse(is.na(WindSpeed.x), WindSpeed.y, WindSpeed.x))%>%
    mutate(Rain.x = ifelse(is.na(Rain.x), Rain.y, Rain.x))%>%
    mutate(Pressure.x = ifelse(is.na(Pressure.x), Pressure.y, Pressure.x))%>%
    select(time, ShortWave.x, LongWave.x, AirTemp.x, RelHum.x, WindSpeed.x, Rain.x, Pressure.x)%>%
    rename(surface_downwelling_shortwave_flux_in_air = ShortWave.x,
           surface_downwelling_longwave_flux_in_air = LongWave.x,
           air_temperature = AirTemp.x,
           specific_humidity = RelHum.x,
           wind_speed = WindSpeed.x,
           precipitation_flux = Rain.x,
           air_pressure = Pressure.x)

  f$time <- lubridate::force_tz(f$time, tzone = "UTC") #input_file_tz

  wshgt <- 3
  roughlength <- 0.000114
  maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
  minTempC = -24 # an lower bound of realistic temperature for the study site in deg C

  f <- f %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                           relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                           relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                           relative_humidity = specific_humidity / 100,
                           air_temperature = air_temperature + 273.15,
                           surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                           wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                           wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
    filter(is.na(time) == FALSE)


  f$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = f$relative_humidity,
                                                  T = f$air_temperature,
                                                  press = f$air_pressure)

  f <- f %>%select(time,
                   air_temperature,
                   air_pressure,
                   relative_humidity,
                   surface_downwelling_longwave_flux_in_air,
                   surface_downwelling_shortwave_flux_in_air,
                   precipitation_flux, specific_humidity,
                   wind_speed)

  cf_var_names1 <- c("air_temperature", "air_pressure", "relative_humidity", "surface_downwelling_longwave_flux_in_air",
                     "surface_downwelling_shortwave_flux_in_air", "precipitation_flux","specific_humidity","wind_speed")

  cf_var_units1 <- c("K", "Pa", "1", "Wm-2", "Wm-2", "kgm-2s-1", "1", "ms-1")  #Negative numbers indicate negative exponents

  f <- f %>%
    tidyr::drop_na()


  a <- f %>% reshape2::melt(., id = "time") %>% ggplot(.) +
    geom_line(aes(time, value)) + labs(title = paste0(siteID," Post imputation"))+
    facet_wrap(~variable, scales = "free_y") +
    theme_classic()

  return(a)

  model_name <- "observed-met"
  site <- "PRLA"
  lat <- 47.15909
  lon <- -99.11388
  start_time <- dplyr::first((f$time))
  end_time <- dplyr::last((f$time))
  cf_units <- cf_var_units1

  identifier <- paste(model_name, "PRLA", sep="_")

  fname <- paste0(identifier,".nc")

  output_file <- file.path(file.path(lake_directory,"data_processed", fname))

  start_time <- min(f$time)
  end_time <- max(f$time)

  data <- f %>%
    dplyr::select(-time)

  diff_time <- as.numeric(difftime(f$time, f$time[1], units = "hours"))

  cf_var_names <- names(data)

  time_dim <- ncdf4::ncdim_def(name="time",
                               units = paste("hours since", format(start_time, "%Y-%m-%d %H:%M")),
                               diff_time, #GEFS forecast starts 5 hours from start time
                               create_dimvar = TRUE)
  lat_dim <- ncdf4::ncdim_def("latitude", "degree_north", lat, create_dimvar = TRUE)
  lon_dim <- ncdf4::ncdim_def("longitude", "degree_east", lon, create_dimvar = TRUE)

  dimensions_list <- list(time_dim, lat_dim, lon_dim)

  nc_var_list <- list()
  for (i in 1:length(cf_var_names)) { #Each ensemble member will have data on each variable stored in their respective file.
    nc_var_list[[i]] <- ncdf4::ncvar_def(cf_var_names[i], cf_units[i], dimensions_list, missval=NaN)
  }

  nc_flptr <- ncdf4::nc_create(output_file, nc_var_list, verbose = FALSE, )

  #For each variable associated with that ensemble
  for (j in 1:ncol(data)) {
    # "j" is the variable number.  "i" is the ensemble number. Remember that each row represents an ensemble
    ncdf4::ncvar_put(nc_flptr, nc_var_list[[j]], unlist(data[,j]))
  }

  ncdf4::nc_close(nc_flptr)  #Write to the disk/storage
}
