met_qaqc <- function(realtime_file,
                     cleaned_met_file_dir,
                     input_file_tz,
                     local_tzone){
  
    d1 <- readr::read_csv(realtime_file)
    d1$time <- lubridate::force_tz(d1$time, tzone = "UTC")
    
    # Impute Barco
    a <- d1 %>% filter(siteID == "BARC")
    a <- as.data.frame(a)
    
    #ShortWave
    amelia.sw <- amelia(a, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave", idvars = "siteID")
    sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
      select(time, ShortWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
    
    #LongWave
    amelia.lw <- amelia(a, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave", idvars = "siteID")
    lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
      select(time, LongWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #AirTemp
    amelia.at <- amelia(a, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp", idvars = "siteID")
    at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
      select(time, AirTemp)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #Himidity
    amelia.rh <- amelia(a, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum", idvars = "siteID")
    rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
      select(time, RelHum)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
    
    #Rain
    amelia.pr <- amelia(a, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain", idvars = "siteID")
    pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
      select(time, Rain)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(Rain = ifelse(Rain <= 0, 0, Rain))
    
    #WindSpeed
    amelia.ws <- amelia(a, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed", idvars = "siteID")
    ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
      select(time, WindSpeed)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
    
    #Pressure
    amelia.p <- amelia(a, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure", idvars = "siteID")
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
    
    met_new <- left_join(a, imputed, by = "time")
    
    a <- met_new %>%
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
    
    a$time <- lubridate::force_tz(a$time, tzone = "UTC") #input_file_tz
    
    wshgt <- 3
    roughlength <- 0.000114
    maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
    minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
    
    a <- a %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                             relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                             relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                             relative_humidity = specific_humidity / 100,
                             air_temperature = air_temperature + 273.15,
                             surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                             wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                             wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
      filter(is.na(time) == FALSE)
    
    
    a$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = a$relative_humidity,
                                                    T = a$air_temperature,
                                                    press = a$air_pressure)
    
    a <- a %>%select(time, 
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
    
    a <- a %>%
      tidyr::drop_na()
    
    
    p <- a %>% reshape2::melt(., id = "time") %>% ggplot(.) +
      geom_line(aes(time, value)) + labs(title = paste0(siteID[i]," Post imputation"))+
      facet_wrap(~variable, scales = "free_y") +
      theme_classic()
    
    return(p)
    
    model_name <- "observed-met"
    site <- "BARC"
    lat <- 29.67562
    lon <- -82.0085
    start_time <- dplyr::first((a$time))
    end_time <- dplyr::last((a$time))
    cf_units <- cf_var_units1
    
    identifier <- paste(model_name, "BARC", sep="_")
    
    fname <- paste0(identifier,".nc")
    
    output_file <- file.path(file.path(lake_directory,"data_processed", fname))
    
    start_time <- min(a$time)
    end_time <- max(a$time)
    
    data <- a %>%
      dplyr::select(-time)
    
    diff_time <- as.numeric(difftime(a$time, a$time[1], units = "hours"))
    
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
    
    # Impute Suggs
    b <- d1 %>% filter(siteID == "SUGG")
    b <- as.data.frame(b)
    
    #ShortWave
    amelia.sw <- amelia(b, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave", idvars = "siteID")
    sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
      select(time, ShortWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
    
    #LongWave
    amelia.lw <- amelia(b, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave", idvars = "siteID")
    lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
      select(time, LongWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #AirTemp
    amelia.at <- amelia(b, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp", idvars = "siteID")
    at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
      select(time, AirTemp)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #Himidity
    amelia.rh <- amelia(b, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum", idvars = "siteID")
    rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
      select(time, RelHum)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
    
    #Rain
    amelia.pr <- amelia(b, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain", idvars = "siteID")
    pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
      select(time, Rain)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(Rain = ifelse(Rain <= 0, 0, Rain))
    
    #WindSpeed
    amelia.ws <- amelia(b, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed", idvars = "siteID")
    ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
      select(time, WindSpeed)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
    
    #Pressure
    amelia.p <- amelia(b, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure", idvars = "siteID")
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
    
    met_new <- left_join(b, imputed, by = "time")
    
    b <- met_new %>%
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
    
    b$time <- lubridate::force_tz(b$time, tzone = "UTC") #input_file_tz
    
    wshgt <- 3
    roughlength <- 0.000114
    maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
    minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
    
    b <- b %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                             relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                             relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                             relative_humidity = specific_humidity / 100,
                             air_temperature = air_temperature + 273.15,
                             surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                             wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                             wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
      filter(is.na(time) == FALSE)
    
    
    b$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = b$relative_humidity,
                                                    T = b$air_temperature,
                                                    press = b$air_pressure)
    
    b <- b %>%select(time, 
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
    
    b <- b %>%
      tidyr::drop_na()
    
    
    e <- b %>% reshape2::melt(., id = "time") %>% ggplot(.) +
      geom_line(aes(time, value)) + labs(title = paste0(siteID[i]," Post imputation"))+
      facet_wrap(~variable, scales = "free_y") +
      theme_classic()
    
    return(e)
    
    model_name <- "observed-met"
    site <- "SUGG"
    lat <- 29.68778
    lon <- -82.017745
    start_time <- dplyr::first((d$time))
    end_time <- dplyr::last((d$time))
    cf_units <- cf_var_units1
    
    identifier <- paste(model_name, "SUGG", sep="_")
    
    fname <- paste0(identifier,".nc")
    
    output_file <- file.path(file.path(lake_directory,"data_processed", fname))
    
    start_time <- min(b$time)
    end_time <- max(b$time)
    
    data <- b %>%
      dplyr::select(-time)
    
    diff_time <- as.numeric(difftime(b$time, b$time[1], units = "hours"))
    
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
    

    # Impute Little Rock
    c <- d1 %>% filter(siteID == "LIRO")
    c <- as.data.frame(c)
    
    #ShortWave
    amelia.sw <- amelia(c, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave", idvars = "siteID")
    sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
      select(time, ShortWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
    
    #LongWave
    amelia.lw <- amelia(c, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave", idvars = "siteID")
    lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
      select(time, LongWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #AirTemp
    amelia.at <- amelia(c, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp", idvars = "siteID")
    at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
      select(time, AirTemp)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #Himidity
    amelia.rh <- amelia(c, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum", idvars = "siteID")
    rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
      select(time, RelHum)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
    
    #Rain
    amelia.pr <- amelia(c, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain", idvars = "siteID")
    pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
      select(time, Rain)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(Rain = ifelse(Rain <= 0, 0, Rain))
    
    #WindSpeed
    amelia.ws <- amelia(c, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed", idvars = "siteID")
    ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
      select(time, WindSpeed)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
    
    #Pressure
    amelia.p <- amelia(c, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure", idvars = "siteID")
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
    
    met_new <- left_join(c, imputed, by = "time")
    
    c <- met_new %>%
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
    
    c$time <- lubridate::force_tz(c$time, tzone = "UTC") #input_file_tz
    
    wshgt <- 3
    roughlength <- 0.000114
    maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
    minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
    
    c <- c %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                             relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                             relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                             relative_humidity = specific_humidity / 100,
                             air_temperature = air_temperature + 273.15,
                             surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                             wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                             wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
      filter(is.na(time) == FALSE)
    
    
    c$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = c$relative_humidity,
                                                    T = c$air_temperature,
                                                    press = c$air_pressure)
    
    c <- c %>%select(time, 
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
    
    c <- c %>%
      tidyr::drop_na()
    
    
    d <- c %>% reshape2::melt(., id = "time") %>% ggplot(.) +
      geom_line(aes(time, value)) + labs(title = paste0(siteID[i]," Post imputation"))+
      facet_wrap(~variable, scales = "free_y") +
      theme_classic()
    
    return(d)
    
    model_name <- "observed-met"
    site <- "LIRO"
    lat <- 45.998269
    lon <- -89.704767
    start_time <- dplyr::first((c$time))
    end_time <- dplyr::last((c$time))
    cf_units <- cf_var_units1
    
    identifier <- paste(model_name, "LIRO", sep="_")
    
    fname <- paste0(identifier,".nc")
    
    output_file <- file.path(file.path(lake_directory,"data_processed", fname))
    
    start_time <- min(c$time)
    end_time <- max(c$time)
    
    data <- c %>%
      dplyr::select(-time)
    
    diff_time <- as.numeric(difftime(c$time, c$time[1], units = "hours"))
    
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
    

    # Impute Crampton
    d <- d1 %>% filter(siteID == "CRAM")
    d <- as.data.frame(d)
    
    #ShortWave
    amelia.sw <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave", idvars = "siteID")
    sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
      select(time, ShortWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
    
    #LongWave
    amelia.lw <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave", idvars = "siteID")
    lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
      select(time, LongWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #AirTemp
    amelia.at <- amelia(d, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp", idvars = "siteID")
    at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
      select(time, AirTemp)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #Himidity
    amelia.rh <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum", idvars = "siteID")
    rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
      select(time, RelHum)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
    
    #Rain
    amelia.pr <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain", idvars = "siteID")
    pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
      select(time, Rain)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(Rain = ifelse(Rain <= 0, 0, Rain))
    
    #WindSpeed
    amelia.ws <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed", idvars = "siteID")
    ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
      select(time, WindSpeed)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
    
    #Pressure
    amelia.p <- amelia(d, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure", idvars = "siteID")
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
    
    met_new <- left_join(d, imputed, by = "time")
    
    d <- met_new %>%
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
    
    d$time <- lubridate::force_tz(d$time, tzone = "UTC") #input_file_tz
    
    wshgt <- 3
    roughlength <- 0.000114
    maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
    minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
    
    d <- d %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                             relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                             relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                             relative_humidity = specific_humidity / 100,
                             air_temperature = air_temperature + 273.15,
                             surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                             wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                             wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
      filter(is.na(time) == FALSE)
    
    
    d$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = d$relative_humidity,
                                                    T = d$air_temperature,
                                                    press = d$air_pressure)
    
    d <- d %>%select(time, 
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
    
    d <- d %>%
      tidyr::drop_na()
    
    
    c <- d %>% reshape2::melt(., id = "time") %>% ggplot(.) +
      geom_line(aes(time, value)) + labs(title = paste0(siteID[i]," Post imputation"))+
      facet_wrap(~variable, scales = "free_y") +
      theme_classic()
    
    return(c)
    
    model_name <- "observed-met"
    site <- "CRAM"
    lat <- 46.209675
    lon <- -89.473688
    start_time <- dplyr::first((d$time))
    end_time <- dplyr::last((d$time))
    cf_units <- cf_var_units1
    
    identifier <- paste(model_name, "CRAM", sep="_")
    
    fname <- paste0(identifier,".nc")
    
    output_file <- file.path(file.path(lake_directory,"data_processed", fname))
    
    start_time <- min(d$time)
    end_time <- max(d$time)
    
    data <- d %>%
      dplyr::select(-time)
    
    diff_time <- as.numeric(difftime(d$time, d$time[1], units = "hours"))
    
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
    

    # Impute Prairie Pothole
    e <- d1 %>% filter(siteID == "PRPO")
    e <- as.data.frame(e)
    
    #ShortWave
    amelia.sw <- amelia(e, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "ShortWave", leads = "ShortWave", idvars = "siteID")
    sw_imputations <- bind_rows(unclass(amelia.sw$imputations), .id = "m") %>%
      select(time, ShortWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(ShortWave = ifelse(ShortWave <= 0, 0, ShortWave))
    
    #LongWave
    amelia.lw <- amelia(e, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "LongWave", leads = "LongWave", idvars = "siteID")
    lw_imputations <- bind_rows(unclass(amelia.lw$imputations), .id = "m") %>%
      select(time, LongWave)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #AirTemp
    amelia.at <- amelia(e, m = 50, polytime = 0, ts = "time", cs = NULL, lags = "AirTemp", leads = "AirTemp", idvars = "siteID")
    at_imputations <- bind_rows(unclass(amelia.at$imputations), .id = "m") %>%
      select(time, AirTemp)%>%
      group_by(time)%>%
      summarise_all(funs(mean))
    
    #Himidity
    amelia.rh <- amelia(e, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "RelHum", leads = "RelHum", idvars = "siteID")
    rh_imputations <- bind_rows(unclass(amelia.rh$imputations), .id = "m") %>%
      select(time, RelHum)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(RelHum = ifelse(RelHum >= 100, 100, RelHum))
    
    #Rain
    amelia.pr <- amelia(e, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Rain", leads = "Rain", idvars = "siteID")
    pr_imputations <- bind_rows(unclass(amelia.pr$imputations), .id = "m") %>%
      select(time, Rain)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(Rain = ifelse(Rain <= 0, 0, Rain))
    
    #WindSpeed
    amelia.ws <- amelia(e, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "WindSpeed", leads = "WindSpeed", idvars = "siteID")
    ws_imputations <- bind_rows(unclass(amelia.ws$imputations), .id = "m") %>%
      select(time, WindSpeed)%>%
      group_by(time)%>%
      summarise_all(funs(mean))%>%
      mutate(WindSpeed = ifelse(WindSpeed <= 0, 0, WindSpeed))
    
    #Pressure
    amelia.p <- amelia(e, m = 50, polytime = 2, ts = "time", cs = NULL, lags = "Pressure", leads = "Pressure", idvars = "siteID")
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
    
    met_new <- left_join(e, imputed, by = "time")
    
    e <- met_new %>%
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
    
    e$time <- lubridate::force_tz(e$time, tzone = "UTC") #input_file_tz
    
    wshgt <- 3
    roughlength <- 0.000114
    maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
    minTempC = -24 # an lower bound of realistic temperature for the study site in deg C
    
    e <- e %>% dplyr::mutate(surface_downwelling_shortwave_flux_in_air = ifelse(surface_downwelling_shortwave_flux_in_air < 0, 0, surface_downwelling_shortwave_flux_in_air),
                             relative_humidity = ifelse(specific_humidity < 0, 0, specific_humidity),
                             relative_humidity = ifelse(specific_humidity > 100, 100, specific_humidity),
                             relative_humidity = specific_humidity / 100,
                             air_temperature = air_temperature + 273.15,
                             surface_downwelling_longwave_flux_in_air = ifelse(surface_downwelling_longwave_flux_in_air < 0, 0, surface_downwelling_longwave_flux_in_air),
                             wind_speed = wind_speed * log(10.00 / 0.000114) / log(wshgt / 0.000114),
                             wind_speed = ifelse(wind_speed < 0, 0, wind_speed)) %>%
      filter(is.na(time) == FALSE)
    
    
    e$specific_humidity <-  noaaGEFSpoint:::rh2qair(rh = e$relative_humidity,
                                                    T = e$air_temperature,
                                                    press = e$air_pressure)
    
    e <- e %>%select(time, 
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
    
    e <- e %>%
      tidyr::drop_na()
    
    
    b <- e %>% reshape2::melt(., id = "time") %>% ggplot(.) +
      geom_line(aes(time, value)) + labs(title = paste0(siteID," Post imputation"))+
      facet_wrap(~variable, scales = "free_y") +
      theme_classic()
    
    return(b)
    
    model_name <- "observed-met"
    site <- "PRPO"
    lat <- 47.129839
    lon <- -99.253147
    start_time <- dplyr::first((e$time))
    end_time <- dplyr::last((e$time))
    cf_units <- cf_var_units1
    
    identifier <- paste(model_name, "PRPO", sep="_")
    
    fname <- paste0(identifier,".nc")
    
    output_file <- file.path(file.path(lake_directory,"data_processed", fname))
    
    start_time <- min(e$time)
    end_time <- max(e$time)
    
    data <- e %>%
      dplyr::select(-time)
    
    diff_time <- as.numeric(difftime(e$time, e$time[1], units = "hours"))
    
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

  

  
  