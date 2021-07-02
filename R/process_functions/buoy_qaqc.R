buoy_qaqc <- function(realtime_buoy_file,
                      realtime_sonde_file,
                      input_file_tz,
                      local_tzone,
                      forecast_site){

  d1 <- readr::read_csv(realtime_buoy_file) %>%
    filter(siteID == forecast_site) %>%
    select(-siteID)

  d2 <- readr::read_csv(realtime_sonde_file) %>%
    filter(siteID == forecast_site) %>%
    select(-siteID)%>%
    mutate(date = lubridate::floor_date(datetime, unit = "hour"))%>%
    rename(depth = depth_m)%>%
    rename(value = temp_c)%>%
    mutate(hour = lubridate::hour(date),
           value = ifelse(is.nan(value), NA, value),
           depth = ifelse(depth <= 0.5, 0.5, depth),
           date = as.Date(date))%>%
    select(date, hour, depth, value)%>%
    group_by(date,hour,depth)%>%
    summarise_at(c("value"), mean, na.rm = TRUE)%>%
    mutate(variable = "temperature")%>%
    ungroup(.)%>%
    arrange(date)

  observations <- bind_rows(d1, d2) %>%
    arrange(date)

  readr::write_csv(observations, file.path(lake_directory, "data_processed", paste0("observations_postQAQC_long_",forecast_site,".csv")))

  }
