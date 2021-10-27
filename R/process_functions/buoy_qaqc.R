buoy_qaqc <- function(realtime_buoy_file,
                      prop_neon,
                      input_file_tz,
                      local_tzone,
                      forecast_site,
                      processed_filename){

  d1 <- readr::read_csv(realtime_buoy_file) %>%
    filter(siteID == forecast_site) %>%
    select(-siteID)

  d2 <- prop_neon %>%
    filter(siteID == forecast_site) %>%
    select(-siteID)%>%
    mutate(datetime = lubridate::ymd_hms(datetime),
      date = lubridate::floor_date(datetime, unit = "hour"))%>%
    rename(depth = depth_m)%>%
    rename(value = temp_c)%>%
    mutate(hour = lubridate::hour(date),
           value = ifelse(is.nan(value), NA, value),
           date = as.Date(date),
           depth = ifelse(depth<=0.5,0.5,depth))%>%
    select(date, hour, depth, value)%>%
    group_by(date,hour,depth)%>%
    summarise_at(c("value"), mean, na.rm = TRUE)%>%
    mutate(variable = "temperature")%>%
    ungroup(.)%>%
    arrange(date)

  observations <- bind_rows(d1, d2) %>%
    arrange(date)

  readr::write_csv(observations, processed_filename)
  }
