buoy_qaqc <- function(realtime_buoy_file,
                      prop_neon,
                      input_file_tz,
                      local_tzone,
                      forecast_site,
                      processed_filename){

  d1 <- readr::read_csv(realtime_buoy_file, show_col_types = FALSE) %>%
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
    mutate(variable = "temperature")%>%
    na.omit() %>%
    select(date, hour, depth, value, variable)

  observations <- bind_rows(d1, d2) %>%
    arrange(date) %>%
    mutate(depth_bins = cut(depth, breaks = seq(0,max(depth), by = 0.5))) %>%
    group_by(date, hour, depth_bins, variable) %>%
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(lower = as.numeric( sub("\\((.+),.*", "\\1", depth_bins) ),
           upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", depth_bins)),
           depth = (lower + upper)/2) %>%
    select(date, hour, depth, value, variable)

  readr::write_csv(observations, processed_filename)

  return(processed_filename)
  }
