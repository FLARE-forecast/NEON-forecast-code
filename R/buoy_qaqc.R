buoy_qaqc <- function(forecast_site,
                      processed_filename,
                      depth_bins,
                      profiler_data = NULL,
                      release = NA){
  
  if(is.null(profiler_data)){
    use_profiler <- FALSE
  }else{
    use_profiler <- TRUE
  }
  
  # Water temperature by depth
  # ----------------------------------------------------------------------------------------
  d1 <- neonstore::neon_read(table = "TSD_30_min-basic", site = forecast_site, release = release)%>%
    select(startDateTime, thermistorDepth, tsdWaterTempMean, siteID, tsdWaterTempFinalQF, verticalPosition) %>%
    filter(tsdWaterTempFinalQF == 0 | (tsdWaterTempFinalQF == 1 & as_date(startDateTime) > as_date("2020-07-01"))) %>% 
    filter(!is.na(thermistorDepth)) %>% 
    mutate(thermistorDepth = ifelse(siteID == "CRAM" & 
                                      lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                      verticalPosition == 502, 1.75, thermistorDepth),
           thermistorDepth = ifelse(siteID == "CRAM" & 
                                      lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                      verticalPosition == 503, 3.45, thermistorDepth),
           thermistorDepth = ifelse(siteID == "CRAM" & 
                                      lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                      verticalPosition == 504, 5.15, thermistorDepth),
           thermistorDepth = ifelse(siteID == "CRAM" & 
                                      lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                      verticalPosition == 505, 6.85, thermistorDepth),
           thermistorDepth = ifelse(siteID == "CRAM" & 
                                      lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                      verticalPosition == 506, 8.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "CRAM" & 
                                      lubridate::as_date(startDateTime) < lubridate::as_date("2020-11-01") & 
                                      verticalPosition == 505, 10.25, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                      lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 502, 0.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                         lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 503, 1.05, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                         lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 504, 1.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                         lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 505, 2.05, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                         lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 506, 2.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) < lubridate::as_date("2021-06-08") | 
                                         lubridate::as_date(startDateTime) > lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 507, 3.05, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 502, 0.3, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 503, 0.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 504, 0.8, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 505, 1.05, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 506, 1.3, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 507, 1.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 508, 2.05, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") & 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 509, 2.55, thermistorDepth),
           thermistorDepth = ifelse(siteID == "BARC" & 
                                      (lubridate::as_date(startDateTime) >= lubridate::as_date("2021-06-08") | 
                                         lubridate::as_date(startDateTime) <= lubridate::as_date("2022-01-07") )& 
                                      verticalPosition == 510, 3.05, thermistorDepth)) %>% 
    arrange(startDateTime, thermistorDepth, siteID) %>%
    rename(depth = thermistorDepth)%>%
    rename(value = tsdWaterTempMean)%>%
    rename(timestamp = startDateTime)%>%
    mutate(variable = "temperature",
           hour = lubridate::hour(timestamp),
           value = ifelse(is.nan(value), NA, value))%>%
    select(timestamp, hour, depth, value, variable, siteID)%>%
    mutate(timestamp = as.Date(timestamp))%>%
    rename(date = timestamp)%>%
    arrange(siteID, date) %>%
    mutate(value = ifelse((siteID == "BARC" & value < 11), NA, value)) %>%
    mutate(value = ifelse((siteID == "PRLA" & year(date) %in% c("2018","2020")), NA, value)) %>% 
    mutate(depth = ifelse(depth < 0.05, 0.05, depth))
  
  if(use_profiler){
  d2 <- profiler_data %>%
    filter(siteID == forecast_site) %>% 
    select(-siteID)%>%
    filter(!is.na(sensorDepth)) %>% 
    filter(sensorDepth >= 0.05) %>% 
    mutate(datetime = lubridate::ymd_hms(startDate),
           date = lubridate::floor_date(datetime, unit = "hour"))%>%
    rename(depth = sensorDepth) %>%
    rename(value = waterTemp) %>%
    mutate(hour = lubridate::hour(date),
           value = ifelse(is.nan(value), NA, value),
           date = as.Date(date)) %>%
    mutate(depth = ifelse(depth < 0.1, 0.1, depth)) %>% 
    #filter(siteID == "BARC",
    #       date >= as_date("2021-03-23") & date <= as_date("2021-06-08")) %>% 
    mutate(variable = "temperature") %>%
    na.omit() %>%
    select(date, hour, depth, value, variable)
  }else{
    d2 <- NULL
  }

  pattern <- "(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])"
  observations <- bind_rows(d1, d2) %>%
    arrange(date) %>%
    mutate(depth_cut = cut(depth, breaks = depth_bins, right = FALSE, dig.lab = 4)) %>%
    group_by(date, hour, depth_cut, variable) %>%
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(depth = as.numeric(gsub(pattern, "\\2",depth_cut))) %>%
    select(date, hour, depth, value, variable)

  readr::write_csv(observations, processed_filename)

  return(processed_filename)
  }
