source(file.path(lake_directory, "R/process_functions/glmtools.R"))
buoy_qaqc <- function(realtime_buoy_file,
                      realtime_kw_file,
                      prop_neon,
                      input_file_tz,
                      local_tzone,
                      forecast_site){

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

  readr::write_csv(observations, file.path(lake_directory, "data_processed", paste0("observations_postQAQC_long_",forecast_site,".csv")))

  kw_site <- readr::read_csv(realtime_kw_file) %>% filter(siteID == forecast_site)
  nml_file = file.path(paste0(lake_directory,"/configuration/", "forecast_model/","glm/", "glm3_",forecast_site,".nml"))
  nml <- read_nml(nml_file)
  get_nml_value(nml, 'Kw')
  new_nml <- set_nml(nml, 'Kw', kw_site$kw)
  get_nml_value(new_nml, 'Kw')
  write_nml(new_nml, file = nml_file)

  }
