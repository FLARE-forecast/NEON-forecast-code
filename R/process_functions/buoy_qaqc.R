buoy_qaqc <- function(realtime_buoy_file,
                     input_file_tz,
                     local_tzone,
                     forecast_site){

  d1 <- readr::read_csv(realtime_buoy_file) %>%
    filter(siteID == forecast_site) %>%
    select(-siteID)

  readr::write_csv(d1, file.path(lake_directory, "data_processed", paste0("observations_postQAQC_long_",forecast_site,".csv")))

  }
