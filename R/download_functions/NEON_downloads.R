# Function to extract and clean up the NEON data from NEON
download_neon_files <- function(siteID, buoy_products, start_date, raw_data_directory){

        # Download newest products
        neonstore::neon_download(product = buoy_products, site = siteID, start_date = start_date)

        # Water temperature by depth
        # ----------------------------------------------------------------------------------------
        water_temp <- neonstore::neon_read(table = "TSD_30_min-basic", site = siteID, start_date = start_date)%>%
          select(startDateTime, thermistorDepth, tsdWaterTempMean, siteID, tsdWaterTempFinalQF) %>%
          filter(tsdWaterTempFinalQF == 0 | (tsdWaterTempFinalQF == 1 & as_date(startDateTime) > as_date("2020-07-01"))) %>%
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
          mutate(value = ifelse((siteID == "PRLA" & year(date) %in% c("2018","2020")), NA, value))

        temp_profiles <- neonstore::neon_read(table = "dep_profileData-basic", site = siteID, start_date = start_date)%>%
                select(date, sampleDepth, waterTemp, siteID) %>%
                arrange(date, siteID, sampleDepth)%>%
                rename(depth = sampleDepth)%>%
                rename(value = waterTemp)%>%
                rename(timestamp = date)%>%
                mutate(variable = "temperature",
                       hour = lubridate::hour(timestamp),
                       value = ifelse(is.nan(value), NA, value))%>%
                select(timestamp, hour, depth, value, variable, siteID)%>%
                mutate(timestamp = as.Date(timestamp))%>%
                rename(date = timestamp)%>%
                arrange(siteID, date)

        d <- bind_rows(water_temp, temp_profiles) %>% arrange(siteID, date)
        d$value <- as.numeric(d$value)

        write_csv(d, file.path(raw_data_directory, "raw_neon_temp_data.csv"))
}
