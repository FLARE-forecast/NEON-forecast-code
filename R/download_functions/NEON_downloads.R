# Function to extract and clean up the NEON data from NEON
download_neon_files <- function(siteID, buoy_products, start_date){

        # Download newest products
        neonstore::neon_download(product = buoy_products, site = siteID, start_date = start_date)

        # Store the NEON buoy data products
        # neonstore::neon_store("TSD_30_min-basic")
        # neonstore::neon_store("dep_secchi-basic")
        # neonstore::neon_store("dep_profileData-basic")

        # Water temperature by depth
        # ----------------------------------------------------------------------------------------
        water_temp <- neonstore::neon_read(table = "TSD_30_min-basic", site = siteID, start_date = start_date)%>%
          select(endDateTime, thermistorDepth, tsdWaterTempMean, siteID) %>%
          arrange(endDateTime, thermistorDepth, siteID)%>%
          rename(depth = thermistorDepth)%>%
          rename(value = tsdWaterTempMean)%>%
          rename(timestamp = endDateTime)%>%
          mutate(variable = "temperature",
                 hour = lubridate::hour(timestamp),
                 value = ifelse(is.nan(value), NA, value))%>%
          select(timestamp, hour, depth, value, variable, siteID)%>%
                mutate(timestamp = as.Date(timestamp))%>%
                rename(date = timestamp)%>%
                arrange(siteID, date)

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

        write_csv(d, "./data_raw/raw_neon_temp_data.csv")

        Kw <- neonstore::neon_read(table = "dep_secchi-basic", site = forecast_site, start_date = start_date)%>%
                select(secchiMeanDepth, siteID) %>%
                group_by(siteID)%>%
                mutate(kw = 1.7/secchiMeanDepth)%>%
                summarise(kw = mean(kw, na.rm = T))
        readr::write_csv(Kw, file.path(lake_directory, "data_raw", paste0("Kw_",forecast_site,".csv")))

}
