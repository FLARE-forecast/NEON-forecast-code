# Function to extract and clean up the NEON data from NEON
download_neon_files <- function(siteID_neon, siteID, ECtower, products, buoy_products){

        # # Download newest products
        # neonstore::neon_download(product = products, site = siteID_neon)
        #
        # # Store the NEON met data products
        # # neonstore::neon_store("SECPRE_30min-basic")
        # # neonstore::neon_store("2DWSD_30min-basic")
        # # neonstore::neon_store("SLRNR_30min-basic")
        # # neonstore::neon_store("SAAT_30min-basic")
        # # neonstore::neon_store("RH_30min-basic")
        # # neonstore::neon_store("BP_30min-basic")
        #
        # # Tidy up the met data
        # # Airtemp
        # airtemp <- neonstore::neon_read(table = "SAAT_30min-basic", site = siteID) %>%
        #   select(endDateTime, tempSingleMean, siteID) %>%
        #   mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
        #   select(-endDateTime)%>%
        #   group_by(time, siteID) %>%
        #   summarize_at(c("tempSingleMean"), mean, na.rm = TRUE)%>%
        #   arrange(siteID, time)
        #
        # # Radiation
        # radiation <- neonstore::neon_read(table = "SLRNR_30min-basic", site = siteID) %>%
        #   select(endDateTime, inSWMean, inLWMean, siteID) %>%
        #   mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
        #   select(-endDateTime)%>%
        #   group_by(time, siteID) %>%
        #   summarize_at(c("inSWMean", "inLWMean"), mean, na.rm = TRUE)%>%
        #   arrange(siteID, time)
        #
        # # Humidity
        # humidity <- neonstore::neon_read(table = "RH_30min-basic", site = siteID) %>%
        #   select(endDateTime, RHMean, siteID)%>%
        #   mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
        #   select(-endDateTime)%>%
        #   group_by(time, siteID) %>%
        #   summarize_at(c("RHMean"), mean, na.rm = TRUE)%>%
        #   arrange(time, siteID)
        #
        # # Precipitation
        # precip_1  <- neonstore::neon_read(table = "SECPRE_30min-basic", site = ECtower) %>%
        #   select(endDateTime, secPrecipBulk, siteID) %>%
        #   mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
        #   select(-endDateTime)%>%
        #   group_by(time, siteID) %>%
        #   summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
        #   mutate(siteID = ifelse(siteID == "OSBS", "BARC", siteID),
        #          siteID = ifelse(siteID == "UNDE", "CRAM", siteID),
        #          siteID = ifelse(siteID == "DCFS", "PRPO", siteID))%>%
        #   arrange(time, siteID)
        #
        # precip_2  <- neonstore::neon_read(table = "SECPRE_30min-basic", site = ECtower) %>%
        #         select(endDateTime, secPrecipBulk, siteID) %>%
        #         mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
        #         select(-endDateTime)%>%
        #         group_by(time, siteID) %>%
        #         summarize_at(c("secPrecipBulk"), sum, na.rm = TRUE)%>%
        #         mutate(siteID = ifelse(siteID == "OSBS", "SUGG", siteID),
        #                siteID = ifelse(siteID == "UNDE", "LIRO", siteID),
        #                siteID = ifelse(siteID == "DCFS", "PRLA", siteID))%>%
        #         arrange(time, siteID)
        #
        # precip <- bind_rows(precip_1, precip_2)
        #
        # # Wind Speed
        # windspeed <- neonstore::neon_read(table = "2DWSD_30min-basic", site = siteID)%>%
        #   select(endDateTime, windSpeedMean, siteID)%>%
        #   mutate(time = lubridate::floor_date(endDateTime, unit = "hour"),
        #          windSpeedMean = ifelse(windSpeedMean==0,NA,windSpeedMean))%>%
        #   select(-endDateTime)%>%
        #   group_by(time, siteID) %>%
        #   summarize_at(c("windSpeedMean"), sum, na.rm = TRUE)%>%
        #   arrange(time, siteID)
        #
        # # Pressure
        # pressure <- neonstore::neon_read(table = "BP_30min-basic", site = siteID) %>%
        #   select(endDateTime, staPresMean, siteID)%>%
        #   mutate(time = lubridate::floor_date(endDateTime, unit = "hour"))%>%
        #   select(-endDateTime)%>%
        #   group_by(time, siteID) %>%
        #   summarize_at(c("staPresMean"), mean, na.rm = TRUE)%>%
        #   arrange(time, siteID)
        #
        #
        # met_target <- full_join(radiation, airtemp, by = c("time","siteID"))%>%
        #   full_join(., humidity, by = c("time","siteID"))%>%
        #   full_join(., windspeed, by = c("time","siteID"))%>%
        #   full_join(., precip, by = c("time","siteID"))%>%
        #   full_join(., pressure, by = c("time","siteID"))%>%
        #   rename(ShortWave = inSWMean, LongWave = inLWMean, AirTemp = tempSingleMean,
        #          RelHum = RHMean, WindSpeed = windSpeedMean, Rain = secPrecipBulk, Pressure = staPresMean)%>%
        #   mutate(Rain = Rain/3600)%>% # convert from mm/hr to m/d
        #   mutate(Pressure = Pressure*1000)%>%
        #   mutate(ShortWave = ifelse(ShortWave<=0,0,ShortWave))%>%
        #         group_by(siteID)%>%
        #   filter(time >= "2018-09-01")
        #
        # met_target <- as.data.frame(met_target)
        #
        # write_csv(met_target, "./data_raw/raw_neon_met_data.csv")

        # Download newest products
        neonstore::neon_download(product = buoy_products, site = siteID)


        # Store the NEON buoy data products
        # neonstore::neon_store("TSD_30_min-basic")
        # neonstore::neon_store("dep_secchi-basic")
        # neonstore::neon_store("dep_profileData-basic")

        # Water temperature by depth
        # ----------------------------------------------------------------------------------------
        water_temp <- neonstore::neon_read(table = "TSD_30_min-basic", site = siteID)%>%
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

        temp_profiles <- neonstore::neon_read(table = "dep_profileData-basic", site = siteID)%>%
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

        Kw <- neonstore::neon_read(table = "dep_secchi-basic", site = siteID)%>%
                select(secchiMeanDepth, siteID) %>%
                group_by(siteID)%>%
                mutate(kw = 1.7/secchiMeanDepth)%>%
                summarise(kw = mean(kw, na.rm = T))

        return(Kw)

}
