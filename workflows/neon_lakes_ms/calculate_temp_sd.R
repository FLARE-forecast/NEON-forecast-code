library(zoo)

sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG")

met_data <- NULL
for(i in 1:length(sites)){
nc <- ncdf4::nc_open(paste0("/data/drivers/noaa/NOAAGEFS_1hr_stacked_average/",sites[i],"/observed-met-noaa_",sites[i],".nc"))
obs_met_time <- ncdf4::ncvar_get(nc, "time")
origin <- stringr::str_sub(ncdf4::ncatt_get(nc,
                                            "time")$units, 13, 28)
origin <- lubridate::ymd_hm(origin)
time <- origin + lubridate::hours(obs_met_time)
air_temperature <- ncdf4::ncvar_get(nc, "air_temperature")

curr_tibble <- tibble(siteID = rep(sites[i], length(time)),
                      time = time,
                      air_temperature = air_temperature)
met_data <- rbind(met_data, curr_tibble)
ncdf4::nc_close(nc)
}


md <- met_data %>%
  mutate(hour = hour(time),
         date = as_date(time)) %>%
  filter(hour %in% c(0,6,12,18)) %>%
  filter(date >= as_date("2021-05-18") & date <= as_date("2021-10-31")) %>%
  group_by(date, siteID) %>%
  summarize(air_temp_day = mean(air_temperature, na.rm = TRUE), .groups = "drop") %>%
  group_by(siteID) %>%
  mutate(roll_sd = rollapply(air_temp_day, width = 10, FUN = sd, fill = NA)) %>%
  summarise(mean = mean(roll_sd, na.rm = TRUE))
