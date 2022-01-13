lake_directory <- getwd()
lake_directory <- here::here()
library(tidyverse)
library(lubridate)
source(file.path(lake_directory, "workflows","neon_lakes_ms", "read_forecast.R"))
source(file.path(lake_directory, "workflows","neon_lakes_ms", "scoring.R"))


sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG")

forecast_directory <- "/data"

for(i in 1:length(sites)){

  message(sites[i])

  theme <- sites[i]

  targets_file <- file.path(theme, paste0(theme,"-targets-insitu.csv"))

  target <- aws.s3::s3read_using(FUN = readr::read_csv,
                                 show_col_types = FALSE,
                                 lazy = FALSE,
                                 progress = FALSE,
                                 object = targets_file,
                                 bucket = "targets",
                                 filename = basename(targets_file),
                                 opts = list(
                                   base_url = "flare-forecast.org",
                                   region = "s3")) %>%
    mutate(theme = theme,
           time = date + lubridate::hours(hour)) %>%
    rename("observed" = value,
           "target" = "variable") %>%
    select(time, depth, target, observed, theme)

  forecast_files <- aws.s3::get_bucket(bucket = "forecasts", prefix = sites[i])
  keys <- vapply(forecast_files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  forecast_files <- keys[!empty]

  #forecast_files <- list.files(file.path(forecast_directory, "forecasts", theme), full.names = TRUE)
  forecast_files <- forecast_files[grepl(paste0(theme, "-"), forecast_files)]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "xml")]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "pdf")]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "FLARE.csv")]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "persistence")]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "test_barc2")]

  ## read, format, and score and write out each forecast file
  suppressMessages({
    furrr::future_walk(forecast_files,
                       function(forecast_file, target){
                         forecast_file %>%
                           read_forecast_s3(grouping_variables = c("time", "depth"),
                                         target_variables = "temp") %>%
                           mutate(filename = forecast_file) %>%
                           rename_at(vars(matches("temp")), ~"temperature") %>%
                           select_forecasts() %>%
                           pivot_forecast() %>%
                           crps_logs_score(target) %>%
                           include_horizon() %>%
                           write_scores_s3()
                       },
                       target = target
    )
  })

}

