lake_directory <- here::here()
library(tidyverse)
library(lubridate)
source(file.path(lake_directory, "workflows","neon_lakes_ms", "read_forecast.R"))
source(file.path(lake_directory, "workflows","neon_lakes_ms", "scoring.R"))

Sys.setenv('AWS_DEFAULT_REGION' = 's3', 
           'AWS_S3_ENDPOINT' = 'flare-forecast.org', 
           'USE_HTTPS' = TRUE)

sites <- c("BARC", "CRAM", "LIRO", "SUGG", "PRLA", "PRPO")


sim_names <- list(barc = c("ms2_doymean", "ms2_glm_flare"),
                  cram = c("ms2_doymean", "ms2_glm_flare"),
                  liro = c("ms2_doymean", "ms2_glm_flare"),
                  sugg = c("ms2_doymean", "ms2_glm_flare"),
                  prla = c("ms2_doymean", "ms2_glm_flare"),
                  prpo = c("ms2_glm_flare", "ms2_doymean"))

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

  forecast_files <- aws.s3::get_bucket(bucket = "forecasts", prefix = sites[i], max = Inf)
  keys <- vapply(forecast_files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  forecast_files <- keys[!empty]

  forecast_files_all <- forecast_files[grepl(paste0(theme, "-"), forecast_files)]
  forecast_files_all <- forecast_files_all[!stringr::str_detect(forecast_files_all, "xml")]
  forecast_files_all <- forecast_files_all[!stringr::str_detect(forecast_files_all, "pdf")]

  forecast_files <- NULL
  for(k in 1:length(sim_names[[i]])){
    forecast_file_subset <- forecast_files_all[stringr::str_detect(forecast_files_all, sim_names[[i]][k])]
    forecast_files <- c(forecast_files, forecast_file_subset)
  }

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


forecast_file %>% 
  read_forecast_s3(grouping_variables = c("time", "depth"),
                   target_variables = "temp") %>%
  mutate(filename = forecast_file) %>%
  rename_at(vars(matches("temp")), ~"temperature") %>%
  select_forecasts() %>%
  pivot_forecast() %>%
  crps_logs_score(target) %>%
  include_horizon() %>% 
  write_csv("test.csv")

aws.s3::put_object("test.csv", "test.csv", bucket = "targets")
