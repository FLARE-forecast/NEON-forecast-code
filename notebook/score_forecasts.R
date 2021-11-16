lake_directory <- here::here()
library(tidyverse)
library(lubridate)
source(file.path(lake_directory, "notebook", "read_forecast.R"))
source(file.path(lake_directory, "notebook", "scoring.R"))


sites <- c("BARC", "CRAM", "LIRO", "PRLA", "PRPO", "SUGG")

sites <- c("BARC", "CRAM")

#sites <- "BARC"

for(i in 1:length(sites)){

  theme <- sites[i]

  targets_file <- file.path(lake_directory, "targets", theme, paste0(theme,"-targets-insitu.csv"))
  dir <- file.path(lake_directory, "scores")
  dir.create(dir, FALSE, TRUE)
  theme <- strsplit(basename(targets_file), "[-_]")[[1]][[1]]

  ## Target is processed only once
  target <- readr::read_csv(targets_file, show_col_types = FALSE, lazy = FALSE, progress = FALSE) %>%
    mutate(theme = theme,
           time = date + lubridate::hours(hour)) %>%
    rename("observed" = value,
           "target" = "variable") %>%
    select(time, depth, target, observed, theme)

  forecast_files <- list.files(file.path(lake_directory, "forecasts", theme), full.names = TRUE)
  forecast_files <- forecast_files[grepl(paste0(theme, "-"), forecast_files)]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "xml")]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "pdf")]
  forecast_files <- forecast_files[!stringr::str_detect(forecast_files, "FLARE.csv")]

  ## read, format, and score and write out each forecast file
  suppressMessages({
    furrr::future_walk(forecast_files,
                       function(forecast_file, target){
                         forecast_file %>%
                           read_forecast(grouping_variables = c("time", "depth"),
                                         target_variables = "temp",
                                         reps_col = "ensemble") %>%
                           mutate(filename = forecast_file) %>%
                           rename_at(vars(matches("temp")), ~"temperature") %>%
                           select_forecasts() %>%
                           pivot_forecast() %>%
                           crps_logs_score(target) %>%
                           include_horizon() %>%
                           write_scores(dir)
                       },
                       target = target
    )
  })

}

