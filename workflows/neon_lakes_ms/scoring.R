#' Compute the CRPS score of your forecast
#'
#' @param forecast forecast data frame or file
#' @param theme theme name. Note: terrestrial must specify the interval.
#' @importFrom dplyr `%>%`
#'
#' @export
#' @examples
#' forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz",
#'                               package = "neon4cast")
#' score(forecast_file, "aquatics")
score <- function(forecast,
                  theme = c("aquatics", "beetles",
                            "phenology", "terrestrial_30min",
                            "terrestrial_daily","ticks")){
  
  theme = match.arg(theme)
  
  ## read from file if necessary
  if(is.character(forecast)){
    filename <- forecast
    forecast <- read_forecast(forecast) %>% mutate(filename = filename)
  }
  ## tables must declare theme and be in "long" form:
  target <- download_target(theme) %>%
    mutate(theme = theme) %>%
    pivot_target()
  forecast <- forecast %>%
    mutate(theme=theme) %>%
    pivot_forecast()
  
  
  crps_logs_score(forecast, target)
  
}




GROUP_VARS = c("theme", "team", "issue_date", "siteID", "time", "depth")
TARGET_VARS = c("oxygen",
                "temperature",
                "richness",
                "abundance",
                "nee",
                "le",
                "vswc",
                "gcc_90",
                "rcc_90",
                "ixodes_scapularis",
                "amblyomma_americanum")
STAT_VARS = c("ensemble", "statistic")
VARS <- c(GROUP_VARS, TARGET_VARS, STAT_VARS)



## utils
isoweek <- function(time) { # Not duckdb-compatible
  ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(time), "-","1"))
}
na_rm <- function(x) as.numeric(stats::na.exclude(x))


## Tidy date formats and drop non-standard columns
## shared by targets + forecasts
standardize_format <- function(df) {
  ## Put tick dates to ISOweek
  ## (arguably should be applied to beetles if not already done too)
  if ("theme" %in% colnames(df) && all(pull(df,theme) == "ticks")) {
    df <- df %>%
      mutate(time = isoweek(time)) %>%
      select(-siteID) %>%
      rename(siteID = plotID)
    
  }
  
  # drop non-standard columns
  df %>% dplyr::select(tidyselect::any_of(VARS))
}


#Select forecasted times using "forecast" flag in standard
select_forecasts <- function(df){
  
  if("forecast" %in% colnames(df)){
    df <- df %>% dplyr::filter(forecast == 1)
  }
  
  df
  
}


#' @importFrom dplyr across any_of




deduplicate_predictions <- function(df){
  
  has_dups <- df %>%
    select(-any_of("predicted")) %>%
    vctrs::vec_group_id() %>%
    vctrs::vec_duplicate_any()
  
  if(has_dups) {
    df <- df %>%
      filter(!is.na(predicted)) %>%
      group_by(across(-any_of("predicted"))) %>%
      filter(dplyr::row_number() == 1L)
  }
  
  df
}

## Parses neon4cast challenge forecast filename components.
split_filename <- function(df){
  
  ## arguably better to split on "-" and unite date components?
  if("filename" %in% colnames(df)) {
    pattern <- "(\\w+)\\-(\\d{4}\\-\\d{2}\\-\\d{2})\\-(\\w+)\\.(csv)?(\\.gz)?(nc)?"
    df <- df %>%
      mutate(theme = gsub(pattern, "\\1", basename(filename)),
             issue_date = gsub(pattern, "\\2", basename(filename)),
             team = gsub(pattern, "\\3", basename(filename)))
  }
  df
}


pivot_target <- function(df){
  
  df %>%
    standardize_format() %>%
    tidyr::pivot_longer(tidyselect::any_of(TARGET_VARS),
                        names_to = "target",
                        values_to = "observed") %>%
    filter(!is.na(observed))
}


pivot_forecast <- function(df){
  
  df <- df %>%
    split_filename() %>%
    standardize_format() %>%
    tidyr::pivot_longer(tidyselect::any_of(TARGET_VARS),
                        names_to = "target",
                        values_to = "predicted")
  
  
  
  
  df <- deduplicate_predictions(df)
  
  if("statistic" %in% colnames(df)){
    df <- df %>%
      tidyr::pivot_wider(names_from = statistic,
                         values_from = predicted)
  }
  
  df
}



## Teach crps to treat any NA observations as NA scores:
crps_sample <- function(y, dat) {
  tryCatch(scoringRules::crps_sample(y, dat),
           error = function(e) NA_real_, finally = NA_real_)
}

crps_norm <- function(y, mean, sd) {
  tryCatch(scoringRules::crps_norm(y, mean = mean, sd = sd),
           error = function(e) NA_real_, finally = NA_real_)
}

## Teach crps to treat any NA observations as NA scores:
logs_sample <- function(y, dat) {
  tryCatch(scoringRules::logs_sample(y, dat),
           error = function(e) NA_real_, finally = NA_real_)
}

logs_norm <- function(y, mean, sd) {
  tryCatch(scoringRules::logs_norm(y, mean = mean, sd = sd),
           error = function(e) NA_real_, finally = NA_real_)
}



## Requires that forecasts and targets have already been cleaned & pivoted!
crps_logs_score <- function(forecast, target){
  
  ## FIXME ensure either both or none have "theme", "issue_date", "team"
  # left join will keep predictions even where we have no observations
  joined <- dplyr::left_join(forecast, target)
  
  if("ensemble" %in% colnames(joined)){
    out <- joined %>%
      group_by(across(-any_of(c("ensemble", "predicted")))) %>%
      summarise(mean = mean(predicted, na.rm =TRUE),
                sd = sd(predicted, na.rm =TRUE),
                crps = crps_sample(observed[[1]], na_rm(predicted)),
                logs = logs_sample(observed[[1]], na_rm(predicted)),
                quantile02.5 = stats::quantile(predicted, 0.025, na.rm = TRUE),
                quantile10 = stats::quantile(predicted, 0.10, na.rm = TRUE),
                quantile20 = stats::quantile(predicted, 0.20, na.rm = TRUE),
                quantile30 = stats::quantile(predicted, 0.30, na.rm = TRUE),
                quantile40 = stats::quantile(predicted, 0.40, na.rm = TRUE),
                quantile50 = stats::quantile(predicted, 0.50, na.rm = TRUE),
                quantile60 = stats::quantile(predicted, 0.60, na.rm = TRUE),
                quantile70 = stats::quantile(predicted, 0.70, na.rm = TRUE),
                quantile80 = stats::quantile(predicted, 0.80, na.rm = TRUE),
                quantile90 = stats::quantile(predicted, 0.90, na.rm = TRUE),
                quantile97.5 = stats::quantile(predicted, 0.975, na.rm = TRUE),
      ) %>% ungroup()
    
  } else {
    out <- joined  %>%
      dplyr::mutate(crps = crps_norm(observed, mean, sd),
                    logs = logs_norm(observed, mean, sd),
                    quantile02.5 = stats::qnorm( 0.025, mean, sd),
                    quantile10 = stats::qnorm(0.10, mean, sd),
                    quantile20 = stats::qnorm(0.20, mean, sd),
                    quantile30 = stats::qnorm(0.30, mean, sd),
                    quantile40 = stats::qnorm(0.40, mean, sd),
                    quantile50 = stats::qnorm(0.50, mean, sd),
                    quantile60 = stats::qnorm(0.60, mean, sd),
                    quantile70 = stats::qnorm(0.70, mean, sd),
                    quantile80 = stats::qnorm(0.80, mean, sd),
                    quantile90 = stats::qnorm(0.90, mean, sd),
                    quantile97.5 = stats::qnorm(0.975, mean, sd))
    
  }
  ## Ensure both ensemble and stat-based have identical column order:
  out %>% select(any_of(c("theme", "team", "issue_date", "siteID","depth", "time",
                          "target", "mean", "sd", "observed", "crps",
                          "logs", "quantile02.5", "quantile10", "quantile20" ,
                          "quantile30", "quantile40", "quantile50", "quantile60",
                          "quantile70", "quantile80", "quantile90", "quantile97.5",
                          "interval",
                          "forecast_start_time")))
}



include_horizon <- function(df){
  
  interval <- df %>%
    group_by(across(any_of(c("theme", "team", "issue_date", "target", "siteID", "depth")))) %>%
    summarise(interval = min(time-dplyr::lag(time), na.rm=TRUE),
              forecast_start_time = min(time) - interval,
              .groups = "drop")
  
  ## add columns for start_time and horizon
  df %>%
    left_join(interval) %>%
    mutate(horizon = time - forecast_start_time)
}



## score_it is batch-oriented: takes a large batch of forecast files,
## outputs scores to disk.  This avoid the need to store all forecasts and
## scores in working RAM at the same time.
score_it <- function(targets_file,
                     forecast_files,
                     dir = "scores"
){
  
  dir.create(dir, FALSE, TRUE)
  theme <- strsplit(basename(targets_file), "[-_]")[[1]][[1]]
  
  ## Target is processed only once
  target <-
    readr::read_csv(targets_file, show_col_types = FALSE,
                    lazy = FALSE, progress = FALSE) %>%
    mutate(theme = theme) %>%
    pivot_target()
  
  ## read, format, and score and write out each forecast file
  suppressMessages({
    furrr::future_walk(forecast_files,
                       function(forecast_file, target){
                         forecast_file %>%
                           read_forecast() %>%
                           mutate(filename = forecast_file) %>%
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


## construct filename from columns and write to disk
write_scores <- function(scores, dir = "scores"){
  r <- utils::head(scores,1)
  output <- file.path(dir,
                      paste0(paste("scores", r$theme, r$time, r$team, sep="-"),
                             ".csv.gz")
  )
  
  readr::write_csv(scores, output)
  invisible(output)
  
}

write_scores_s3 <- function(df){
  r <- utils::head(df,1)
  output <- paste0(paste("scores", r$theme, r$time, r$team, sep="-"),
                   ".csv.gz")
  
  aws.s3::s3write_using(FUN = readr::write_csv,
                        x = df,
                        object = file.path(r$theme, output),
                        bucket = "scores",
                        opts = list(
                          base_url = "flare-forecast.org",
                          region = "s3"))
}

write_scores <- function(df, dir){
  r <- utils::head(df,1)
  output <- paste0(paste("scores", r$theme, r$time, r$team, sep="-"),
                   ".csv.gz")
  
  readr::write_csv(x = df, file = file.path(dir, r$theme, output))
}

read_forecast_s3 <- function(x, grouping_variables, target_variables){
  aws.s3::s3read_using(FUN = read_forecast,
                       grouping_variables = grouping_variables,
                       target_variables = target_variables,
                       object = x,
                       bucket = "forecasts",
                       filename = x,
                       opts = list(
                         base_url = "flare-forecast.org",
                         region = "s3")
  )
}

score_schema  <- function() {
  
  arrow::schema(
    theme      = arrow::string(),
    team       = arrow::string(),
    issue_date = arrow::date32(),
    depth = arrow::float64(),
    time       = arrow::timestamp("s", timezone="UTC"),
    target     = arrow::string(),
    mean       = arrow::float64(),
    sd         = arrow::float64(),
    observed   = arrow::float64(),
    crps       = arrow::float64(),
    logs       = arrow::float64(),
    quantile02.5 = arrow::float64(),
    quantile10 =arrow::float64(),
    quantile20 = arrow::float64(),
    quantile30 = arrow::float64(),
    quantile40 = arrow::float64(),
    quantile50 = arrow::float64(),
    quantile60 = arrow::float64(),
    quantile70 = arrow::float64(),
    quantile80 = arrow::float64(),
    quantile90 = arrow::float64(),
    quantile97.5 = arrow::float64(),
    interval   = arrow::float64(),
    forecast_start_time = arrow::timestamp("s", timezone="UTC"),
    horizon    = arrow::float64()
  )
}


score_spec <-
  list(
    "theme" = readr::col_character(),
    "team" = readr::col_character(),
    "issue_date" = readr::col_character(),
    "depth" = readr::col_double(), 
    "time" = readr::col_datetime(),
    "target"  = readr::col_character(),
    "mean" = readr::col_double(),
    "sd" = readr::col_double(),
    "observed" = readr::col_double(),
    "crps" = readr::col_double(),
    "logs" = readr::col_double(),
    "quantile02.5" = readr::col_double(),
    "quantile10" = readr::col_double(),
    "quantile20" = readr::col_double(),
    "quantile30" = readr::col_double(),
    "quantile40" = readr::col_double(),
    "quantile50" = readr::col_double(),
    "quantile60" = readr::col_double(),
    "quantile70" = readr::col_double(),
    "quantile80" = readr::col_double(),
    "quantile90" = readr::col_double(),
    "quantile97.5" = readr::col_double(),
    "interval" = readr::col_integer(),
    "forecast_start_time" = readr::col_datetime()
  )

utils::globalVariables(c("observed", "predicted", "value",
                         "variable", "statistic", "sd",
                         "filename"))