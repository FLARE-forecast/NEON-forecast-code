library(tidyverse)
library(arrow)
library(lubridate)

source("R/ignore_sigpipe.R")

Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")


NEON_sites <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(field_site_subtype == 'Lake') %>%
  dplyr::distinct(field_site_id) %>%
  dplyr::pull()

#### WITH DATA ASSIMILATION ####
flare_model_name <- 'Simstrat'
challenge_model_name <- 'flareSimstrat'
force <- FALSE


# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/forecasts/parquet/",
                              endpoint_override = "renc.osn.xsede.org",
                              anonymous=TRUE)

today <- paste(Sys.Date(), '00:00:00')
# yesterday <- paste((Sys.Date() - days(1)), '00:00:00')

this_year <- as.character(seq.Date(as_date('2024-01-01'), Sys.Date(), by = 'day'))

# check for missed submissions 
flare_dates  <- arrow::open_dataset(forecasts) |> 
  dplyr::filter(site_id %in% NEON_sites, 
                reference_date %in% this_year, 
                model_id == flare_model_name) |> 
  dplyr::distinct(reference_datetime) |>  
  dplyr::pull(as_Vector = T) 
flare_dates <- sort(flare_dates)

# Get all the submissions 
submissions <- aws.s3::get_bucket_df("bio230014-bucket01", 
                                     prefix = "challenges/forecasts/raw",
                                     region = "sdsc",
                                     base_url = "osn.xsede.org",
                                     max = Inf)

# are these dates in the challenge?
for (i in 1:length(flare_dates)) {
  
  forecast_date <- as.character(flare_dates[i])
  forecast_file <- paste0('aquatics-', forecast_date, '-', challenge_model_name, '.csv.gz')
  
  exists <- nrow(dplyr::filter(submissions, stringr::str_detect(Key, forecast_file))) > 0
  
  
  if (exists == T) {
    message(forecast_file, ' already submitted')
  } 
  if (exists == F) {
    open_ds <- arrow::open_dataset(forecasts) %>%
      dplyr::filter(site_id %in% NEON_sites, 
                    reference_date == forecast_date,
                    datetime > as_datetime(reference_datetime),
                    model_id == flare_model_name,
                    depth <= 1) %>% 
      dplyr::collect() 
    
    challenge_submission <- open_ds %>%
      dplyr::filter(variable == "temperature",
                    datetime >= reference_datetime) %>%
      # FLARE output at multiple depths
      # Need a single "surface" average
      dplyr::group_by(site_id, datetime, parameter, reference_datetime,
                      family, variable) %>%
      dplyr::summarise(prediction = mean(prediction)) %>% 
      dplyr::mutate(model_id = challenge_model_name, 
                    reference_datetime = gsub(' 00:00:00', '', reference_datetime),
                    site_id = ifelse(site_id == 'TOOL', 'TOOK', site_id))%>%
      
      dplyr::select(c('datetime', 'reference_datetime', 'site_id', 'family',
                      'parameter', 'variable', 'prediction', 'model_id'))  
    
    # Write the submission
    file_to_submit <- paste0('aquatics-', forecast_date, '-', challenge_model_name, '.csv.gz')
    
    readr::write_csv(challenge_submission, file_to_submit)
    # Submit forecast!
    
    # Now we can submit the forecast output to the Challenge using 
    neon4cast::forecast_output_validator(file_to_submit)
    neon4cast::submit(forecast_file = file_to_submit,
                      ask = F)
    message('submitting missed forecast from: ', file_to_submit)
  }
}

###### NO DATA ASSIMILATION FORECAST #####
flare_model_name <- 'flareSimstrat_noDA'
challenge_model_name <- 'flareSimstrat_noDA'
force <- FALSE


# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/forecasts/parquet/",
                              endpoint_override = "renc.osn.xsede.org",
                              anonymous=TRUE)

today <- paste(Sys.Date(), '00:00:00')
# yesterday <- paste((Sys.Date() - days(1)), '00:00:00')

this_year <- as.character(seq.Date(as_date('2024-01-01'), Sys.Date(), by = 'day'))

# check for missed submissions 
flare_dates  <- arrow::open_dataset(forecasts) |> 
  dplyr::filter(site_id %in% NEON_sites, 
                reference_date %in% this_year, 
                model_id == flare_model_name) |> 
  dplyr::distinct(reference_datetime) |>  
  dplyr::pull(as_vector = T) 
flare_dates <- sort(flare_dates)

# Get all the submissions 
submissions <- aws.s3::get_bucket_df("bio230014-bucket01", 
                                     prefix = "challenges/forecasts/raw",
                                     region = "sdsc",
                                     base_url = "osn.xsede.org",
                                     max = Inf)

# are these dates in the challenge?
for (i in 1:length(flare_dates)) {
  
  forecast_date <- as.character(flare_dates[i])
  forecast_file <- paste0('aquatics-', forecast_date, '-', challenge_model_name, '.csv.gz')
  
  exists <- nrow(dplyr::filter(submissions, stringr::str_detect(Key, forecast_file))) > 0
  
  
  if (exists == T) {
    message(forecast_file, ' already submitted')
  } 
  if (exists == F) {
    open_ds <- arrow::open_dataset(forecasts) %>%
      dplyr::filter(site_id %in% NEON_sites, 
                    reference_date == forecast_date,
                    datetime > as_datetime(reference_datetime),
                    model_id == flare_model_name,
                    depth <= 1) %>% 
      dplyr::collect() 
    
    challenge_submission <- open_ds %>%
      dplyr::filter(variable == "temperature",
                    datetime >= reference_datetime) %>%
      # FLARE output at multiple depths
      # Need a single "surface" average
      dplyr::group_by(site_id, datetime, parameter, reference_datetime,
                      family, variable) %>%
      dplyr::summarise(prediction = mean(prediction)) %>% 
      dplyr::mutate(model_id = challenge_model_name, 
                    reference_datetime = gsub(' 00:00:00', '', reference_datetime),
                    site_id = ifelse(site_id == 'TOOL', 'TOOK', site_id))%>%
      
      dplyr::select(c('datetime', 'reference_datetime', 'site_id', 'family',
                      'parameter', 'variable', 'prediction', 'model_id'))  
    
    # Write the submission
    file_to_submit <- paste0('aquatics-', forecast_date, '-', challenge_model_name, '.csv.gz')
    
    readr::write_csv(challenge_submission, file_to_submit)
    # Submit forecast!
    
    # Now we can submit the forecast output to the Challenge using 
    neon4cast::forecast_output_validator(file_to_submit)
    neon4cast::submit(forecast_file = file_to_submit,
                      ask = F)
    message('submitting missed forecast from: ', file_to_submit)
  }
}
