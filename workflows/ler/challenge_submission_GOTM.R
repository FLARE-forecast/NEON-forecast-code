library(tidyverse)
library(arrow)
library(lubridate)


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
flare_model_name <- 'GOTM'
challenge_model_name <- 'flareGOTM'
force <- FALSE


# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "forecasts/parquet",
                              endpoint_override = "s3.flare-forecast.org",
                              anonymous=TRUE)

this_year <- as.character(paste0(seq.Date(as_date('2023-03-07'), Sys.Date(), by = 'day'), ' 00:00:00'))

# check for missed submissions 
flare_dates  <- arrow::open_dataset(forecasts) |> 
  dplyr::filter(site_id %in% NEON_sites, 
                reference_datetime %in% this_year, 
                model_id == flare_model_name) |> 
  dplyr::distinct(reference_datetime) |>  
  dplyr::pull(as_vector = T) 

challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# are these dates in the challenge?
for (i in 1:length(flare_dates)) {
  
  forecast_file <- paste0('aquatics-', as_date(flare_dates[i]), '-', challenge_model_name, '.csv.gz')
  
  exists <- suppressMessages(aws.s3::object_exists(object = file.path("raw", 
                                                                      'aquatics', forecast_file),
                                                   bucket = "neon4cast-forecasts",
                                                   region = challenge_s3_region,
                                                   base_url = challenge_s3_endpoint))
  if (exists == T & force == F) {
    message(forecast_file, ' already submitted')
  } 
  if (exists == F | (exists == T & force == T)) {
    open_ds <- arrow::open_dataset(forecasts) %>%
      dplyr::filter(site_id %in% NEON_sites, 
                    reference_datetime == flare_dates[i],
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
                    reference_datetime = gsub(' 00:00:00', '', reference_datetime))%>%
      
      dplyr::select(c('datetime', 'reference_datetime', 'site_id', 'family',
                      'parameter', 'variable', 'prediction', 'model_id'))  
    
    # Write the submission
    file_to_submit <- paste0('aquatics-', challenge_submission$reference_datetime[1], '-', challenge_model_name, '.csv.gz')
    
    readr::write_csv(challenge_submission, file_to_submit)
    # Submit forecast!
    
    # Now we can submit the forecast output to the Challenge using 
    # neon4cast::forecast_output_validator(file_to_submit)
    neon4cast::submit(forecast_file = file_to_submit,
                      ask = F, s3_region = 'data', s3_endpoint = 'ecoforecast.org')
    message('submitting missed forecast from: ', file_to_submit)
  }
}


###### NO DATA ASSIMILATION FORECAST #####
flare_model_name <- 'flareGOTM_noDA'
challenge_model_name <- 'flareGOTM_noDA'
force <- FALSE

# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "forecasts/parquet",
                              endpoint_override = "s3.flare-forecast.org",
                              anonymous=TRUE)


this_year <- as.character(paste0(seq.Date(as_date('2023-03-07'), Sys.Date(), by = 'day'), ' 00:00:00'))

# check for missed submissions 
flare_dates  <- arrow::open_dataset(forecasts) |> 
  dplyr::filter(site_id %in% NEON_sites, 
                reference_datetime %in% this_year, 
                model_id == flare_model_name) |> 
  dplyr::distinct(reference_datetime) |>  
  dplyr::pull(as_vector = T) 

challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# are these dates in the challenge?
for (i in 1:length(flare_dates)) {
  
  forecast_file <- paste0('aquatics-', as_date(flare_dates[i]), '-', challenge_model_name, '.csv.gz')
  
  exists <- suppressMessages(aws.s3::object_exists(object = file.path("raw", 
                                                                      'aquatics', forecast_file),
                                                   bucket = "neon4cast-forecasts",
                                                   region = challenge_s3_region,
                                                   base_url = challenge_s3_endpoint))
  if (exists == T & force == F) {
    message(forecast_file, ' already submitted')
  } 
  if (exists == F | (exists == T & force == T)) {
    open_ds <- arrow::open_dataset(forecasts) %>%
      dplyr::filter(site_id %in% NEON_sites, 
                    reference_datetime == flare_dates[i],
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
                    reference_datetime = gsub(' 00:00:00', '', reference_datetime))%>%
      
      dplyr::select(c('datetime', 'reference_datetime', 'site_id', 'family',
                      'parameter', 'variable', 'prediction', 'model_id'))  
    
    # Write the submission
    file_to_submit <- paste0('aquatics-', challenge_submission$reference_datetime[1], '-', challenge_model_name, '.csv.gz')
    
    readr::write_csv(challenge_submission, file_to_submit)
    # Submit forecast!
    
    # Now we can submit the forecast output to the Challenge using 
    # neon4cast::forecast_output_validator(file_to_submit)
    neon4cast::submit(forecast_file = file_to_submit,
                      ask = F, s3_region = 'data', s3_endpoint = 'ecoforecast.org')
    message('submitting missed forecast from: ', file_to_submit)
  }
}
