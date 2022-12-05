library(tidyverse)
library(arrow)
library(lubridate)

Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

team_name <- 'FLARE'

# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "forecasts/parquet",
                              endpoint_override = "s3.flare-forecast.org",
                              anonymous=TRUE)

today <- paste(Sys.Date(), '00:00:00')
# yesterday <- paste((Sys.Date() - days(1)), '00:00:00')

NEON_sites <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(field_site_subtype == 'Lake') %>%
  distinct(field_site_id) %>%
  pull()

open_ds <- arrow::open_dataset(forecasts) %>%
  filter(site_id %in% NEON_sites, 
         reference_datetime == today,
         forecast == 1,
         depth <= 1) %>% 
  collect() 

challenge_submission <- open_ds %>%
  filter(variable == "temperature",
         datetime >= today) %>%
  # FLARE output at multiple depths
  # Need a single "surface" average
  group_by(site_id, datetime, parameter, reference_datetime,
           family, variable) %>%
  summarise(prediction = mean(prediction)) %>% 
  mutate(model_id = 'FLARE', 
         reference_datetime = gsub(' 00:00:00', '', reference_datetime))%>%
  
  select(c('datetime', 'reference_datetime', 'site_id', 'family', 
                  'parameter', 'variable', 'prediction', 'model_id'))  

# Write the submission
forecast_file <- paste0('aquatics-', challenge_submission$reference_datetime[1], '-', team_name, '.csv.gz')

write_csv(challenge_submission, forecast_file)
# Submit forecast!

# Now we can submit the forecast output to the Challenge using 
neon4cast::forecast_output_validator(forecast_file)
neon4cast::submit(forecast_file = forecast_file,
                  ask = F, s3_region = 'data', s3_endpoint = 'ecoforecast.org')


