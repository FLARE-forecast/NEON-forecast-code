library(tidyverse)
setwd(here::here())

# Check the FLARE S3 bucket to see if it is up to date
Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")


NEON_sites <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(field_site_subtype == 'Lake') |> 
  dplyr::distinct(field_site_id)|> 
  dplyr::mutate(field_site_id = ifelse(field_site_id == 'TOOK', 'TOOL', field_site_id)) |> 
  dplyr::pull() 


# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "forecasts/parquet",
                              endpoint_override = "s3.flare-forecast.org",
                              anonymous=TRUE)

today <- paste(Sys.Date(), '00:00:00')

# when was the last FLARE fun
most_recent <- arrow::open_dataset(forecasts) |> 
  dplyr::filter(site_id %in% NEON_sites, 
                model_id %in% c('Simstrat', 'test_runS3')) |> 
  group_by(site_id, model_id) |> 
  summarise(last_forecast = max(reference_datetime)) |> 
  collect()

# check what the most recent date is
for (i in 1: nrow(most_recent)) {
  site <- most_recent$site_id[i]
  model_id <- most_recent$model_id[i]
  recent_forecast <- most_recent$last_forecast[i]
  
  if (recent_forecast != today) {
    assign(paste('rerun', model_id, site, sep = '_'), T)
  } 
}

# write a file for those that are out of date
to_rerun <- ls(pattern = 'rerun_*')

if (!dir.exists('reruns')) {
  dir.create('reruns')
}


for (i in 1:length(to_rerun)) {
  file.create(file.path('reruns',to_rerun[i]))
}