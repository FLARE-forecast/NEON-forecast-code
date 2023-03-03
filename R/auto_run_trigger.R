

# Check the FLARE S3 bucket to see if it is up to date
Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
Sys.unsetenv("AWS_DEFAULT_REGION")
Sys.unsetenv("AWS_S3_ENDPOINT")
Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")


NEON_sites <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(field_site_subtype == 'Lake') %>%
  dplyr::distinct(field_site_id) %>%
  dplyr::pull()


# get the forecast from the FLARE bucket
forecasts <- arrow::s3_bucket(bucket = "forecasts/parquet",
                              endpoint_override = "s3.flare-forecast.org",
                              anonymous=TRUE)

today <- paste(Sys.Date(), '00:00:00')

# when was the last FLARE fun
most_recent <- arrow::open_dataset(forecasts) |> 
  dplyr::filter(site_id %in% NEON_sites, 
                model_id == 'Simstrat') |> 
  group_by(site_id) |> 
  summarise(last_forecast = max(reference_datetime)) |> 
  collect()

# check what the most recent date is
for (site in most_recent$site_id) {
  recent_forecast <- most_recent$last_forecast[which(most_recent$site_id == site)]
  
  if (recent_forecast != today) {
    assign(paste0('rerun.', site), T)
  } 
}

# write a file for those that are out of date
to_rerun <- ls(pattern = 'rerun.*')

for (i in 1:length(to_rerun)) {
  file.create(to_rerun[i])
}



