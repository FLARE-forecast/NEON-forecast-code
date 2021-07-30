##' Load in the required functions for processing the data
source(file.path(lake_directory, "R/process_functions/met_qaqc2.R"))
source(file.path(lake_directory, "R/process_functions/buoy_qaqc.R"))
source(file.path(lake_directory, "R/process_functions/glmtools.R"))

##' Set up configurations for the data processing
lake_directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "data_processed","NOAA_data","noaa",config$met$forecast_met_model)
config$run_config <- run_config

##' Download the latest "early release" data from Bobby Hensley at NEON
prop_neon <- read.csv("https://raw.githubusercontent.com/FLARE-forecast/NEON-proprietary-data/master/surface_sonde_NEON_raw.csv")

##' Process the NEON data for the site selected in the original .yml file
buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
          prop_neon = prop_neon,
          input_file_tz = "UTC",
          local_tzone = "UTC",
          forecast_site = forecast_site)

##' Update the GLM3r configuration files with the newest Kw values based off of all previous secchi data at the site.
# In this case, we are simply assuming kw = 1.7/secchi
# read example configuration into memory
Kw <- neonstore::neon_read(table = "dep_secchi-basic", site = siteID) %>%
  select(secchiMeanDepth, siteID) %>%
  group_by(siteID)%>%
  mutate(kw = 1.7/secchiMeanDepth)%>%
  summarise(kw = mean(kw, na.rm = T))
kw_site <- Kw %>% filter(siteID == forecast_site)
nml_file = file.path(lake_directory, "configuration", "forecast_model", "glm", paste0("glm3_",forecast_site,".nml"))
nml <- read_nml(nml_file)
get_nml_value(nml, 'Kw')
new_nml <- set_nml(nml, 'Kw', kw_site$kw)
get_nml_value(new_nml, 'Kw')
write_nml(new_nml, file = nml_file)


##' get NOAA met forecasts and stack first day to use as met 'obs'
dates <- seq.Date(as.Date('2021-04-13'), as.Date(config$run_config$forecast_start_datetime), by = 'day') # cycle through historical dates
cycle <- c('00','06','12','18')
outfile <- config$file_path$qaqc_data_directory

stack_noaa_forecasts(dates = dates,
                     outfile = outfile,
                     config = config,
                     model_name = paste0("observed-met_",config$location$site_id),
                     hist_file = file.path(paste0(lake_directory,"/data_processed/","observed-met_",forecast_site,".nc")),
                     noaa_directory = noaa_directory)
