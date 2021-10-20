library(tidyverse)
lake_directory <- here::here()
setwd(lake_directory)
update_run_config <<- TRUE #TRUE is used for an iterative workflow
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_run.yml")))
forecast_site <- run_config$forecast_site
configuration_file <<- paste0("configure_flare_",forecast_site,".yml")

noaa_directory <- file.path(dirname(lake_directory), "drivers", "noaa")

source(file.path("automation/check_noaa_present.R"))

noaa_ready <- check_noaa_present(lake_directory, noaa_directory, configuration_file)

if(noaa_ready){

  source(file.path("01_get_data.R"))

  setwd(lake_directory)

  source(file.path("02_process_data.R"))

  setwd(lake_directory)

  source(file.path("04_run_flarer_forecast.R"))

}
