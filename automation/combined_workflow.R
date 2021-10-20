library(tidyverse)
lake_directory <- dirname(getwd())
setwd(lake_directory)
update_run_config <<- TRUE #TRUE is used for an iterative workflow
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configuration_run.yml")))
forecast_site <- run_config$forecast_site
configuration_file <<- paste0("configure_flare_",forecast_site,".yml")

source(file.path("automation/check_noaa_present.R"))

noaa_ready <- check_noaa_present(lake_directory)

if(noaa_ready){

  source(file.path("01_downloads.R"))

  setwd(lake_directory)

  source(file.path("02_process_data.R"))

  setwd(lake_directory)

  source(file.path("03_single_forecast_example.R"))

}
