#renv::restore()

library(tidyverse)
library(lubridate)
lake_directory <- here::here()
setwd(lake_directory)
configure_run_file <- "configure_run.yml"

update_run_config <- TRUE

noaa_ready <- FLAREr::check_noaa_present(lake_directory, configure_run_file)

if(noaa_ready){

  source(file.path("01_generate_targets.R"))

  setwd(lake_directory)

  source(file.path("02_run_inflow_forecast.R"))

  setwd(lake_directory)

  source(file.path("03_run_flarer_forecast.R"))

  setwd(lake_directory)

  source(file.path("04_visualize.R"))
}
