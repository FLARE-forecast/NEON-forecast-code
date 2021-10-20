##'
# Source the Functions to download the NEON and NOAA data

lake_directory <- here::here()
setwd(lake_directory)

source(file.path(lake_directory, "R/download_functions/NOAA_downloads.R"))
source(file.path(lake_directory, "R/download_functions/NEON_downloads.R"))

run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_run.yml")))
forecast_site <- run_config$forecast_site
raw_data_directory <- file.path(lake_directory, "data_raw")

buoy_products <- c("DP1.20264.001",
                   "DP1.20252.001",
                   "DP1.20254.001")

neon_database <-  file.path(dirname(lake_directory),"neonstore")

##'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NOAA DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


use_efi_server <- FALSE

if(use_efi_server){

  date <- as.Date(run_config$forecast_start_datetime)
  cycle <- "00"

  noaa_data_location <- file.path(dirname(lake_directory), "drivers","noaa","NOAAGEFS_1hr",forecast_site)

  for(p in 1:length(siteID)){
    for(i in 1:length(date)){
      for(g in 1:length(cycle)){

        if (length(list.files(file.path(noaa_data_location, date[i], cycle[g]))) != 31){
          download_noaa_files_s3(siteID = forecast_site[p],
                                 date = date[i],
                                 cycle = cycle[g],
                                 noaa_directory = noaa_directory,
                                 overwrite = TRUE)

        }
      }
    }
  }
}

##'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NEON DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (file.exists(file.path(neon_database))){
  Sys.setenv("NEONSTORE_DB" = neon_database)
  Sys.setenv("NEONSTORE_HOME" = neon_database)
  neonstore::neon_dir()
} else {
  dir.create(neon_database)
  Sys.setenv("NEONSTORE_HOME" = neon_database)
  Sys.setenv("NEONSTORE_DB" = neon_database)
  neonstore::neon_dir()
}

download_neon_files(siteID = forecast_site,
                    buoy_products = buoy_products,
                    start_date = as.Date("2021-01-01"),
                    raw_data_directory = raw_data_directory)
