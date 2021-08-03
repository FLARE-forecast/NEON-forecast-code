##'
# Source the Functions to download the NEON and NOAA data
source(file.path(lake_directory, "R/download_functions/NOAA_downloads.R"))
source(file.path(lake_directory, "R/download_functions/NEON_downloads.R"))

##'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NOAA DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


date <- seq(from = as.Date("2021-04-13"), to = as.Date("2021-06-01"), by = "days")
cycle <- c("00","06","12","18")

if (!file.exists(file.path(noaa_data_location))){

  for(p in 1:length(siteID)){
    for(i in 1:length(date)){
      for(g in 1:length(cycle)){
        download_noaa_files_s3(siteID = siteID[p],
                              date = date[i],
                              cycle = cycle[g],
                              noaa_directory = noaa_directory,
                              overwrite = TRUE)
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

download_neon_files(siteID = siteID, buoy_products = buoy_products, start_date = as.Date("2021-01-01"))
