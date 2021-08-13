##'
# Source the Functions to download the NEON and NOAA data
source(file.path(lake_directory, "R/download_functions/NOAA_downloads.R"))
source(file.path(lake_directory, "R/download_functions/NEON_downloads.R"))

##'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NOAA DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd(here::here())

date <- c(as.Date("2021-05-18"),
          as.Date("2021-05-25"),
          as.Date("2021-06-01"),
          as.Date("2021-06-08"),
          as.Date("2021-06-15"),
          as.Date("2021-06-22"),
          as.Date("2021-06-29"),
          as.Date("2021-07-06"),
          as.Date("2021-07-13"),
          as.Date("2021-07-20"),
          as.Date("2021-07-27"),
          as.Date("2021-08-03"))

cycle <- "00"

for(p in 1:length(siteID)){
  for(i in 1:length(date)){
    for(g in 1:length(cycle)){

      if (length(list.files(file.path(noaa_data_location, date[i], cycle[g]))) != 31){
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

download_neon_files(siteID = siteID, buoy_products = buoy_products, start_date = as.Date("2021-01-01"), raw_data_directory = raw_data_directory)

