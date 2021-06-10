#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NOAA DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(file.path(lake_directory, "R/download_functions/NOAA_downloads.R"))

date = seq(from = Sys.Date()-30, to = Sys.Date()-1, by = "days")
cycle = c("00")

for(p in 1:length(siteID)){
    for(i in 1:length(date)){
      for(g in 1:length(cycle)){
        download_noaa_files_s3(siteID = siteID[p],
                              date = date[i],
                              cycle = cycle[g],
                              noaa_directory <- noaa_directory)
    }
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOANLOAD THE NEWEST NEON DATA ###
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(file.path(lake_directory, "R/download_functions/NEON_downloads.R"))

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

download_neon_files(siteID_neon = siteID_neon,
                    siteID = siteID,
                    ECtower = ECtower,
                    products = products,
                    buoy_products = buoy_products)
