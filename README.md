# NEON lake forecasts in R using FLAREr (Forecasting Lake And Reservoir Ecosystems)

<a href="url"><img src = "FLARE.jpg" align="top" height="200" width="200" ></a>
<a href="url"><img src = "nsf_neon_logo.png" align="top" height="200" width="560" ></a>

-----


:busts_in_silhouette: Ryan McClure, Quinn Thomas, Tadhg Moore, Cayelan Carey, Renato Figueiredo, Whitney Woelmer, Heather Wander, Vahid Daneshmand    

Questions?  :email: ryan333@vt.edu, rqthomas@vt.edu, cayelan@vt.edu, or tadhgm@vt.edu

-----

## Motivation

Thank you for checking out NEON-forecast-code. Freshwater lakes globally are increasingly threatened as a result of rapidly changing land use and climate (Carpenter et al., 2011). In response, developing forecast workflows has has emerged as a powerful tool to predict future environmental conditions in lakes in order to make informed management decisions for safety, health, and conservation (Carey et al., 2021; Baracchini et al., 2020; Page et al., 2018). However, the discipline of forecasting in lakes is still in the early stages of making forecasts that are robust and reproducible. As a result, there is a dire need for open-source forecast workflows that are broadly applicable to many different lake ecosystems and flexible to different datastreams and local needs.

Here, we applied the FLAREr forecasting system (Thomas et al., 2020) to six NEON lakes to test FLAREr's robustness and scalability to other sites. The NEON lakes serve as an exemplar case to test FLARE because they have reliable, open-source datastreams in which new data can be acquired at relatively low latencies (<1.5 months). The goal of our forecast scaling study was to show that FLAREr is scalable to other lake ecosystems and can produce robust forecasts of water temperatures up to 35-days into the future. Altogether, we hope this workflow is a first step to building a community of lake and reservoir forecast practitioners that develop reliable forecast workflows and make informed decisions for future lake conservation and management.

## Prerequisites

FLAREr has been tested across Windows, Mac, and Linux OS. It also requires R version 4.0.x or higher.

### Word of caution

Some packages will need manual compilation <b>if you have a Mac OS with the new Apple silicon arm64</b> (M1 chip) and have recently updated to R 4.1.0-arm64. The specific package is udunits. Homebrew is not yet (to our knowledge) bottled for udunits and Apple silicon so go here to download the version of udunits for 4.1.0-arm64: https://mac.r-project.org/libs-arm64/udunits-2.2.28-darwin.20-arm64.tar.gz. 



## Cloning NEON-forecast-code onto your computer (5 steps)
1. Go to the [NEON-forecast-code](https://github.com/FLARE-forecast/NEON-forecast-code) repository and copy the repo URL. 
2. Open R
3. Start a new project: File > New Project
4. Select: Version Control > Git
5. Paste the repo's URL into "Repository URL:", keep the project directory name as the default, select "open in new session", and click <b>New Project</b>



## Forecast Site Setup 
1. When you have cloned the project into R, Open the following R scripts in main project directory: 
   <i>00_setup.R</i>, <i>01_downloads.R</i>, <i>02_process_data.R</i>, and <i>03_single_forecast_example.R</i>.
2. Navigate to <i>00_setup.R</i> and <b>read the directions at the top!</b>
3. Open the <i>run_configuration.yml</i> file located in "NEON-forecast-code/configuration/FLAREr/". It should look like this:
``` yml
restart_file: .na
start_datetime: 2021-04-13 00:00:00
end_datetime: .na
forecast_start_datetime: 2021-05-30 00:00:00
forecast_horizon: 35
sim_name: BARC_LAKE #SUGG_LAKE CRAM_LAKE LIRO_LAKE PRLA_LAKE PRPO_LAKE
forecast_site: BARC #SUGG CRAM LIRO PRLA PRPO
forecast_output_directory: .na
forecast_configuration_directory: .na
execute_directory: .na
```
4. You can edit the sim_name: and forecast_site: lines in the <i>run_configuration.yml</i> file to chose what NEON lake you wish to forecast. 
5. Save the <i>run_configuration.yml</i> when sim_name and forecast_site are specified and then navigate to <i>00_setup.R</i> script. 
  
  
  
## Run Setup Script
1. You can either source the <i>00_setup.R</i> script or run through it incrementally. 

### This script is setting up a few more configuarions to execute a forecast. This includes:
Getting default packages
``` r
##' Download the packages required to process data and run FLAREr
if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, lubridate, naniar, Amelia, dplyr,
               mice, FactoMineR, broom, aws.s3, scattermore,
               reshape2, duckdb, RCurl, here)
```

Getting specific packages from [Github](https://github.com/)
``` r
##' Manually download packages from Github
remotes::install_github("cboettig/neonstore", force = F)
remotes::install_github("eco4cast/EFIstandards", force = F)
remotes::install_github("rqthomas/noaaGEFSpoint", force = F)
remotes::install_github("FLARE-forecast/GLM3r", force = F)
remotes::install_github("FLARE-forecast/FLAREr", force = F)
```

Specify the site you wish to download data for and forecast
``` r
##' Set up the sites for downloading
run_config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "run_configuration.yml")))
forecast_site <- run_config$forecast_site
siteID = forecast_site
```
This block sets directories for the forecast workflow
``` r
##' Set up the directories and databases for processing files
lake_directory <- getwd()
noaa_directory <- file.path(getwd(), "data_processed", "NOAA_data")
neon_database <- file.path("/Volumes/Seagate Backup Plus Drive/neonstore")
forecast_location <- file.path(getwd(), "flare_tempdir")
```

### WORD OF CAUTION
``` r
neon_database <- file.path("/Volumes/Seagate Backup Plus Drive/neonstore")
```
This directory will be used when you reach the <i>01_data_download.R</i> script and can take up a large amount of space on your computer. We suggest storing the data in an external storage space if possible. Here, this exmaple has the NEON data stored in an external hard drive "/Volumes/Seagate Backup Plus Drive/neonstore". 

This block specifies the NEON data products to download
``` r
##' Specify the NEON data products to download
buoy_products = c("DP1.20264.001",   #Buoy
                  "DP1.20252.001",   #Sonde Profiles
                  "DP1.20254.001")   #Secchi
```
2. When script has finished running click on the <i>01_downloads.R</i> script. 



## Run Downloading Script
1. You can either source the <i>01_downloads.R</i> script or run through it incrementally. 

### This script includes two componenets that calls separate functions

One function downloads the NOAA Global Ensemble Forecasting Systems (NOAA GEFS) forecasts
``` r
source(file.path(lake_directory, "R/download_functions/NOAA_downloads.R"))
```
These forecasts are being downloaded from a s3 bucket that is part of the [Ecological Forecasting](https://ecoforecast.org) challenge ([EFI-RCN](https://projects.ecoforecast.org/neon4cast-docs/)) to develop forecasts of different ecological variables across numerous NEON sites. The raw forecasts can be found [HERE](https://data.ecoforecast.org/minio/drivers/noaa/).

Another function downloads the NEON observations of temperature and secchi depth from the lake specified. 
``` r
source(file.path(lake_directory, "R/download_functions/NEON_downloads.R"))
```
This function leverages the novel [neonstore](https://cran.r-project.org/web/packages/neonstore/index.html) package. 'neanstore' will avoid repeated downloading, provides persistent storage, and improves performance. It can also construct a local 'duckdb' database of stacked tables, making it possible
to work with tables that are far to big to fit into memory. Refer to this [Github repo](https://github.com/cboettig/neonstore) for more detail on the package and how to use it for other NEON data products in R. 

This code block downloads the NOAA GEFS data.
``` r
date = seq(from = as.Date("2021-04-13"), to = as.Date("2021-06-01"), by = "days")
cycle = c("00","06","12","18")

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
```
This code block downloads the NEON data.
``` r
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
```

### WORD OF CAUTION
<a href="url"><img src = "DWNLD.jpg" align="top" height="100" width="150" ></a>

Just be mindful that downloading the NOAA GEFS and NEON data might take a few minutes. Maybe go grab a tea or coffee?

2. When script has finished running click on the <i>02_process_data.R</i> script.



## Run Data Processing Script
1. If everything has been properly aligned, you should also be able to click source and the script will run through. 

### This script includes componenets that calls three separate functions

One function processes the NOAA GEFS forecasts so the forecast from the first day becomes the meteorological driver data for the forecast model spinup. This is currently for simplicity in executing this example. However, we are working on a detailed workflow that downloads NEON meterological data form each lake site and the nearby eddy flux covariance towers as the meterological drivers of the model. Here, this process is excluded because downloading hourly met NEON data across multiple sites takes multiple hours and the processing/QAQC of the data is still in production. 

The second function processes the NEON temperature and secchi data such that it can be implemented in the ENKF that is used by FLARE. 

``` r
source(file.path(lake_directory, "R/process_functions/met_qaqc2.R"))
source(file.path(lake_directory, "R/process_functions/buoy_qaqc.R"))
```

The next block of code is used to configure the processing of the data. 
``` r
##' Set up configurations for the data processing
lake_directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "data_processed","NOAA_data","noaa",config$met$forecast_met_model)
config$run_config <- run_config
```

The next block of code is accessing prereleased NEON data that is stored in a seperate [data repository](https://github.com/FLARE-forecast/NEON-proprietary-data).
``` r
##' Download the latest "early release" data from Bobby Hensley at NEON
x <- getURL("https://raw.githubusercontent.com/FLARE-forecast/NEON-proprietary-data/master/surface_sonde_NEON_raw.csv")
prop_neon <- read.csv(text = x)
```

This block will append and process the NEON data from 'neonstore' and prereleased NEON data so it can be accessed by FLAREr's ENKF.
``` r
##' Process the NEON data for the site selected in the original .yml file
buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
          realtime_kw_file = file.path(lake_directory, "data_raw", paste0("Kw_",forecast_site,".csv")),
          prop_neon = prop_neon,
          input_file_tz = "UTC",
          local_tzone = "UTC",
          forecast_site = forecast_site)
```

The next block processes the NOAA forecasts so the first day of the forecast can be used as meterological driver data. 
``` r
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
```
### It will look like this:

<a href="url"><img src = "NOAA_stack.gif" align="top" height="100" width="400" ></a>

2. When this script has finished running click on the <i>03_single_forecast_example.R</i> script.

## Run Forecasting Script

1. If everything has been properly aligned, you should also be able to click source and the script will run through. 

### This script includes the bulk for FLAREr. Everything else so far has been just staging to prep for the foreacsts. The coding blocks are as follows: 

This block sets up the configuations from the forecast. This includes the main configuration file for the site being forecasted. 
``` r
##' Set up configurations for the forecasts
lake_directory <- here::here()
config <- yaml::read_yaml(file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "configure_flare_",forecast_site,".yml")))
config$file_path$qaqc_data_directory <- file.path(lake_directory, "data_processed")
config$file_path$data_directory <- file.path(lake_directory, "data_raw")
config$file_path$noaa_directory <- file.path(lake_directory, "data_processed","NOAA_data","noaa",config$met$forecast_met_model)
config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$run_config <- file.path(paste0(lake_directory,"/configuration/", "FLAREr/", "run_configuration_",forecast_site,".yml"))
config$file_path$forecast_output_directory <- file.path(lake_directory, "forecast_output")
config$run_config <- run_config
```

This block creates a directory for the forecast output and a place to temporarily run all the forecasts
``` r
##' Create directories if not present
if(!dir.exists(config$file_path$execute_directory)) {
  dir.create(config$file_path$execute_directory)
}
if(!dir.exists(config$file_path$forecast_output_directory)) {
  dir.create(config$file_path$forecast_output_directory)
}
```

This block of code configures the meteorological driver data as seperate met forecast ensembles that are used by flare. The current setup has the same met driver data from "2021-04-13" to "2021-05-30" and then hands off to the 35-day NOAA GEFS forecast on "2021-05-31".

``` r
##' Configure the NOAA met data from data processing
observed_met_file <- file.path(paste0(config$file_path$qaqc_data_directory, "/observed-met_",forecast_site,".nc"))
start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
if(is.na(config$run_config$forecast_start_datetime)){
  end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
  forecast_start_datetime <- end_datetime
}else{
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_datetime <- forecast_start_datetime + lubridate::days(config$forecast_horizon)
}
forecast_hour <- lubridate::hour(forecast_start_datetime)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
noaa_forecast_path <- file.path(config$file_path$noaa_directory, config$location$site_id,
                                lubridate::as_date(forecast_start_datetime), "00")

##' Convert NOAA forecasts to GLM format
met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                          out_dir = config$file_path$execute_directory,
                                          forecast_dir = noaa_forecast_path,
                                          config = config)
historical_met_error <- met_out$historical_met_error
```
This next block creates a matrix of the temperature observations across space and time that is used in the ENKF. 

``` r
##' Create observation matrix
cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,paste0("observations_postQAQC_long_",forecast_site,".csv"))
obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$obs_config_file), col_types = readr::cols())
obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                 obs_config,
                                 config)
states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$states_config_file), col_types = readr::cols())
states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
model_sd <- FLAREr::initiate_model_error(config = config, states_config = states_config)
```

This next block generates the Initial Conditions
``` r
##' Generate initial conditions
pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, "FLAREr", config$model_settings$par_config_file), col_types = readr::cols())
init <- FLAREr::generate_initial_conditions(states_config,
                                            obs_config,
                                            pars_config,
                                            obs,
                                            config,
                                            restart_file = config$run_config$restart_file,
                                            historical_met_error = met_out$historical_met_error)
```

Aaaaannd finally... The block that run the forecasts is as follows:
``` r
print("Starting Data Assimilation and Forecasting")
print("-----------------------------------")
da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                              pars_init = init$pars,
                                              aux_states_init = init$aux_states_init,
                                              obs = obs,
                                              obs_sd = obs_config$obs_sd,
                                              model_sd = model_sd,
                                              working_directory = config$file_path$execute_directory,
                                              met_file_names = met_out$filenames[2:31],
                                              config = config,
                                              pars_config = pars_config,
                                              states_config = states_config,
                                              obs_config = obs_config,
                                              da_method = config$da_setup$da_method,
                                              par_fit_method = config$da_setup$par_fit_method)




print("Writing output file")
saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                            forecast_output_directory = file.path(config$file_path$forecast_output_directory,forecast_site))

print("Creating metadata")
FLAREr::create_flare_metadata(file_name = saved_file,
                              da_forecast_output = da_forecast_output)

rm(da_forecast_output)
gc()
print("Generating plot")
print("-----------------------------------")
plotting_general_2(file_name = saved_file,
                   qaqc_data_directory = config$file_path$qaqc_data_directory, ncore = 5)

print(paste0("Metadata and plots generated! Go to NEON-forecast/forecast_output/",forecast_site," folder to view"))
```
