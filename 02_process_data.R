source(file.path(lake_directory, "R/process_functions/met_qaqc.R"))
source(file.path(lake_directory, "R/process_functions/buoy_qaqc.R"))


met_qaqc(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              input_file_tz = "UTC",
              local_tzone = "UTC",
              forecast_site = forecast_site)

buoy_qaqc(realtime_buoy_file = file.path(lake_directory,"data_raw","raw_neon_temp_data.csv"),
         input_file_tz = "UTC",
         local_tzone = "UTC",
         forecast_site = forecast_site)
