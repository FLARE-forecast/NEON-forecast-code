source(file.path(lake_directory, "R/process_functions/met_qaqc_BARC.R"))
source(file.path(lake_directory, "R/process_functions/met_qaqc_SUGG.R"))
source(file.path(lake_directory, "R/process_functions/met_qaqc_LIRO.R"))
source(file.path(lake_directory, "R/process_functions/met_qaqc_CRAM.R"))
source(file.path(lake_directory, "R/process_functions/met_qaqc_PRPO.R"))
source(file.path(lake_directory, "R/process_functions/met_qaqc_PRLA.R"))

met_qaqc_barc(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              cleaned_met_file_dir = file.path(lake_directory,"data_processed/"),
              input_file_tz = "UTC",
              local_tzone = "UTC")

met_qaqc_sugg(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              cleaned_met_file_dir = file.path(lake_directory,"data_processed/"),
              input_file_tz = "UTC",
              local_tzone = "UTC")

met_qaqc_liro(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              cleaned_met_file_dir = file.path(lake_directory,"data_processed/"),
              input_file_tz = "UTC",
              local_tzone = "UTC")

met_qaqc_cram(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              cleaned_met_file_dir = file.path(lake_directory,"data_processed/"),
              input_file_tz = "UTC",
              local_tzone = "UTC")

met_qaqc_prpo(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              cleaned_met_file_dir = file.path(lake_directory,"data_processed/"),
              input_file_tz = "UTC",
              local_tzone = "UTC")

met_qaqc_prla(realtime_file = file.path(lake_directory,"data_raw","raw_neon_met_data.csv"),
              cleaned_met_file_dir = file.path(lake_directory,"data_processed/"),
              input_file_tz = "UTC",
              local_tzone = "UTC")
