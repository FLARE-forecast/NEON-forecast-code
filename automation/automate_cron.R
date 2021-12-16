#remotes::install_github("rqthomas/cronR")
#remotes::install_deps()
library(cronR)

lake_directory <- here::here()

run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr","configure_run.yml"))
config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",run_config$configure_flare))
forecast_site <- config$location$site_id

cmd <- cronR::cron_rscript(rscript = file.path(lake_directory, "automation", "combined_workflow.R"),
                           rscript_log = file.path(lake_directory, "automation", paste0(forecast_site, ".log")),
                           log_append = FALSE,
                           workdir = file.path(lake_directory, "automation"))
#trailing_arg = "curl -fsS -m 10 --retry 5 -o /dev/null https://hc-ping.com/cb249e47-f56b-45da-af7f-9c0c47db1a6c")
cronR::cron_add(command = cmd,  frequency = 'hourly', id = paste0(forecast_site, "_forecast"))
