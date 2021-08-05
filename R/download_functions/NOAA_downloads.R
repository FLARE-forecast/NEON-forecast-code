# Function to pull the NOAA files from MinIO
download_noaa_files_s3 <- function(siteID, date, cycle, noaa_directory, overwrite = T){

  Sys.setenv("AWS_DEFAULT_REGION" = "data",
             "AWS_S3_ENDPOINT" = "ecoforecast.org")

  object_1hr <- aws.s3::get_bucket("drivers", prefix=paste0("noaa/NOAAGEFS_1hr/",
                                                            siteID,"/",date,"/",cycle))
  object_1hr_stacked <- aws.s3::get_bucket("drivers", prefix=paste0("noaa/NOAAGEFS_1hr_stacked/",
                                                            siteID))

  for(i in 1:length(object_1hr)){
    aws.s3::save_object(object_1hr[[i]], bucket = "drivers",
                        file = file.path(noaa_directory, object_1hr[[i]]$Key))
  }
  for(i in 1:length(object_1hr_stacked)){
    aws.s3::save_object(object_1hr_stacked[[i]], bucket = "drivers",
                        file = file.path(noaa_directory, object_1hr_stacked[[i]]$Key),
                        overwrite = overwrite)
  }
}
