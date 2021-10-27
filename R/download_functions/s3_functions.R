download_s3_objects <- function(lake_directory, bucket, prefix){

  files <- aws.s3::get_bucket(bucket = bucket, prefix = prefix)
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],bucket = bucket, file = file.path(lake_directory, bucket, keys[i]))
    }
  }
}
