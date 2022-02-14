# Function to extract and clean up the NEON data from NEON
download_neon_files <- function(siteID, buoy_products, start_date, raw_data_directory){

        # Download newest products
        neonstore::neon_download(product = buoy_products, site = siteID, start_date = start_date)
}
