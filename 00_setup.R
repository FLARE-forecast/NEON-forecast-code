#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*
#~*#~*#~*#~*#~*#~* PLEASE READ THIS #~*#~*#~*#~*#~*#~*#~*#~*#~*
#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*

# Before running through and hitting source for each script. Please refer to the
# run_configuration.yml file located in "NEON-forecast-code/configuration/FLAREr/...

# You can open the file directly in R by clicking the file in your panel. When opened,
# two lines of the YAML need to be adapted to meet your needs. This includes...

# sim_name: BARC_LAKE #SUGG_LAKE CRAM_LAKE LIRO_LAKE PRLA_LAKE PRPO_LAKE

# and

# forecast_site: BARC #SUGG CRAM LIRO PRLA PRPO

# As a default, it will run BARC_LAKE and BARC. However, if you wish to run another
# site just move that site in place of BARC_LAKE and BARC and make sure both names are not
# hashed out. Make sure NOT to mix up sites (e.g., no SUGG_LAKE & CRAM) and DO NOT
# change these name conventions. Doing so will propagate errors throughout this whole workflow.

#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*
#~*#~*#~*#~*#~*#~* THANKS FOR READING #~*#~*#~*#~*#~*#~*#~*#~*#
#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*#~*

##'
# Download the packages required to process data and run FLAREr

if (!require('pacman')) install.packages('pacman'); library('pacman')
pacman::p_load(tidyverse, naniar,mice, FactoMineR, aws.s3, scattermore,
               reshape2, duckdb, RCurl, here)
##'
# Manually download packages from Github

remotes::install_github("cboettig/neonstore", force = F)
remotes::install_github("eco4cast/EFIstandards", force = F)
remotes::install_github("rqthomas/noaaGEFSpoint", force = F)
remotes::install_github("FLARE-forecast/GLM3r", force = F)
remotes::install_github("FLARE-forecast/FLAREr", force = F)


