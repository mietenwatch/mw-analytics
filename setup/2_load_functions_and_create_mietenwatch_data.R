#################################################
# SETUP
# STEP 2: Load libraries, functions and data
#
#################################################

# Pacman installs package if previously uninstalled. 
# Once package installed, pacman::p_load loads it.


#### Load packages ####
if (!require("pacman")) install.packages("pacman")

library("pacman")
p_load(
  tidyverse,
  tidyselect,
  readxl,
  odbc,
  RMySQL,
  R.utils,
  hrbrthemes,
  enc,
  lubridate,
  RecordLinkage,
  sf,
  rgdal,
  readxl,
  data.table,
  rmarkdown)

# empty local environment to have more RAM free
remove(list = ls())

#### Load functions ####

# load all functions needed for data wrangling & analysis
sapply(list.files(
  pattern = "[.]R$",
  path = "functions",
  full.names = TRUE), 
  source)

#### Create new data from raw data ####

# We better make sure MASS does not interfere with dplyr
select <- dplyr::select

# Create mw_data includes:
# Download of SQL View, filtering, data wrangling, 
# creation/mutation of existing and new variables 
# and cleaning

create_mw_data()

# All data will be saved in /data/mw_data_<date>.RData
