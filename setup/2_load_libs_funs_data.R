###################################
# Load Libraries, Functions, Data
###################################

##### Libs ####

#### Load libraries ####

if (!require("pacman")) install.packages("pacman")

library("pacman")
p_load(tidyverse, 
       tidyselect,
       readxl,
       odbc, 
       RMySQL,
       R.utils,
       enc,
       lubridate,
       RecordLinkage,
       sf,
       rgdal,
       readxl,
       data.table,
       rmarkdown)

# Functionskonflikten mit select Befehl vorbeugen
# (es gibt manchmal wohl clashes)
select <- dplyr::select

# Empty local environment to have more RAM free
remove(list = ls())

#### Functions ####

sapply(list.files(pattern="[.]R$", 
                  path="functions", 
                  full.names=TRUE), source)

#### Data ####

# Download fresh data from Database & prepare it for R?
refresh_mw_data() 

# Empty local environment to have more RAM free
remove(list = ls())

# loads latest mw_data
load(list.files(path = "./data", 
                pattern = "mw_data*" ,
                include.dirs = T, 
                full.names = T))
