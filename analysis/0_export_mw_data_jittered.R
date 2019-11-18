#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Data export for all maps that need jittered data to api
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### Load packages, functions and data ####

if (!require("pacman")) install.packages("pacman")

library("pacman")
p_load(tidyverse, 
       rgdal, rgeos, sf)

# load functions
sapply(list.files(
  pattern = "[.]R$",
  path = "functions",
  full.names = TRUE), 
  source)

# load latest mw_data
load(
  list.files(
    path = "data",
    pattern = "mw_data*",
    include.dirs = T,
    full.names = T)
)

### Create jittered mw_data for maps in 1.3, 1.4, 1.5 & 3.x ####

mw_data_jittered <- mw_data %>%
  # without gesamtmiete, offer cannnot be displayed
  drop_na(cst_gesamtmiete) %>% 
  create_mw_data_jittered() %>% 
  select(geo_lat, geo_lon,
         geo_ortsteil, geo_ortsteil_id,
         obj_is_gefoerdert,
         obj_zimmer,
         obj_wohnflaeche,
         is_anbieter_agg,
         cst_gesamtmiete,
         cst_gesamtmiete_sqm,
         cst_md_gesamtmiete) %>% 
  mutate(obj_zimmer = round(obj_zimmer, 1),
         obj_wohnflaeche = round(obj_wohnflaeche,0),
         cst_gesamtmiete = round(cst_gesamtmiete, 0),
         cst_gesamtmiete_sqm = round(cst_gesamtmiete_sqm, 2),
         cst_md_gesamtmiete = round(cst_md_gesamtmiete, 0))

### Export ###

write_csv(mw_data_jittered,
          "../mw-api/static/mw_data_jittered.csv")
