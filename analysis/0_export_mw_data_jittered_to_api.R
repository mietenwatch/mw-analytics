#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Datenexport der gejitterten Angebote für Karten
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


library("pacman")

p_load(tidyverse, 
       rgdal, rgeos, sf, mapview, # GIS packages
       data.table)

# loads latest mw_data
load(list.files(path = "data/",
                pattern = "mw_data*" ,
                include.dirs = T,
                full.names = T))

# Load Functions 
sapply(list.files(pattern="[.]R$", 
                  path="functions", 
                  full.names=TRUE), source)
# Um conflicts vorzubeugen
select <- dplyr::select


### Daten jittered für Karten in 1.3, 1.4, 1.5 & 3.x ####

mw_data_jittered <- mw_data %>% 
  create_mw_data_jittered() %>% 
  select(geo_lat, geo_lon,
         geo_ortsteil, geo_ortsteil_id,
         obj_zimmer,
         obj_wohnflaeche,
         is_anbieter_agg,
         cst_nettokalt_sqm,
         cst_nebenkosten,
         cst_gesamtmiete,
         cst_md_gesamtmiete,
         cst_md_nettokalt,
         cst_md_nettokalt_sqm) %>% 
  mutate(obj_zimmer = round(obj_zimmer, 1),
         obj_wohnflaeche = round(obj_wohnflaeche,0),
         cst_nettokalt_sqm = round(cst_nettokalt_sqm, 2),
         cst_nebenkosten = round(cst_nebenkosten,0),
         cst_gesamtmiete = round(cst_gesamtmiete, 0),
         cst_md_gesamtmiete = round(cst_md_gesamtmiete, 0),
         cst_md_nettokalt = round(cst_md_nettokalt, 0),
         cst_md_nettokalt_sqm = round(cst_md_nettokalt_sqm, 2))

### Export csv to frontend repo ####
write_csv(mw_data_jittered,
          "../mw-api/static/mw_data_jittered.csv")