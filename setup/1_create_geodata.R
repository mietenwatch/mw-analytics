########################################
# SETUP 
# STEP 1: Create all nescessary geodata
#
########################################

# Here we create all geodata nescessary for later analysis and matching 
# with offers.

# Pacman installs package if previously uninstalled. 
# Once package installed, pacman::p_load loads it.

#### Load libraries ####

if (!require("pacman")) install.packages("pacman")

library("pacman")
p_load(
  "osmdata",
  "rgdal",
  "sf",
  "tidyverse",
  "mapview"
  )

#### S-Bahnring ####

# Get S-Bahnring from OSM

# This is the  OSM relation ID of Berliner S-Bahnring
id <- 6636485
sbahn <- opq_osm_id(type = "relation",
                    id = id) %>%
  opq_string () %>% 
  osmdata_sf() 

#plot(sbahn$osm_lines)  
#plot(sbahn$osm_multilines)  

sbahn_line1 <- sbahn$osm_multilines %>% 
  dplyr::select(osm_id) %>% 
  slice(1)

sbahn_line2 <- sbahn$osm_multilines %>% 
  dplyr::select(osm_id) %>% 
  slice(-1)

# have to tie both lines together
sbahn_ring <- st_union(sbahn_line1,
                       sbahn_line2) 

sf_sbahn_ring <- sbahn_ring %>% 
  dplyr::select(-osm_id.1) %>%  # osm_id.1 is void
  rename(geo_sbahn = osm_id) %>% # later it will be called geo_sbahn
  # we prepare the factor - will be later called "innerhalb"
  mutate(geo_sbahn = fct_recode(factor(geo_sbahn),
                                "innerhalb" = "6636485")) 

# Make polygon of s-bahnring
sf_sbahn_ring <- sf_sbahn_ring %>%
  st_transform(crs = 4326) %>% 
  st_polygonize() 

# mapview(sbahn_ring)

#### LOR Planungsräume/Bezirksregionen/Prognoseräume ####

sf_LOR <- readOGR("data/geodata/lor_planungsraeume.shp/lor_planungsraeume.shp",
  verbose = F
  ) %>%
  st_as_sf() %>%
  rename(
    geo_lor_planungsraum_id = "spatial_na",
    geo_lor_planungsraum = "PLRNAME",
    geo_lor_bezirksregion = "BZRNAME",
    geo_lor_prognoseraum = "PGRNAME",
    geo_bezirk = "BEZNAME"
  ) %>%
  dplyr::select(starts_with("geo")) %>%
  st_transform(4326)

#### Ortsteile

sf_ortsteile <- readOGR("data/geodata/ortsteile_statistikamt_bb_1412/RBS_OD_ORT_1412.shp",
  verbose = F
  ) %>%
  st_as_sf() %>%
  rename(
    geo_ortsteil = "Ortsteilna",
    geo_ortsteil_id = "ORT"
  ) %>%
  dplyr::select(starts_with("geo")) %>%
  st_transform(4326)

#### Soziale Erhaltungsgebiete = Mileuschutz
# Erhaltungsverordnungsgebiete gem. § 172 Abs. 1 Nr. 1 BauGB (ES)

sp_milieuschutz_sozial <- readOGR(
  "WFS:https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_erhaltgeb_es",
  "fis:s_erhaltgeb_es"
  )

sf_milieuschutz_sozial <- sp_milieuschutz_sozial %>%
  st_as_sf() %>%
  dplyr::select(-gml_id, -starts_with("AE")) %>%
  rename(
    pol_milieu_s_name = "GEBIETSNAME",
    pol_milieu_s_bezirk = "BEZIRK",
    pol_milieu_s_GVBL_datum = "F_GVBL_DAT",
    pol_milieu_s_inkraft_datum = "F_IN_KRAFT",
    pol_milieu_s_flaeche = "FL_IN_HA"
  ) %>%
  mutate(
    pol_milieu_s_GVBL_datum = as.Date(
      as.character(pol_milieu_s_GVBL_datum),
      "%d.%m.%Y"
    ),
    pol_milieu_s_inkraft_datum = as.Date(
      as.character(pol_milieu_s_inkraft_datum),
      "%d.%m.%Y"
    )
  ) %>%
  st_transform(4326)

#### Städtebauliche Erhaltungsgebiete = Milieuschutz
# Erhaltungsverordnungsgebiete gem. § 172 Abs. 1 Nr. 2 BauGB (EM)

sp_milieuschutz_baulich <- readOGR(
  "WFS:https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_erhaltgeb_em",
  "fis:s_erhaltgeb_em"
  )

sf_milieuschutz_baulich <- sp_milieuschutz_baulich %>%
  st_as_sf() %>%
  dplyr::select(-gml_id, -PDF_LINK, -starts_with("AE")) %>%
  rename(
    pol_milieu_b_name = "GEBIETSNAME",
    pol_milieu_b_bezirk = "BEZIRK",
    pol_milieu_b_GVBL_datum = "F_GVBL_DAT",
    pol_milieu_b_inkraft_datum = "F_IN_KRAFT",
    pol_milieu_b_flaeche = "FL_HA"
  ) %>%
  mutate(
    pol_milieu_b_GVBL_datum = as.Date(
      as.character(pol_milieu_b_GVBL_datum),
      "%d.%m.%Y"),
    pol_milieu_b_inkraft_datum = as.Date(
      as.character(pol_milieu_b_inkraft_datum),
      "%d.%m.%Y")
  ) %>%
  st_transform(4326)


#### Save all sf objects in one .RData file

save(
  sf_sbahn_ring,
  sf_LOR,
  sf_ortsteile,
  sf_milieuschutz_sozial,
  sf_milieuschutz_baulich,
  file = "data/geodata/mw_geodata.RData"
  )
