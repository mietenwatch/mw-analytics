##### CREATE SF OBJECT ####
###########################

create_mw_data_sf <- function(mw_data_3) {
  
  #### 0. Make "sf" object from tibble ####
  
  mw_data_sf <- mw_data_3 %>% 
  #mw_data_sf <- mw_data %>%
    st_as_sf(
      coords = c("geo_lon","geo_lat"),
      agr = "constant",
      crs = 4326,
      na.fail = FALSE, # IMPORTANT! We want to keep offers without coords!
      remove = FALSE)
  
  ####### Add geodata #########
  #############################
  
  # geodata from previously created mw_geodata #
  load("data/geodata/mw_geodata.RData")
  
  #### 1. S-Bahnring ####
  
  # Leftjoin with mw_data
  mw_data_sf <- st_join(mw_data_sf, sf_sbahn_ring) %>% 
    # here even offers without geodata get "ausserhalb"
    mutate(geo_sbahn = fct_explicit_na(geo_sbahn, 
                                         na_level = "ausserhalb")) %>% 
    # we give them "NA" if they have no valid geo_lat
    mutate(geo_sbahn = factor(ifelse(is.na(geo_lat), NA, geo_sbahn))) %>% 
    # clean recode back to levels "innerhalb" & "ausserhalb"
    mutate(geo_sbahn = recode_factor(geo_sbahn,
                                     `1` = "innerhalb",
                                     `2` = "ausserhalb"))

  #### 2. LOR Planungsräume/Bezirksregionen/Prognoseräume/Bezirke ####

  # Leftjoin with mw_data
  mw_data_sf <- st_join(mw_data_sf, sf_LOR) 
  
  #### 3. Ortsteile ####
  
  # Leftjoin with mw_data
  mw_data_sf <- st_join(mw_data_sf, sf_ortsteile) 
  # 
  # ##### geodata from mw_geodata ####
  # load("data/geodata/mw_geodata.RData")
  
  #### 4. Milieuschutzgebiete ####
  
  mw_data_sf <- st_join(mw_data_sf, sf_milieuschutz_baulich)
  mw_data_sf <- st_join(mw_data_sf, sf_milieuschutz_sozial)
  # Is offer inside milleuschutz area? (s = sozial, b = baulich)
  mw_data_sf$pol_milieu_s <- ifelse( is.na(mw_data_sf$pol_milieu_s_name), FALSE, TRUE) 
  mw_data_sf$pol_milieu_b <- ifelse( is.na(mw_data_sf$pol_milieu_b_name), FALSE, TRUE) 
  
  mw_data_sf <- mw_data_sf %>% 
    # we filter out all offers that seem to lie in Brandenburg
    # (They HAVE coords, no Bezirk but lie outside of sbahnring)
    # Its very few cases anyway
    filter(!(geo_sbahn == "ausserhalb" & is.na(geo_bezirk) & !is.na(geo_lat)))
  
  # We dont need the bezirke - we already have them
  mw_data_sf <- mw_data_sf %>%
    dplyr::select(-pol_milieu_b_bezirk,
                  -pol_milieu_s_bezirk)
  
  return(mw_data_sf)
}