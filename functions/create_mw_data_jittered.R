#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Create jittered data 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

create_mw_data_jittered <- function(mw_data,
                                    mw_jitterfactor = .00035) {
  
  library("tidyverse")
  library("sf")
  #library("mapview")
  
  source("functions/sfc_as_cols.R")
  
  # We use sf::st_jitter for jittering data (privacy reasons)
  
  ### A.) mw_data WITHOUT offers that have missing coords ####
  mw_data_clean <- mw_data %>% 
    drop_na(geo_lat) 
  
  ### B.) Create jittered data ####
  mw_data_sf_clean_j <- mw_data_clean %>% 
    # We want to keep only DISTINCT coords, so that jitter is created for same coords:
    # first keep all otherwise same coords get kicked out too early
    distinct(geo_lat, geo_lon, .keep_all=TRUE) %>% 
    # we keep geo_lat, geo_lon
    dplyr::select(geo_lat, geo_lon) %>% 
    # convert to sf Object
    st_as_sf(
      coords = c("geo_lon","geo_lat"),
      agr = "constant",
      crs = 4326,
      remove = FALSE) %>% 
    # Finally jitter:  jitterfactor defined above!
    st_jitter(factor = mw_jitterfactor)
  
  
  ### Join A.) mit B.) -> Leftjoin damit rows erhalten bleiben! ####
  mw_data_sf_clean_j_full <- left_join(mw_data_clean,
                                       mw_data_sf_clean_j) %>%
    st_as_sf(sf_column_name = "geometry")
  
  ### Check ####
  
  # Map comparing jittered and non jittered data:
  #  mapview(st_geometry(mw_data_sf), color = "blue") + 
  #  mapview(st_geometry(mw_data_sf_clean_j_full), color="red")
  
  # Look at data:
  # mw_data_sf_clean_j_full %>% 
  #   sfc_as_cols(names = c("geo_jittered_lon",
  #                         "geo_jittered_lat")) %>%  
  #   select(ends_with("lat"), ends_with("lon")) %>%
  #   slice(1000:5000) %>% 
  #   View()
  
  ### Finalize dataset with renaming cols ####
  mw_data_jittered <- mw_data_sf_clean_j_full %>% 
    dplyr::select(-geo_lat, -geo_lon) %>% 
    sfc_as_cols(names = c("geo_lon",
                          "geo_lat"))
  
  # Remove SF from mw_data_jittered - make it a normal tibble
  mw_data_jittered <- st_set_geometry(mw_data_jittered, NULL) 
  
  return(mw_data_jittered)
  
}
