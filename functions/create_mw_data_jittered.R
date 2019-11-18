#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Create jittered data 
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

create_mw_data_jittered <- function(mw_data, mw_jitterfactor = .00035) {
  
  library("tidyverse")
  library("sf")
  #library("mapview")
  
  source("functions/sfc_as_cols.R")
  
  # Generell benutzen wir den Befehl st_jitter zum jittern:
  
  # plot(st_geometry(mw_data_sf), 
  #      cex = 0.01,
  #      pch = 0.01)
  # 
  # plot(st_jitter(st_geometry(mw_data_sf), 
  #                factor = .005),
  #      add = TRUE,
  #      col = 'red',
  #      cex = 0.01,
  #      pch = 0.01)
  
  ### A.) mw_data OHNE die Angebote, bei denen die Koordinaten fehlen: ####
  mw_data_clean <- mw_data %>% 
    drop_na(geo_lat) 
  
  ### B.) Generierung von gejitterten Daten ####
  mw_data_sf_clean_j <- mw_data_clean %>% 
    # Wir wollen nur DISTINCT Koordinaten behalten, damit jitter der selbe ist:
    # Erstmal keep all sonst wird geo_lon/lat rausgekickt 
    distinct(geo_lat, geo_lon, .keep_all=TRUE) %>% 
    # Wir behalten nur geo_lat, geo_lon
    dplyr::select(geo_lat, geo_lon) %>% 
    # convert to sf Object
    st_as_sf(
      coords = c("geo_lon","geo_lat"),
      agr = "constant",
      crs = 4326,
      remove = FALSE) %>% 
    # Finally jitter: Achtung jitterfactor!
    st_jitter(factor = mw_jitterfactor)
  
  
  ### Join A.) mit B.) -> Leftjoin damit rows erhalten bleiben! ####
  mw_data_sf_clean_j_full <- left_join(mw_data_clean,
                                       mw_data_sf_clean_j) %>%
    st_as_sf(sf_column_name = "geometry")
  
  ### Kontrolle ####
  
  # Karte: wie die gejitterten Daten im Vergleich zu den Originaldaten liegen:
  #  mapview(st_geometry(mw_data_sf), color = "blue") + 
  #  mapview(st_geometry(mw_data_sf_clean_j_full), color="red")
  
  # Daten: alte geo_ mit geo_jittered vergleichen
  # mw_data_sf_clean_j_full %>% 
  #   sfc_as_cols(names = c("geo_jittered_lon",
  #                         "geo_jittered_lat")) %>%  
  #   select(ends_with("lat"), ends_with("lon")) %>%
  #   slice(1000:5000) %>% 
  #   View()
  
  ### Finalize dataset mit umbenennen der Variablen ####
  mw_data_jittered <- mw_data_sf_clean_j_full %>% 
    dplyr::select(-geo_lat, -geo_lon) %>% 
    sfc_as_cols(names = c("geo_lon",
                          "geo_lat"))
  
  # SF aus mw_data_jittered entfernen
  mw_data_jittered <- st_set_geometry(mw_data_jittered, NULL) 
  
  return(mw_data_jittered)
  
}
