##### CREATE SF OBJECT ####
###########################

create_mw_data_sf <- function(mw_data) {
  
  ### Aus mw_data ein "sf" object machen --> Es sind ja schließlich geodata!
  mw_data_sf <- mw_data %>%
    st_as_sf(
      coords = c("geo_lon","geo_lat"),
      agr = "constant",
      crs = 4326,
      na.fail = FALSE, # WICHTIG! Wir wollen die Angebote ohne Adresse nicht rausschmeissen!
      remove = FALSE)
  
  ####### geodata: Liegt das Angebot im SBahnring? Bezirk? Ortsteil? etc.
  
  load("data/geodata/mw_geodata.RData")
  
  ### 1. S-Bahnring ####
  
  # Leftjoin mit mw_data
  mw_data_sf <- st_join(mw_data_sf, sf_sbahn_ring) %>% 
    mutate(geo_sbahn = fct_explicit_na(geo_sbahn, 
                                       na_level = "ausserhalb")) 
  
  # wollte das mit Dplyr machen, gab aber n komischen Fehler. deswegen in base.
  mw_data_sf$geo_sbahn <- as.factor(mw_data_sf$geo_sbahn)
  
  #### 2. LOR Planungsräume/Bezirksregionen/Prognoseräume/Bezirke ####
  
  sf_LOR <- readOGR("data/geodata/lor_planungsraeume.shp/lor_planungsraeume.shp", 
                    verbose=F) %>% 
    st_as_sf() %>% 
    rename(geo_lor_planungsraum_id = "spatial_na",
           geo_lor_planungsraum = "PLRNAME",
           geo_lor_bezirksregion = "BZRNAME",
           geo_lor_prognoseraum = "PGRNAME",
           geo_bezirk = "BEZNAME") %>% 
    dplyr::select(starts_with("geo")) %>% 
    st_transform(4326)
  
  # Leftjoin mit mw_data
  mw_data_sf <- st_join(mw_data_sf, sf_LOR) 
  
  #### Ortsteile ####
  
  # Wir laden die Ortsteile aus dem shape file rein, nicht vom WFS des FIS Broker.
  # Die shape daten sind besser weil mit IDs
  
  sf_ortsteile <- readOGR("data/geodata/ortsteile_statistikamt_bb_1412/RBS_OD_ORT_1412.shp", 
                          verbose=F) %>% 
    st_as_sf() %>% 
    rename(geo_ortsteil = "Ortsteilna",
           geo_ortsteil_id = "ORT") %>% 
    dplyr::select(starts_with("geo")) %>% 
    st_transform(4326)
  
  # Leftjoin mit mw_data
  mw_data_sf <- st_join(mw_data_sf, sf_ortsteile) 
  
  ### Milieuschutzgebiete
  mw_data_sf <- st_join(mw_data_sf, sf_milieuschutz_baulich)
  mw_data_sf <- st_join(mw_data_sf, sf_milieuschutz_sozial)
  mw_data_sf$pol_milieu_s <- ifelse( is.na(mw_data_sf$pol_milieu_s_name), FALSE, TRUE) #ist das Angebot innerhalb eines Millieuschutzgebiets?
  mw_data_sf$pol_milieu_b <- ifelse( is.na(mw_data_sf$pol_milieu_b_name), FALSE, TRUE) #ist das Angebot innerhalb eines Millieuschutzgebiets?
  
  # Bezirk ist schon in "geo_bezirk"
  mw_data_sf <- mw_data_sf %>% dplyr::select(-pol_milieu_b_bezirk,
                                             -pol_milieu_s_bezirk)
  
  return(mw_data_sf)
}