#### Function that downloads fresh Mietenwatch data from database  
# and stores it as .Rdata in "./data"
refresh_mw_data <- function(){
  
  # This downloads fresh raw data from the SQL database 
  mw_data <- download_is24view_from_database()
  
  # Der gerade runtergeladene Datensatz "mw_data" ist ein 
  # SQL-View Datensatz "is24_clean_with_rentindex2017" und schon etwas bereinigt.
  # Es wurden diverse ungültige/unplausible Daten 
  # ausgeschlossen. Etwa Wohnungen, die 100000 Euro/Quadratmeter kosten etc.
  
  # Deswegen hier nur zusätzliche Datensäuberungen, vor allem geht es um
  # das richtige einlesen der Daten: Factors als Factors und nicht Chr Vectors, etc.
  
  # Wir nennen den SF Datensatz mit den Geometrien "mw_data_sf".
  
  mw_data_sf <- mw_data %>% 
    prepare_mw_data() %>% 
    calculate_kostendeckende_miete() %>% 
    calculate_mietendeckel_miete() %>% 
    create_mw_data_sf() %>% 
    clean_mw_data() %>% 
    # Reorder Variables alphabetically
    dplyr::select(sort(peek_vars())) %>%
    # Then reorder Variables according to prefix
    dplyr::select(starts_with("is_"),
                  starts_with("geo_"),
                  starts_with("obj_"),
                  starts_with("eqp_"),
                  starts_with("egy_"),
                  starts_with("des_"),
                  starts_with("cst_"),
                  starts_with("rid_"),
                  starts_with("pol_")) %>% 
    # We kick out the descriptions: For now we dont use them in the analysis
    # --> they just bloat our dataset
    dplyr::select(-starts_with("des_"),
                  -obj_foto)
  
  # "mw_data" ist OHNE Geometrien, aber mit geo_lat & geo_lon.
  # Wir Kicken hier die Geometrien raus, die Variablen geo_sbahn etc. bleiben aber drinnen.
  mw_data <- st_set_geometry(mw_data_sf, NULL) 
  
  ### Save all data locally
  
  # Delete old data
  file.remove(list.files("./data",
                         pattern="mw_data*",
                         include.dirs = T,
                         full.names = T))
  
  # Create new dataname and data then save
  mw_data_filename <- paste("mw_data_",
                            Sys.Date(),
                            ".RData",
                            sep="")
  
  save(mw_data_sf, # mit Geometrien
       mw_data, # Ohne Geometrien
       file = paste("./data/",mw_data_filename,sep=""))
}
