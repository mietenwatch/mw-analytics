#### Calculate Mietendeckel Analysis ####
#########################################

calculate_mietendeckel_miete <- function(mw_data) {
  
  # Nur mit Daten rechnen die ein obj_baujahr haben & keine Kohleheizung!
  mw_data_for_md <- mw_data %>% 
    # Wohnungen ohne Baujahr werden raus gekickt
    filter(!is.na(obj_baujahr)) 
  
  # Mietobergrenzen nach Referententwurf (immer für Bad & Sammelheizung)
  # md = MietenDeckel
  md_bis1918 <- 6.45
  md_1919_1949 <- 6.27
  md_1950_1964 <- 6.08
  md_1965_1972 <- 5.95
  md_1973_1990 <- 6.04
  md_1991_2002 <- 8.13
  md_2003_2013 <- 9.80
  
  # Berechnung der MIETENDECKEL Miete
  mw_data_with_md <- mw_data_for_md %>% 
    mutate(cst_md_modaufschlag_sqm = case_when(egy_verbrauchswert<=75 & obj_alter > 15 ~ 1.40,
                                               TRUE ~ 0),
           # Mietendeckelmiete pro QM
           cst_md_nettokalt_sqm = case_when(year(obj_baujahr) <= 1918 ~ md_bis1918 + cst_md_modaufschlag_sqm,
                                            year(obj_baujahr) %between% c(1919, 1949) ~ md_1919_1949 + cst_md_modaufschlag_sqm,
                                            year(obj_baujahr) %between% c(1950, 1964) ~ md_1950_1964 + cst_md_modaufschlag_sqm,
                                            year(obj_baujahr) %between% c(1965, 1972) ~ md_1965_1972 + cst_md_modaufschlag_sqm,
                                            year(obj_baujahr) %between% c(1973, 1990) ~ md_1973_1990 + cst_md_modaufschlag_sqm,
                                            year(obj_baujahr) %between% c(1991, 2002) ~ md_1991_2002 + cst_md_modaufschlag_sqm,
                                            year(obj_baujahr) %between% c(2003, 2013) ~ md_2003_2013 + cst_md_modaufschlag_sqm,
                                            TRUE ~ cst_nettokalt_sqm),
           # Nettokaltmiete für gesamte Wohnung bei greifendem Mietendeckel
           cst_md_nettokalt = round(cst_md_nettokalt_sqm * obj_wohnflaeche, 2),
           # Gesamtmiete für gesamte Wohnung bei greifendem Mietendeckel
           cst_md_gesamtmiete = round(cst_md_nettokalt + cst_nebenkosten, 2),
           # Gesamtmiete für gesamte Wohnung pro m²  bei greifendem Mietendeckel
           cst_md_gesamtmiete_sqm = round(cst_md_nettokalt_sqm + cst_nebenkosten_sqm, 2)) %>% 
    # Wir joinen gleich, der Rest der Daten kann raus aus mw_data_with_md
    dplyr::select(is_id, 
                  starts_with("cst_md"))
  
  # Daten wieder zusammenführen: mw_data hat die Angebote ohne Baujahr & Kohleheizung drinnen,
  # mw_data_with_md nicht
  mw_data_with_md <- left_join(mw_data, mw_data_with_md)
  
  return(mw_data_with_md)
  
}
