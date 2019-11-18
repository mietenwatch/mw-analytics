#### Calculate rent cap "Mietendeckel" (=md) rent ####
######################################################

create_mietendeckel_miete <- function(mw_data_2) {
  
  # We only use offers that have valid obj_baujahr (nescessary for md calculation)
  mw_data_for_md <- mw_data_2 %>% 
    filter(!is.na(obj_baujahr)) 
  
  # Most recent proposal of rent cap law:
  # https://www.stadtentwicklung.berlin.de/wohnen/wohnraum/mietendeckel/download/Gesetzentwurf-Neuregelung-Mietenbegrenzung-MietenWoGBln.pdf 
  # md = MietenDeckel
  md_bis1918 <- 6.45
  md_1919_1949 <- 6.27
  md_1950_1964 <- 6.08
  md_1965_1972 <- 5.95
  md_1973_1990 <- 6.04
  md_1991_2002 <- 8.13
  md_2003_2013 <- 9.80
  
  # Calculation of md rent according to pdf linked above
  mw_data_with_md <- mw_data_for_md %>% 
    mutate(
      # §5 (2) cannont be analyzed with our data. eqp_wohnungsart does not allow for counting apartments in house, neither obj_stockwerke
      # §5 (3) - 1 Euro + if at least 3 valued adding traits are present in offer
      cst_md_1euroaufschlag = case_when((replace_na(eqp_kueche, FALSE) +
                                         replace_na(eqp_aufzug, FALSE) +
                                        # we proxy luxurious bathroom and floor by eqp_innen either luxury or sophisticated
                                        (eqp_innen == "luxury" | eqp_innen == "sophisticated") +
                                        (replace_na(egy_verbrauchswert, 121) < 120)) >= 3 ~ 1,
                                        TRUE ~ 0),
      cst_md_nettokalt_sqm = case_when(year(obj_baujahr) <= 1918 & obj_is_gefoerdert == FALSE ~ md_bis1918 + cst_md_1euroaufschlag,
                                       year(obj_baujahr) %between% c(1919, 1949) & obj_is_gefoerdert == FALSE ~ md_1919_1949 + cst_md_1euroaufschlag,
                                       year(obj_baujahr) %between% c(1950, 1964) & obj_is_gefoerdert == FALSE ~ md_1950_1964 + cst_md_1euroaufschlag,
                                       year(obj_baujahr) %between% c(1965, 1972) & obj_is_gefoerdert == FALSE ~ md_1965_1972 + cst_md_1euroaufschlag,
                                       year(obj_baujahr) %between% c(1973, 1990) & obj_is_gefoerdert == FALSE ~ md_1973_1990 + cst_md_1euroaufschlag,
                                       year(obj_baujahr) %between% c(1991, 2002) & obj_is_gefoerdert == FALSE ~ md_1991_2002 + cst_md_1euroaufschlag,
                                       year(obj_baujahr) %between% c(2003, 2013) & obj_is_gefoerdert == FALSE ~ md_2003_2013 + cst_md_1euroaufschlag,
                                       TRUE ~ cst_nettokalt_sqm),
      cst_md_nettokalt = round(cst_md_nettokalt_sqm * obj_wohnflaeche, 2),
      cst_md_gesamtmiete = round(cst_md_nettokalt + cst_nebenkosten, 2),
      cst_md_gesamtmiete_sqm = round(cst_md_gesamtmiete / obj_wohnflaeche, 2)
      ) %>% 
    # We will join with complete dataset shortly, we exclude all the rest here from mw_data_with_md
    dplyr::select(is_id, 
                  starts_with("cst_md")) 
  
  # Join data again
  mw_data_with_md <- left_join(mw_data_2, mw_data_with_md)
  
  return(mw_data_with_md)
  
}
