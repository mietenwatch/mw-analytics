#### Secondary data cleaning ####
#################################

clean_mw_data <- function(mw_data) {
  
  cleaned_mw_data <- mw_data %>% 
    # "0" im Mietspiegel muss auf "NA" umgecoded werden. "0" kam rein,
    # da es leere Mietspiegelfelder gab/gibt. Wir haben keinen gültigen Wert
    # deshalb muss das "NA" gesetzt werden. Gleiches gilt für Mietobergrenze
    mutate(rid_baseLocalRent = na_if(rid_baseLocalRent, 0),
           rid_maxLocalRent  = na_if(rid_maxLocalRent, 0),
           pol_rentLimit = na_if(pol_rentLimit, 0),
          # "é" wird nicht richtig angezeigt, wir korrigieren das.
           geo_lor_planungsraum = str_replace(as.character(geo_lor_planungsraum),"¿", "é") %>% factor()) %>% 
    # Wir kicken alle Variablen zu Gestaltschutzgebiete (Erhaltungsgebiet zur städtebauliche Eigenart),
    # sowas wie bauliche Milleuschutzgebiete raus. Die sind für unsere Analysen irrelevant.
    dplyr::select(-starts_with("pol_milieu_b")) %>% 
    dplyr::select(-GK_ABL_DAT)
  
  # Level Cleaning of Baujahr Groups
  levels(cleaned_mw_data$obj_baujahr_10y) <- levels(cleaned_mw_data$obj_baujahr_10y) %>% 
    str_replace(",","-") %>% 
    str_replace("\\(","") %>% 
    str_replace("\\]","") %>% 
    str_replace("2020","2019")
  
  levels(cleaned_mw_data$obj_baujahr_5y) <- levels(cleaned_mw_data$obj_baujahr_5y) %>% 
    str_replace(",","-") %>% 
    str_replace("\\(","") %>% 
    str_replace("\\]","") %>% 
    str_replace("2020","2019")
  
  return(cleaned_mw_data)
}