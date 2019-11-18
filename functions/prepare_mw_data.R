##### Daten für R vorbereiten #########
#######################################

prepare_mw_data <- function(mw_data){

  # Den Rest der zu enteignenden scheint es im Datensatz nicht zu geben.
  exprop_anbieter <- c("Deutsche Wohnen Gruppe",
                       "Vonovia SE",
                       "ADO Immobilien Management GmbH",
                       "AKELIUS GmbH",
                       "Covivio Immobilien GmbH",
                       "Grand City Property Ltd., ZNL Dtl.",
                       "Grand City Property",
                       #"Hilfswerk-Siedlung GmbH", # rausgenommen weil nicht profitorientiert
                       "BGP Immobilienservice GmbH") 
  
  ############################################
  # Variablengenerierung und Umstrukturierung
  ############################################
  
  mw_data <- mw_data %>% 
    # unnötige Variablen rauskicken
    dplyr::select(-geo_strasse,
                  -geo_kreis,
                  -geo_bundesland,
                  -geo_land,
                  -geo_bezirk) %>%
    # wir nehmen hier geo_bezirk raus weil die Variable von IS unbrauchbar ist
    # Wir spielen später mit den geodata den Bezirk wieder zu
    mutate(is_anbieter = fct_explicit_na(factor(is_anbieter), "keine Angaben"),
           # Zu enteignende Firmen
           is_anbieter_exprop = fct_collapse(is_anbieter,
                                             `Deutsche Wohnen` = "Deutsche Wohnen Gruppe",
                                             `Vonovia` = "Vonovia SE",
                                             `ADO Immobilien`= "ADO Immobilien Management GmbH",
                                             Akelius = "AKELIUS GmbH",
                                             `Convivio` = "Covivio Immobilien GmbH",
                                             `Grand City Properties`= c("Grand City Property Ltd., ZNL Dtl.",
                                                                        "Grand City Property"),
                                             `BGP Investment` = "BGP Immobilienservice GmbH",
                                             # group_other Option funktioniert für den Befehl fct_collapse hier nicht
                                             `andere Anbieter` = levels(is_anbieter)[which(!levels(is_anbieter) %in% c(exprop_anbieter,
                                                                                                                       "keine Angaben"))],
                                             `keine Angaben` = "keine Angaben"),
           # Logical variable: Zu enteignen oder nicht
           is_anbieter_is_exprop = case_when(is_anbieter_exprop %in% c("keine Angaben",
                                                                       "andere Anbieter") ~ FALSE,
                                                    TRUE ~ TRUE),
           cst_nebenkosten_sqm = cst_nebenkosten / obj_wohnflaeche,
           cst_gesamtmiete_sqm = cst_sqm + cst_nebenkosten_sqm, 
           # Objektzustand beschrieben wie auf Immoscout Seite
           obj_zustand = fct_recode(factor(obj_zustand),
                                    Erstbezug = "first_time_use",
                                    `Erstbezug nach Sanierung`= "first_time_use_after_refurbishment",
                                    `vollständig renoviert` = "fully_renovated",
                                    neuwertig = "mint_condition",
                                    modernisiert = "modernized",
                                    renovierungsbedürftig = "need_of_renovation",
                                    `nach Vereinbarung` = "negotiable",
                                    `keine Informationen` = "no_information",
                                    saniert = "refurbished",
                                    gepflegt = "well_kept"),
           obj_wohnungsart = fct_recode(factor(obj_wohnungsart),
                                        Etagenwohnung = "apartment",
                                        Erdgeschosswohnung = "ground_floor",
                                        Souterrain = "half_basement",
                                        Loft = "loft",
                                        Maisonette = "maisonette",
                                        Sonstige = "other",
                                        Penthouse = "penthouse",
                                        Hochparterre = "raised_ground_floor",
                                        Dachgeschoss = "roof_storey",
                                        Terrassenwohnung = "terraced_flat",
                                        `keine Informationen` = "no_information"),
           obj_neubau = fct_recode(factor(obj_neubau),
                                   Neubau = "y",
                                   `Kein Neubau` = "n"),
           obj_renovierungsjahr = as.integer(obj_renovierungsjahr),
           is_eingestellt = as.Date(is_eingestellt),
           is_entfernt = as.Date(is_entfernt),
           rid_areaWas_17 = factor(rid_areaWas_17),
           rid_areaWas_19 = factor(rid_areaWas_19),
           rid_noiseStatus_17 = as.logical(rid_noiseStatus_17),
           rid_noiseStatus_19 = as.logical(rid_noiseStatus_19)) %>% 
    mutate_at(vars(contains('eqp_')), as.factor) %>% 
    # die binären Equipment Variablen werden in Logical (true/false) umgewandelt.
    # ist besser für spätere Modellrechnungen
    mutate(eqp_garten = convert_2lev_factor_to_logical(eqp_garten),
           eqp_kueche = convert_2lev_factor_to_logical(eqp_kueche),
           eqp_keller = convert_2lev_factor_to_logical(eqp_keller),
           eqp_balkon = convert_2lev_factor_to_logical(eqp_balkon),
           eqp_aufzug = convert_2lev_factor_to_logical(eqp_aufzug),
           eqp_moebliert = convert_2lev_factor_to_logical(eqp_moebliert)) %>% 
    mutate(
          obj_baujahr_5y = cut(obj_baujahr,
                                breaks = seq(from = 1800, to = 2020, by = 5),
                                ordered_result = T,
                                dig.lab = 4),
           obj_baujahr_10y = cut(obj_baujahr,
                                 breaks = seq(from = 1800, to = 2020, by = 10),
                                 ordered_result = T,
                                 dig.lab = 4),
           egy_kategorie = case_when(egy_verbrauchswert<=25 ~ "A+",
                                     egy_verbrauchswert>25 & egy_verbrauchswert<=75 ~ "A und B",
                                     egy_verbrauchswert>75 | is.na(egy_verbrauchswert) ~ "schlechter als B")) %>% 
    mutate(obj_baujahr    = as.Date(ISOdate(obj_baujahr, 6, 30)), #Baujahr als Date, Datum ist Jahresmitte
           obj_alter = year(Sys.Date())-year(obj_baujahr)) %>% # Gebäudealter
    # wähle gültigen Mietspiegel für Stichtag 2018-09-01 ( https://www.mieterschutzbund-berlin.de/mietspiegel.html )
    mutate(rid_baseLocalRent = ifelse(is_eingestellt < "2018-09-01",
                                      rid_baseLocalRent_17, 
                                      rid_baseLocalRent_19),
           rid_maxLocalRent  = ifelse(is_eingestellt < "2018-09-01", 
                                      rid_maxLocalRent_17, 
                                      rid_maxLocalRent_19)) %>% 
    rename(cst_nettokalt_sqm = cst_sqm)  # cst_sqm ist nicht selbsterklärend
  
  
  ### Wohnlage
  mw_data$rid_objectStatus_19 <- factor(mw_data$rid_objectStatus_19, ordered=T)
  #Wohnlage (0=einfach, 1=mittel, 2=gut). Wir nehmen nur Wohnlage 2019 da sich die Ermittlungsmethode zu 2017 anscheinend geändert hat, um Konsistenz in der Datengrundlage zu gewährleisten (Wohnlageermittlungsmodell, https://www.stadtentwicklung.berlin.de/wohnen/mietspiegel/de/wohnlagen.shtml)
  
  #### Parkplatz
  #Neue logical Parkplatz Variable. Geht irgendwie in dplyr nicht.
  mw_data$eqp_parkplatz <- replace_na((mw_data$eqp_parkplaetze %>% 
                                         as.character() %>% 
                                         as.numeric()>=1), # wenn 1 oder mehr Stellplätze, dann gibt es einen Parkplatz
                                      FALSE) # bei NA nehmen wir an, dass es keinen Parkplatz gibt
  
  
  #### Aggregierte Anbieter ####
  # Hier fassen wir die verschiedenen subunternehmen der landeseigenen Wohnungsbaugesellschaften zusammen
  # in der neuen Variable "is_anbieter_agg"
  
  mw_data$is_anbieter_agg <- mw_data %>% 
    pull(is_anbieter) %>% 
    as.character() %>% 
    str_replace(regex("degewo.*", ignore_case = T),                                         
                "degewo AG") %>% 
    str_replace(regex("gewobag.*", ignore_case = T),
                "Gewobag AG") %>% 
    str_replace(regex("HOWOGE.*", ignore_case = T),
                "HOWOGE GmbH") %>% 
    str_replace(regex("GESOBAU.*", ignore_case = T),
                "GESOBAU AG") %>% 
    str_replace(regex("STADT UND LAND.*", ignore_case = T),
                "Stadt und Land GmbH") %>% # WBM hat keine Tochterfirmen
    na_if("keine Angaben") %>% 
    factor()
  

  mw_data <- mw_data %>% 
    ### Binäre Variable ob Angebot von den Landeseigenen kommt oder nicht
    mutate(is_anbieter_landeseigene = ifelse(is_anbieter_agg %in% c("degewo AG",
                                                                    "Gewobag AG",
                                                                    "GESOBAU AG",
                                                                    "HOWOGE GmbH",
                                                                    "WBM Wohnungsbaugesellschaft Berlin-Mitte mbH",
                                                                    "Stadt und Land GmbH"),
                                             TRUE, FALSE)) %>% 
    ### Big Player Variable erstellen
    # Anzahl der Angebote des Anbieters heisst nun: "is_anbieter_agg_anzahl"
    add_count(is_anbieter_agg, sort=TRUE, 
              name = "is_anbieter_agg_anzahl") %>% 
    # Logische variable ob das Angebot zu den 10 Big Players gehört oder nicht:
    mutate(is_anbieter_bigplayer = ifelse(group_indices(.,
                                                        factor(is_anbieter_agg,
                                                               levels = unique(is_anbieter_agg)))<=10,
                                          TRUE,
                                          FALSE)) 
  

  return(mw_data)
}
