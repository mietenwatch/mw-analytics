##### Create new cols from existing ones #########
##################################################

create_cols <- function(mw_data_0){

  #### 1. STEP: Create/correct easy, non-seller specific cols ####
  
  mw_data_1 <- mw_data_0 %>% 
    mutate(
      ### Correction cst_betrieb 
      # in case cst_betrieb includes cst_heiz 
      # (thats the case when cst_gesamtmiete is just the sum of cst_nettokalt and cst_betrieb)
      # we deduct it to correct cst_betrieb
      cst_betrieb = case_when(
        # we only update cst_betrieb if:
        ((cst_betrieb > 0) & 
         (cst_betrieb > cst_heiz) & # otherwise we get negative cst_betrieb
         (cst_gesamtmiete - cst_nettokalt - cst_betrieb) == 0) ~ (cst_betrieb - replace_na(cst_heiz, 0)),
        TRUE ~ cst_betrieb),
      ### Create cst_nebenkosten 
      cst_nebenkosten = case_when(
        # additional costs are sum of cst_betrieb and cst_heiz (cst_heiz are considered part of "nebenkosten") -->
        # https://www.wiwo.de/finanzen/immobilien/nebenkostenabrechnung-was-zu-den-nebenkosten-gehoert/13328890-2.html
        # https://www.nebenkostenabrechnung.com/nebenkosten/
        !is.na(cst_betrieb) & !is.na(cst_heiz) ~ (cst_betrieb + cst_heiz),
        # if there is a missing in either cst_heiz or cst_betrieb, we take the diff between cst_gesamt and cst_netto
        (cst_gesamtmiete > cst_nettokalt) & (is.na(cst_betrieb) | is.na(cst_heiz)) ~ (cst_gesamtmiete - cst_nettokalt),
        # We leave cases where cst_gesamtmiete = cst_nettkalt in the dataset:
        # We always receive cst_nebenkosten missing - because undefined.
        # These stay in the dataset - we just exclude them in nebenkosten analysis.
        TRUE ~ NA_real_),
      ### Correct cst_gesamtmiete 
      # Fill NAs if valid data from cst_nettokalt + cst_nebekosten exists
      cst_gesamtmiete = case_when(
        is.na(cst_gesamtmiete) & !is.na(cst_nebenkosten) ~ (cst_nettokalt + cst_nebenkosten),
        TRUE ~ cst_gesamtmiete),
      ### --> NO correction for cst_nettokalt, we prioritize its validity as "best" info from landlords
      ### Create all rents per sqm 
      cst_nettokalt_sqm = cst_nettokalt / obj_wohnflaeche,
      cst_betrieb_sqm = cst_betrieb / obj_wohnflaeche,
      cst_nebenkosten_sqm = cst_nebenkosten / obj_wohnflaeche,
      cst_gesamtmiete_sqm = cst_gesamtmiete / obj_wohnflaeche, 
      ### Age and age categories 
      obj_alter = year(Sys.Date()) - year(obj_baujahr),
      obj_baujahr_5y = cut(year(obj_baujahr),
                           breaks = seq(from = 1800, to = 2020, by = 5),
                           ordered_result = T,
                           dig.lab = 4),
      obj_baujahr_10y = cut(year(obj_baujahr),
                                 breaks = seq(from = 1800, to = 2020, by = 10),
                                 ordered_result = T,
                                 dig.lab = 4),
      ### Energy efficiency classes
      # Classes defined in "Energieausweis" here:
      # https://www.verbraucherzentrale.de/wissen/energie/energetische-sanierung/energieausweis-was-sagt-dieser-steckbrief-fuer-wohngebaeude-aus-24074
      egy_kategorie = case_when(egy_verbrauchswert <= 25 ~ "A+",
                                egy_verbrauchswert > 25 & egy_verbrauchswert <= 75 ~ "A und B",
                                egy_verbrauchswert > 75 | is.na(egy_verbrauchswert) ~ "schlechter als B"),
      ### Parking
      # Logical whether parking space exists
      eqp_parkplatz = replace_na((eqp_parkplaetze %>% 
                                    as.character() %>%
                                    # if one or more parking spaces, then true
                                    as.numeric()>=1), 
                                 # for NA we assume there is no parking sapce
                                 FALSE),
      ### Mietspiegel
      # Choose correct Mietspiegel with deadline 2018-09-01 ( https://www.mieterschutzbund-berlin.de/mietspiegel.html )
      # "0" in Mietspiegel has to be recoded to "NA". Same for rent_limit
      rid_baseLocalRent = (ifelse(is_eingestellt < "2018-09-01",
                                 rid_baseLocalRent_17,
                                 rid_baseLocalRent_19) %>% 
                             na_if(0)),
      rid_maxLocalRent = (ifelse(is_eingestellt < "2018-09-01",
                                rid_maxLocalRent_17,
                                rid_maxLocalRent_19) %>% 
                            na_if(0))
      )
  
  #### 2. STEP: Create easy, seller specific new cols ####
  # --> all "anbieter" related 
  
  mw_data_2 <- mw_data_1 %>% 
    mutate(is_anbieter_agg = create_is_anbieter_agg(is_anbieter),
           is_anbieter_exprop = create_is_anbieter_exprop(is_anbieter),
           # logical: to be expropriated or not
           is_anbieter_is_exprop = case_when(is_anbieter_exprop %in% c("keine Angaben",
                                                                       "andere Anbieter") ~ FALSE,
                                             TRUE ~ TRUE),
           # logical if seller is municipal "städtisch" = "landeseigen"
           is_anbieter_landeseigene = ifelse(is_anbieter_agg %in% c("degewo AG",
                                                                    "Gewobag AG",
                                                                    "GESOBAU AG",
                                                                    "HOWOGE GmbH",
                                                                    "WBM Wohnungsbaugesellschaft Berlin-Mitte mbH",
                                                                    "Stadt und Land GmbH"),
                                             TRUE, FALSE)
           ) 
  
  #### 3. STEP: Create difficult, analysis heavy cols ####
  
  mw_data_3 <- mw_data_2 %>% 
    # special regex if apartment is WBS subsidized housing
    create_obj_is_gefoerdert() %>% 
    # extra calculation of cost covering rent
    create_kostendeckende_miete() %>% 
    # rent cap rent according to latest Berlin senate decision (2019-10-22)
    create_mietendeckel_miete()

  return(mw_data_3)
}

