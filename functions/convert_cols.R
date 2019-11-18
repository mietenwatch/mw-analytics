##### Comvert col types and levels of existing cols #########
#############################################################

convert_cols <- function(mw_data) {
  
mw_data_with_converted_cols <-  mw_data %>%
    mutate_at(
      vars(contains("eqp_")), as.factor
      ) %>%
    mutate(
      # date offer went online
      is_eingestellt = as.Date(is_eingestellt),
      # date it was removed
      is_entfernt = as.Date(is_entfernt),
      is_anbieter = fct_explicit_na(factor(is_anbieter), "keine Angaben"),
      # translating state of apartment
      obj_zustand = fct_recode(factor(obj_zustand),
        Erstbezug = "first_time_use",
        `Erstbezug nach Sanierung` = "first_time_use_after_refurbishment",
        `vollständig renoviert` = "fully_renovated",
        neuwertig = "mint_condition",
        modernisiert = "modernized",
        renovierungsbedürftig = "need_of_renovation",
        `nach Vereinbarung` = "negotiable",
        `keine Informationen` = "no_information",
        saniert = "refurbished",
        gepflegt = "well_kept"),
      # translating type of apartment
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
      # Bulding year as date - date of each years is mid of year
      obj_baujahr = as.Date(ISOdate(obj_baujahr, 6, 30)),
      # All equipment vars will be transformed into logical
      # --> Better for later models and calculations
      eqp_garten = convert_2lev_factor_to_logical(eqp_garten),
      eqp_kueche = convert_2lev_factor_to_logical(eqp_kueche),
      eqp_keller = convert_2lev_factor_to_logical(eqp_keller),
      eqp_balkon = convert_2lev_factor_to_logical(eqp_balkon),
      eqp_aufzug = convert_2lev_factor_to_logical(eqp_aufzug),
      eqp_moebliert = convert_2lev_factor_to_logical(eqp_moebliert),
      rid_areaWas_17 = factor(rid_areaWas_17),
      rid_areaWas_19 = factor(rid_areaWas_19),
      rid_noiseStatus_17 = as.logical(rid_noiseStatus_17),
      rid_noiseStatus_19 = as.logical(rid_noiseStatus_19),
      # rid_objectStatus is "Wohnlage" (0=simple, 1=mid, 2=good). 
      # We only use "Wohnlage" 2019 because calculation was changed in comparison to 2017
      # https://www.stadtentwicklung.berlin.de/wohnen/mietspiegel/de/wohnlagen.shtml)
      rid_objectStatus_19 = factor(rid_objectStatus_19, ordered = T))

  return(mw_data_with_converted_cols)
}
