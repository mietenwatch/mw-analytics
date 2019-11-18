#### Excludes offers that are implausable by their values

exclude_implausible_offers <- function(mw_data_raw) {
  
  # We assume that faulty item values render the entire offer invalid.
  
  # Rough Effects:
  # Filter cst_gesamtmiete: Excludes around 200 implausable offers
  # Filter cst_nettokalt: Excludes around 70 implausable offers
  # Filter cst_betrieb & cst_heiz: Excludes around 70 implausable offers
  # Filter obj_baujahr: Excludes around 50 implausable offers
  # Filter obj_wohnflaeche: Excludes around 60 implausable offers
  # Filter geo_stockwerk: Excludes around 30 implausable offers
  
  # Strongest outliers according to each distribution excluded.
  
  mw_data_implausible_excluded <- mw_data_raw %>%
    filter(
        cst_gesamtmiete >= cst_nettokalt | is.na(cst_gesamtmiete_sqm),
        # We allow missing cst_gesamtmiete in update. 
        # Missing cst_gesamtmiete will be replaced later if valid cst_nebenkosten exist or
        # may be calculated
        cst_gesamtmiete_sqm > 3 | is.na(cst_gesamtmiete_sqm),
        cst_gesamtmiete_sqm < 100 | is.na(cst_gesamtmiete_sqm),
        # There are no NAs in nettokalt in raw data - this is why we prioritize it
        # in create_cols
        cst_nettokalt >= 100,
        cst_nettokalt <= 16000,
        cst_betrieb < 4000 | is.na(cst_betrieb),
        cst_heiz < 1000 | is.na(cst_heiz),
        (cst_betrieb < cst_nettokalt) | is.na(cst_betrieb < cst_nettokalt),
        (cst_heiz < cst_nettokalt) | is.na(cst_heiz < cst_nettokalt),
        # obj_baujahr not as date here, because it comes as int from sql
        obj_baujahr <= 2019 | is.na(obj_baujahr),
        obj_baujahr >= 1800 | is.na(obj_baujahr),
        # There are no NAs in floor area in raw data.
        obj_wohnflaeche >= 8,
        obj_wohnflaeche <= 600,
        obj_zimmer < 12 | is.na(obj_zimmer),
        # average room size (excluding kitchen and bathroom) may not exceed 200sqm
        (obj_wohnflaeche / obj_zimmer) <= 200 | is.na(obj_wohnflaeche / obj_zimmer),
        geo_stockwerk <= 30 | is.na(geo_stockwerk)
    ) 
    
  return(mw_data_implausible_excluded)
}
  