
# Input: mw_data
# Output: 1) variable- importance als csv
#         2) modellobjekte von RF und GBM

fit_randomforest <- function(data) {

  # ===== Data preprocessing ===============

  # select influence factors
  mw_cstInfl <- data %>% select(
    cst_nettokalt_sqm, # dependent variable
    obj_zustand,
    eqp_garten,
    eqp_aufzug,
    eqp_moebliert,
    eqp_balkon,
    rid_objectStatus_19,
    rid_noiseStatus_19,
    geo_stockwerk,
    obj_stockwerke,
    obj_baujahr,
    is_anbieter_bigplayer,
    is_anbieter_agg,
    geo_bezirk,
    cst_nebenkosten_sqm,
    obj_wohnflaeche,
    geo_sbahn
  )

  # include only offers from 10 biggest players (public & private)
  mw_cstInfl <- mw_cstInfl %>%
    filter(is_anbieter_bigplayer) %>%
    select(-is_anbieter_bigplayer)
  ### missing values ###
  mw_cstInfl <- mw_cstInfl %>%
    # remove Anzahl Stockwerke because too many NA's
    select(-obj_stockwerke) %>%
    # replace NA for indicators where a missing value indicates lack of value-improving feature
    replace_na(list(
      eqp_moebliert = 0,
      eqp_aufzug = 0,
      eqp_garten = 0,
      eqp_balkon = 0
    )) %>%
    # remove NA rows beause algorithm only works without NA's
    na.omit()
  # remove unrealistic outliers for stockwerk
  mw_cstInfl <- mw_cstInfl %>% filter(geo_stockwerk <= 40)
  # remove no data in objektzustand because this predictor is analytically relevant for our model -> reduces samplesize by ~ 1/3
  mw_cstInfl <- filter(mw_cstInfl, (obj_zustand != "keine Informationen"))
  # drop unused factor level
  mw_cstInfl$obj_zustand <- factor(mw_cstInfl$obj_zustand)
  # Flats from Vonovia are lost because they do not present any object condition

  ### transform variables for Random Forest ###
  # drop unused factor levels
  mw_cstInfl <- mw_cstInfl %>%
    mutate(is_anbieter_agg = factor(is_anbieter_agg)) %>%
    # covnert logical to factor for GBM
    mutate_if(is.logical, factor) %>%
    mutate_at(vars(starts_with("eqp")), factor) %>%
    # convert year to numeric for Random Forest
    mutate_if(is.Date, as.numeric) %>%
    # z-transform everything to speed up computation of Random Forest
    mutate_if(is.numeric, function(x) as.numeric(scale(x)))

  # ======= fit randomForest ==============
  ### Random Forest ###
  f.rf <- randomForest(formula = formula("cst_nettokalt_sqm ~ ."), data = mw_cstInfl, keep.inbag = T, importance = T)

  save(f.rf, mw_cstInfl, file = paste0("data/randomForest_cst_", Sys.Date(), ".RData"))
  # load(file=paste0("../data/randomForest_cst_",Sys.Date(),".RData"))

  # calculate variable importance
  i.rf <- importance(f.rf)
  i.rf <- tibble(Wichtigkeit = i.rf[, 2], Einflussgröße = rownames(i.rf)) %>%
    # scale so that values lie between {1, 100} for better interpretation
    mutate(Wichtigkeit = Wichtigkeit - min(Wichtigkeit)) %>% # set min to 0
    mutate(Wichtigkeit = Wichtigkeit * 100 / max(Wichtigkeit) + 1) %>% # set max to ~ 100
    # rename vars
    spread(key = Einflussgröße, value = Wichtigkeit) %>%
    rename_recode_cols_for_export(mutate = F, rename = T) %>% # rename (and dont mutate)
    gather(key = "Einflussgröße", value = "Wichtigkeit") %>%
    mutate(Wichtigkeit = round(Wichtigkeit, 2)) %>%
    arrange(desc(Wichtigkeit))

  return(i.rf)
}
