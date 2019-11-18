##############################################
#
# Chapter 2 `Wohnen als Ware`
#
##############################################

#### SETUP ####
#++++++++++++++

### libraries
if (!require("pacman")) install.packages("pacman")

library("pacman")
p_load(
  tidyverse,
  scales,
  lubridate,
  validate,
  knitr,
  rgdal, rgeos, sf, mapview, # GIS packages
  randomForest,
  viridis,
  treemap,
  klaR
)

# load latest mw_data
load(
  list.files(
    path = "data",
    pattern = "mw_data*",
    include.dirs = T,
    full.names = T)
)

# load functions
sapply(list.files(
  pattern = "[.]R$",
  path = "functions",
  full.names = TRUE),
  source)

# Um conflicts vorzubeugen
select <- dplyr::select

# relative path to website-repo for csv-export
websitePath <- "../mw-frontend/static/visualization-data/"

#### ANALYSIS ####
#+++++++++++++++++

# Wer handelt mit Wohnraum?

# Wer sind die Big Players

### 2.2.2
p2.2.2data <- mw_data %>%
  mutate(is_anbieter_agg_anteil = round(
    100 * is_anbieter_agg_anzahl / nrow(mw_data),
    1
  )) %>%
  filter(is_anbieter_bigplayer == TRUE) %>%
  group_by(is_anbieter_agg) %>%
  summarise(
    is_anbieter_agg_anteil = mean(is_anbieter_agg_anteil),
    is_anbieter_landeseigene = first(is_anbieter_landeseigene)
  ) %>%
  arrange(is_anbieter_agg_anteil)

p2.2.2data %>%
  trim_vars_4_plotting() %>%
  write_csv(path = paste0(
    websitePath,
    "2.2.2_marktanteile_bigplayers.csv"
  ))

### akkum_Marktanteil_der_BP
# Ausrechnen des Marktanteils von den Big Players
marktanteil_BP <- table(mw_data$is_anbieter_bigplayer) %>%
  prop.table() %>%
  last() %>%
  percent(1)
marktanteil_BP

### mw_data_bigplayer
# Datensatz NUR mit Big Players (= kleiner)
mw_data_bigplayer <- mw_data %>%
  filter(is_anbieter_bigplayer == TRUE) %>%
  mutate(is_anbieter_agg = factor(is_anbieter_agg))

### 2.2.7 Verteilung der Gebäudealter bei den Big Players
p2.2.7data <- mw_data_bigplayer %>%
  # Wir nehmen diese beiden Anbieter raus, weil für sie zu wenige gültige Baujahresdaten vorliegen
  filter(!is_anbieter_agg %in% c("homefully GmbH", "ROMI Immobilien GmbH")) %>%
  drop_na(obj_baujahr_10y) %>%
  mutate(is_anbieter_agg = factor(is_anbieter_agg)) %>%
  # neuer is_anbieter_agg_anzahl count weil wir NAs rausgeschmissen haben
  add_count(is_anbieter_agg, sort = TRUE, name = "is_anbieter_agg_anzahl") %>%
  group_by(is_anbieter_agg, obj_baujahr_10y) %>%
  summarise(
    anzahl_objekte = n(),
    anteil_objekte = round(100 * (anzahl_objekte / mean(is_anbieter_agg_anzahl)), 1)
  ) %>%
  ungroup() %>%
  complete(is_anbieter_agg, obj_baujahr_10y,
    fill = list(
      anzahl_objekte = 0,
      anteil_objekte = 0
    )
  ) %>%
  # Vor 1880 sind nur 2 Angebote drinnen, wir filtern nur die älteren raus
  filter(obj_baujahr_10y > "1870-1880")

## 2.2.7 export csv
p2.2.7data %>%
  select(-anzahl_objekte) %>%
  trim_vars_4_plotting() %>%
  spread(
    key = `Baujahr aggregiert (10 J.)`,
    value = anteil_objekte
  ) %>%
  write_csv(path = paste0(
    websitePath,
    "2.2.7_gebaeudealtersverteilung_bigplayers.csv"
  ))

### 2.3.2 Charakterisierung der Big Player Bestände
p2.3.2data <- mw_data_bigplayer %>%
  group_by(is_anbieter_agg) %>%
  summarise(
    `Gesamtmiete in €/m²` = mean(cst_gesamtmiete_sqm) %>% round(2),
    `Nettokaltmiete in €/m²` = mean(cst_nettokalt_sqm) %>% round(2),
    `Nebenkosten in €/m²` = mean(cst_nebenkosten_sqm) %>% round(2),
    is_anbieter_landeseigene = first(is_anbieter_landeseigene)
  ) %>%
  arrange(-`Gesamtmiete in €/m²`)

### 2.3.2 export csv
p2.3.2data <- p2.3.2data %>%
  select(
    -`Gesamtmiete in €/m²`,
    -is_anbieter_landeseigene
  ) %>%
  trim_vars_4_plotting() %>%
  rename(
    "Nettokaltmiete" = "Nettokaltmiete in €/m²",
    "Nebenkosten" = "Nebenkosten in €/m²"
  ) %>%
  arrange(Nettokaltmiete)

p2.3.2data %>%
  write_csv(path = paste0(
    websitePath,
    "2.3.2_miete_bigplayers.csv"
  ))

### 2.3.4 minimum n of offers bigplayer
# --> brauchen wir gleich für Factor recoding "andere Anbieter"
bigplayers_anzahl_threshold <- mw_data_bigplayer %>%
  select(is_anbieter_agg_anzahl) %>%
  pull() %>%
  min()

# Daten für POINT CLOUD
p2.3.4data <- mw_data %>%
  # is_anbieter_agg hier mit "andere Anbieter" für nicht Big-Players
  mutate(
    is_anbieter_agg = fct_lump_min(is_anbieter_agg,
      min = bigplayers_anzahl_threshold,
      other_level = "andere Anbieter"
    ),
    is_anbieter_agg = fct_explicit_na(is_anbieter_agg, "andere Anbieter")
  ) %>%
  arrange(-is_anbieter_agg_anzahl) %>%
  select(
    is_anbieter_agg,
    cst_nettokalt_sqm,
    cst_nebenkosten_sqm
  ) %>%
  filter(
    cst_nebenkosten_sqm < 7.5,
    cst_nettokalt_sqm < 65
  ) %>%
  filter(cst_nebenkosten_sqm != 0)

p2.3.4data %>%
  trim_vars_4_plotting() %>%
  write_csv(path = paste0(
    websitePath,
    # heisst jetzt 2.3.6!!!
    "2.3.6_miete_vs_nebenkosten_bigplayers.csv"
  ))

### 2.4.2 importance random forest
# run model fitting
p2.4.2data <- fit_randomforest(mw_data)

### 2.4.2 export csv
p2.4.2data %>%
  mutate(Einflussgröße = recode(Einflussgröße,
    `Nebenkosten in €/m²` = "Nebenkosten",
    `Wohnfläche in m²` = "Wohnfläche",
    moebliert = "Möblierung"
  )) %>%
  write_csv(paste0(websitePath, "2.4.2_wichtigkeit_random_forest.csv"))

### 2.5.2 Waterfall für NKM-KdM = Gewinne
p2.5.2data <- mw_data %>%
  drop_na(cst_km_kostdeckend_nettokalt_sqm) %>%
  # Für Gesamt-Berlin Zusammenfassen
  summarise(
    cst_nettokalt_sqm = mean(cst_nettokalt_sqm, na.rm = T),
    `Kostendeckende Miete in €/m²` = round(mean(cst_km_kostdeckend_nettokalt_sqm, na.rm = T), 2),
    `Surplus Miete in €/m²` = round(mean(cst_km_gewinnekredite_sqm, na.rm = T), 2)
  ) %>%
  trim_vars_4_plotting()

# transpose data for website export
p2.5.2data <- tibble(
  Komponente = names(p2.5.2data),
  `Berliner Durchschnitt` = (p2.5.2data %>%
    slice(1) %>%
    c(., recursive = TRUE) %>%
    unname())
)

### 2.5.2 export csv
p2.5.2data %>%
  write_csv(path = paste0(
    websitePath,
    "2.5.2_NKM_KostendeckMiete_SurplusMiete_Waterfall.csv"
  ))

### 2.5.4 Tabelle von NKM, KdM und SM nach BigPlayers
p2.5.4data <- mw_data %>%
  drop_na(cst_km_kostdeckend_nettokalt_sqm) %>%
  filter(is_anbieter_bigplayer == TRUE) %>%
  group_by(is_anbieter_agg) %>%
  summarise(
    `Nettokaltmiete in €/m²` = round(mean(cst_nettokalt_sqm, na.rm = T), 2),
    `Kostendeckende Miete in €/m²` = round(mean(cst_km_kostdeckend_nettokalt_sqm, na.rm = T), 2),
    `Surplus Miete in €/m²` = round(mean(cst_km_gewinnekredite_sqm, na.rm = T), 2)
  ) %>%
  arrange(-`Nettokaltmiete in €/m²`) %>%
  trim_vars_4_plotting()

### 2.5.4 export csv
p2.5.4data %>%
  write_csv(path = paste0(
    websitePath,
    "2.5.4_NKM_KostendeckMiete_SurplusMiete_BigPlayers_Tabelle.csv"
  ))


### mw_data_mietspiegel
## Filtern außerdem alle Angebote raus, für die kein Mietspiegel Wert zuordnbar ist
mw_data_mietspiegel <- mw_data[mw_data$rid_baseLocalRent > 0 & !is.na(mw_data$rid_baseLocalRent), ] %>%
  # Gebäude die nach 01.01.2018 fertig gestellt wurden sind vom Mietspiegel ausgenommen
  filter(obj_baujahr < as.Date("2018-01-01")) %>%
  mutate(
    is_anbieter = fct_explicit_na(is_anbieter, na_level = "keine Angaben"),
    cst_overBaseLocalRent = cst_nettokalt_sqm - rid_baseLocalRent, # Wieviel Euro/QM über Mietspiegel
    cst_overMaxLocalRent = cst_nettokalt_sqm - rid_maxLocalRent,
    cst_overBaseLocalRent_rel = cst_overBaseLocalRent / rid_baseLocalRent, # Wieviel % über Mietspiegel
    cst_overMaxLocalRent_rel = cst_overMaxLocalRent / rid_maxLocalRent,
    cst_overBaseMietbremse = cst_nettokalt_sqm - (rid_baseLocalRent * 1.1), # Mietpreisbremse lässt 10% über ortsüblich zu
    cst_overMaxMietbremse = cst_nettokalt_sqm - (rid_maxLocalRent * 1.1),
    cst_overBaseMietbremse_logical = cst_overBaseMietbremse > 0,
    cst_overMaxMietbremse_logical = cst_overMaxMietbremse > 0
  )

### 2.6.1.3 Überschreitung Mietspiegel
# Überschreitung des Mietspiegels auf Karte STADTWEIT --> nur für Text, kommt in keine Grafik/Karte
p2.6.1.3data_berlin <- mw_data_mietspiegel %>%
  # Erstmal Generierung aller möglichen Variablen zur Kontrolle
  summarise(
    overBaseLocalRent_mean = mean(cst_overBaseLocalRent, na.rm = T),
    overMaxLocalRent_mean = mean(cst_overMaxLocalRent, na.rm = T),
    overBaseLocalRent_mean_rel = mean(cst_overBaseLocalRent_rel, na.rm = T),
    overMaxLocalRent_mean_rel = mean(cst_overMaxLocalRent_rel, na.rm = T),
    overBaseLocalRent_med = median(cst_overBaseLocalRent, na.rm = T),
    overMaxLocalRent_med = median(cst_overMaxLocalRent, na.rm = T),
    overBaseLocalRent_med_rel = median(cst_overBaseLocalRent_rel, na.rm = T),
    overMaxLocalRent_med_rel = median(cst_overMaxLocalRent_rel, na.rm = T),
    n = n()
  ) %>%
  arrange(-overMaxLocalRent_med_rel) %>%
  select(
    overMaxLocalRent_med_rel,
    overMaxLocalRent_med,
    n
  ) %>%
  trim_vars_4_plotting()

### nach Bezirk

## 2.6.1.3 .. nach Bezirken
# Überschreitung des Mietspiegels auf Karte nach Bezirken --> nur für Text, kommt in keine Grafik/Karte
p2.6.1.3data_bezirk <- mw_data_mietspiegel %>%
  drop_na(geo_bezirk) %>%
  group_by(geo_bezirk) %>%
  # Erstmal Generierung aller möglichen Variablen zur Kontrolle
  summarise(
    overBaseLocalRent_mean = mean(cst_overBaseLocalRent, na.rm = T),
    overMaxLocalRent_mean = mean(cst_overMaxLocalRent, na.rm = T),
    overBaseLocalRent_mean_rel = mean(cst_overBaseLocalRent_rel, na.rm = T),
    overMaxLocalRent_mean_rel = mean(cst_overMaxLocalRent_rel, na.rm = T),
    overBaseLocalRent_med = median(cst_overBaseLocalRent, na.rm = T),
    overMaxLocalRent_med = median(cst_overMaxLocalRent, na.rm = T),
    overBaseLocalRent_med_rel = median(cst_overBaseLocalRent_rel, na.rm = T),
    overMaxLocalRent_med_rel = median(cst_overMaxLocalRent_rel, na.rm = T),
    n = n()
  ) %>%
  arrange(-overMaxLocalRent_med_rel) %>%
  select(
    geo_bezirk,
    overMaxLocalRent_med_rel,
    overMaxLocalRent_med,
    n
  ) %>%
  trim_vars_4_plotting()

## 2.6.1.3 .. nach Ortsteil
p2.6.1.3data <- mw_data_mietspiegel %>%
  drop_na(geo_ortsteil) %>%
  group_by(geo_ortsteil_id) %>%
  # Erstmal Generierung aller möglichen Variablen zur Kontrolle
  summarise(
    geo_ortsteil = first(geo_ortsteil),
    overBaseLocalRent_mean = mean(cst_overBaseLocalRent, na.rm = T),
    overMaxLocalRent_mean = mean(cst_overMaxLocalRent, na.rm = T),
    overBaseLocalRent_mean_rel = mean(cst_overBaseLocalRent_rel, na.rm = T),
    overMaxLocalRent_mean_rel = mean(cst_overMaxLocalRent_rel, na.rm = T),
    overBaseLocalRent_med = median(cst_overBaseLocalRent, na.rm = T),
    overMaxLocalRent_med = median(cst_overMaxLocalRent, na.rm = T),
    overBaseLocalRent_med_rel = median(cst_overBaseLocalRent_rel, na.rm = T),
    overMaxLocalRent_med_rel = median(cst_overMaxLocalRent_rel, na.rm = T),
    n = n()
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(-overMaxLocalRent_med_rel)

### 2.6.1.3 export csv
# Define Threshold number of offers for each Ortsteil to be NA
min_anzahl_angebote_pro_ortsteil <- 50

p2.6.1.3data %>%
  select(
    geo_ortsteil,
    overMaxLocalRent_med_rel,
    overMaxLocalRent_med,
    n
  ) %>%
  filter(n > min_anzahl_angebote_pro_ortsteil) %>%
  trim_vars_4_plotting()

p2.6.1.3data <- p2.6.1.3data %>%
  # Wir setzen die Werte auf NA falls es weniger als min_anzahl_angebote_pro_ortsteil gibt
  mutate(
    overMaxLocalRent_med = ifelse(n < min_anzahl_angebote_pro_ortsteil,
      NA, overMaxLocalRent_med
    ),
    overMaxLocalRent_med_rel = ifelse(n < min_anzahl_angebote_pro_ortsteil,
      NA, overMaxLocalRent_med_rel
    ),
    n = ifelse(n < min_anzahl_angebote_pro_ortsteil,
      NA, n
    )
  ) %>%
  select(
    geo_ortsteil_id,
    geo_ortsteil,
    overMaxLocalRent_med_rel,
    overMaxLocalRent_med,
    n
  ) %>%
  arrange(geo_ortsteil_id)

p2.6.1.3data %>%
  trim_vars_4_plotting() %>%
  write_csv(path = paste0(
    websitePath,
    "2.6.1.3_Ueberschreitung_Mietspiegel_nach_Ortsteil.csv"
  ))

### nach Baujahr
# Für Gebäude die zwischen 1950 und 1970 gebaut wurden:
overMaxLocalRent_50_70_rel <- mw_data_mietspiegel %>%
  filter(obj_baujahr %within% interval(
    ymd("1950-01-01"),
    ymd("1970-01-01")
  )) %>%
  summarise(overMaxLocalRent_50_70_rel = median(cst_overMaxLocalRent_rel)) %>%
  pull(overMaxLocalRent_50_70_rel) %>%
  percent()

### mw_data_mietbremse
mw_data_mietbremse <- mw_data_mietspiegel %>%
  # Mietpreisbremse zieht nicht bei Erstbezug und bei Vollsanierung.
  # Baujahr älter als 01.01.2018 wurde schon oben gefiltert bei mw_data_mietspiegel
  filter(!obj_zustand %in% c(
    "Erstbezug",
    "Erstbezug nach Sanierung"
  ))

# Berechnung für maximal Ausstattaung der Wohnung = mietspiegelsteigernde Merkmale
overMaxMietbremse_percent <- mw_data_mietbremse %>%
  pull(cst_overMaxMietbremse_logical) %>%
  table() %>%
  prop.table() %>%
  as.vector() %>%
  scales::percent()
# overMaxMietbremse_percent

# Berechnung für durchschnittl. Ausstattaung der Wohnung
overBaseMietbremse_percent <- mw_data_mietbremse %>%
  pull(cst_overBaseMietbremse_logical) %>%
  table() %>%
  prop.table() %>%
  as.vector() %>%
  scales::percent()
# overBaseMietbremse_percent

## 2.6.2.2 nach Bezirk
p2.6.2.2data <- mw_data_mietbremse %>%
  drop_na(geo_bezirk) %>%
  group_by(geo_bezirk) %>%
  # Überschreitung auf Basis der MAXIMAL luxuriösen Ausstattung
  summarise(`Anteil Angebote teurer als Mietpreisbremse in %` = round(sum(cst_overMaxMietbremse_logical) * 100 / n(), 1)) %>%
  mutate(`Anteil Angebote günstiger als Mietpreisbremse in %` = round(100 - `Anteil Angebote teurer als Mietpreisbremse in %`, 1)) %>%
  # Hier zum Vergleich als Überschreitung auf Basis der DURCHSCHNITTLICHEN Ausstattung
  # `Anteil der Angebote welche die Mietpreisbremse überschreiten base` = sum(cst_overBaseMietbremse_logical)/n(),
  arrange(`Anteil Angebote teurer als Mietpreisbremse in %`)

p2.6.2.2data_berlin <- mw_data_mietbremse %>%
  # Überschreitung auf Basis der MAXIMAL luxuriösen Ausstattung
  summarise(`Anteil Angebote teurer als Mietpreisbremse in %` = round(sum(cst_overMaxMietbremse_logical) * 100 / n(), 1)) %>%
  mutate(`Anteil Angebote günstiger als Mietpreisbremse in %` = round(100 - `Anteil Angebote teurer als Mietpreisbremse in %`, 1)) %>%
  # Hier zum Vergleich als Überschreitung auf Basis der DURCHSCHNITTLICHEN Ausstattung
  # `Anteil der Angebote welche die Mietpreisbremse überschreiten base` = sum(cst_overBaseMietbremse_logical)/n(),
  arrange(`Anteil Angebote teurer als Mietpreisbremse in %`) %>%
  mutate(geo_bezirk = "Berlin") %>%
  select(3, 1, 2)

p2.6.2.2data <- bind_rows(
  p2.6.2.2data,
  p2.6.2.2data_berlin
) %>%
  arrange(`Anteil Angebote teurer als Mietpreisbremse in %`)

### 2.6.2.2 export csv
p2.6.2.2data %>%
  trim_vars_4_plotting() %>%
  rename(
    "Anteil Angebote teurer als Mietpreisbremse" = "Anteil Angebote teurer als Mietpreisbremse in %",
    "Anteil Angebote günstiger als Mietpreisbremse" = "Anteil Angebote günstiger als Mietpreisbremse in %"
  ) %>%
  write_csv(path = paste0(
    websitePath,
    "2.6.2.2_Anteil_Angebote_ueber_Mietpreisbremse_nach_Bezirk.csv"
  ))

### 2.6.3.2 Preis versus Baujahr
# set first year for baujahr
start_year <- as.Date("1881-01-01")

p2.6.3.2data <- mw_data %>%
  # dupliziere Angebote für neue Eigentümerkategorie: Gesamt
  bind_rows(mw_data) %>%
  mutate(Besitz = c(
    ifelse(mw_data$is_anbieter_landeseigene, "Städtisch", "Privat"),
    rep("alle Eigentümer", length.out = nrow(mw_data))
  )) %>%
  filter(!is.na(obj_baujahr_10y) & obj_baujahr >= start_year) %>%
  trim_vars_4_plotting() %>%
  select(`Baujahr aggregiert (10 J.)`, Besitz, `Nettokaltmiete in €/m²`) %>%
  rename(Eigentümer = Besitz) %>%
  # mean für Baujahr aggregiert (10 J.) und anbieter
  group_by(Eigentümer, `Baujahr aggregiert (10 J.)`) %>%
  summarise(
    `Nettokaltmiete in €/m²` = round(mean(`Nettokaltmiete in €/m²`), 2),
    n = n()
  ) %>%
  filter(Eigentümer != "alle Eigentümer")

### 2.6.3.2 export csv
p2.6.3.2data %>%
  select(-n) %>%
  write_csv(path = paste0(websitePath, "2.6.3.2_lineplot_neubau.csv"))

### 2.6.3.3 Heatmap Nettokaltmieten Bezirk vs Baujahr
p2.6.3.3data_raw <-
  mw_data %>%
  select(cst_nettokalt_sqm, geo_bezirk, obj_baujahr, obj_baujahr_10y) %>%
  filter(!is.na(geo_bezirk) & obj_baujahr >= start_year) %>%
  ungroup()

p2.6.3.3data <- p2.6.3.3data_raw %>%
  mutate(geo_bezirk = as.character(geo_bezirk)) %>% # to char for later merge with "Berlin gesamt"
  group_by(geo_bezirk, obj_baujahr_10y) %>%
  summarise(
    cst_nettokalt_sqm = median(cst_nettokalt_sqm, na.rm = T),
    Angebotszahl = n()
  ) %>%
  # duplicate rows to add data for whole Berlin
  bind_rows(
    p2.6.3.3data_raw %>%
      group_by(obj_baujahr_10y) %>%
      summarise(
        cst_nettokalt_sqm = median(cst_nettokalt_sqm, na.rm = T),
        Angebotszahl = n()
      ) %>%
      mutate(geo_bezirk = "Berlin Gesamt")
  ) %>%
  # complete missing combinations of Bezirk and Year
  mutate(obj_baujahr_10y = fct_drop(obj_baujahr_10y)) %>% # drop unused factor levels to avoid complete() including them two
  complete(geo_bezirk, obj_baujahr_10y = unique(.$obj_baujahr_10y)) %>%
  mutate(
    # now remove entries with < 100 offers
    cst_nettokalt_sqm = ifelse(Angebotszahl < 100, NA, cst_nettokalt_sqm)
  ) %>%
  filter(!is.na(obj_baujahr_10y)) %>%
  filter(geo_bezirk != "Berlin Gesamt") %>%
  arrange(desc(geo_bezirk)) %>%
  trim_vars_4_plotting()

### 2.6.3.3
p2.6.3.3data %>%
  select(-Angebotszahl) %>%
  write_csv(path = paste0(websitePath, "2.6.3.3_heatmap_neubau.csv"))

