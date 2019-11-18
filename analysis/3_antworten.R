##############################################
#
# Chapter 3 `Antworten`
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
  ggmosaic,
  klaR,
  cowplot,
  data.table
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

### 3.2.4 Berechnung analog zu dottplot in 1.4.5
# vector for income
net_income <- c(`1Pers` = 1375, `2Pers` = 2625, `3Pers` = 3075, `4Pers` = 3425, `5Pers` = 3000)

# Daten vom Mikrozensusbericht 2016.
# https://www.statistik-berlin-brandenburg.de/publikationen/stat_berichte/2017/SB_A01-11-00_2016j01_BE.pdf Seite 49

# indicate offers with suitable n of rooms
is.suitabl <- tbl_df(mw_data) %>%
  drop_na(cst_md_gesamtmiete) %>%
  select(geo_bezirk, cst_md_gesamtmiete_sqm, cst_md_gesamtmiete, obj_zimmer, obj_wohnflaeche) %>%
  mutate(
    `1` = obj_zimmer >= 1 & obj_zimmer <= 2, # is flat suitable?
    `2` = obj_zimmer >= 2 & obj_zimmer <= 3,
    `3` = obj_zimmer >= 3 & obj_zimmer <= 4,
    `4` = obj_zimmer >= 4 & obj_zimmer <= 5,
    `5` = obj_zimmer >= 5 & obj_zimmer <= 6
  ) %>%
  # remove unrealistically large areas with regard to room number
  filter(!obj_wohnflaeche < obj_zimmer * 10) %>%
  filter(!is.na(geo_bezirk)) %>%
  pivot_longer(names_to = "Personen im Haushalt", cols = c(6:10), values_to = "is.suit") %>%
  mutate(`Personen im Haushalt` = as.numeric(`Personen im Haushalt`)) # to use variable name as index

## calculate affordable area
is.affdbl.area <- is.suitabl %>%
  # first for bezirke
  group_by(geo_bezirk, `Personen im Haushalt`) %>%
  dplyr::summarise(
    `Leistbare Fläche` = 0.3 * net_income[unique(`Personen im Haushalt`)] / median(cst_md_gesamtmiete_sqm [is.suit],
      na.rm = TRUE
    ),
    # anzahl angebote für den Fall von unsicherer datenlage
    n = sum(is.suit)
  ) %>%
  # then also for whole Berlin, binding rows
  bind_rows(
    is.suitabl %>%
      group_by(`Personen im Haushalt`) %>%
      summarise(
        `Leistbare Fläche` = (0.3 * net_income[unique(`Personen im Haushalt`)]) / median(cst_md_gesamtmiete_sqm [is.suit],
          na.rm = TRUE
        ),
        n = sum(is.suit, na.rm = T)
      ) %>%
      mutate(geo_bezirk = rep("Berlin", times = length(`Leistbare Fläche`)))
  ) %>%
  ungroup() %>%
  mutate(
    geo_bezirk = factor(geo_bezirk),
    `Leistbare Fläche` = ifelse(n <= 100, NA, `Leistbare Fläche`),
    `Leistbare Fläche` = round(`Leistbare Fläche`, 1),
    # relevel factors for plot-order
    geo_bezirk = fct_reorder2(geo_bezirk, `Leistbare Fläche`, `Personen im Haushalt`, function(leist, pers) {
      unique(leist[pers == 1])
    })
  ) %>%
  trim_vars_4_plotting()

### 3.2.4 export csv
is.affdbl.area %>%
  select(-`Anzahl der Angebote`) %>%
  spread(key = `Personen im Haushalt`, value = `Leistbare Fläche`) %>%
  rename(
    `1 Person` = "1",
    `2 Personen` = "2",
    `3 Personen` = "3",
    `4 Personen` = "4",
    `5 Personen und mehr` = "5"
  ) %>%
  arrange(-`1 Person`) %>%
  mutate_if(is.numeric, round, 1) %>%
  write_csv(path = paste0(websitePath, "3.2.4_leistbare_wohnflaeche_mietendeckel_dottplot.csv"))

### 3.2.5 Vergleich der Angebotsmieten mit und ohne Mietendeckel
# Hier die Grenzen der Mietendeckel Altersklassen
md_klasse_breaks <- c(
  min(year(mw_data$obj_baujahr), na.rm = TRUE),
  1919, 1950, 1965, 1973, 1991, 2003, 2013,
  year(Sys.Date())
)

# Wir rechnen hier mit dem arithm. Mittel, nicht mit dem Median - weil sonst
# die durchschnitte die Modernisierungsumlagen nicht berücksichtigen...

p3.2.5data <- mw_data %>%
  drop_na(obj_baujahr) %>%
  filter(year(obj_baujahr) <= year(Sys.Date())) %>%
  mutate(obj_baujahr_md_klasse = cut(year(obj_baujahr),
    breaks = md_klasse_breaks,
    labels = c(
      "bis 1918",
      "1919-1949",
      "1950-1964",
      "1965-1972",
      "1973-1990",
      "1991-2002",
      "2003-2013",
      "ab 2014"
    ),
    include.lowest = TRUE,
    ordered_result = TRUE,
    right = TRUE,
    dig.lab = 4
  )) %>%
  group_by(obj_baujahr_md_klasse) %>%
  summarise(
    cst_nettokalt_sqm = median(cst_nettokalt_sqm),
    cst_nebenkosten_sqm = median(cst_nebenkosten_sqm, na.rm = T),
    cst_md_nettokalt_sqm = median(cst_md_nettokalt_sqm, na.rm = T),
    # Nebenkosten nach Mietendeckel sind identisch zu vor Mietendeckel
    cst_md_nebenkosten_sqm = median(cst_nebenkosten_sqm, na.rm = T)
  )

# Ohne Nebenkosten für Website
p3.2.5data_small <- p3.2.5data %>%
  select(-starts_with("cst_nebenkosten")) %>%
  select(-starts_with("cst_md_nebenkosten"))

p3.2.5data_small


### 3.2.5 Berliner Durchschnitte ausrechnen
p3.2.5data_berlin <- mw_data %>%
  drop_na(obj_baujahr) %>%
  filter(year(obj_baujahr) <= year(Sys.Date())) %>%
  summarise(
    cst_nettokalt_sqm = mean(cst_nettokalt_sqm),
    cst_nebenkosten_sqm = mean(cst_nebenkosten_sqm, na.rm = T),
    cst_md_nettokalt_sqm = mean(cst_md_nettokalt_sqm, na.rm = T),
    # Nebenkosten nach Mietendeckel sind identisch zu vor Mietendeckel
    cst_md_nebenkosten_sqm = mean(cst_nebenkosten_sqm, na.rm = T)
  )

## Berlin gesamt
p3.2.5data_berlin %>%
  trim_vars_4_plotting() %>%
  rename(
    `Nettokaltmiete ohne Mietendeckel in €/m²` = "Nettokaltmiete in €/m²",
    `Nebenkosten ohne Mietendeckel in €/m²` = "Nebenkosten in €/m²"
  )

### 3.2.5 Nach Baujahresklassen
p3.2.5data_export <- p3.2.5data_small %>%
  trim_vars_4_plotting() %>%
  rename(`Nettokaltmiete ohne Mietendeckel` = "Nettokaltmiete in €/m²") %>%
  rename("Nettokaltmiete mit Mietendeckel" = "Nettokaltmiete mit Mietendeckel in €/m²")

p3.2.5data_export %>%
  write_csv(path = paste0(websitePath, "3.2.5_md_ohne_mit_mietendeckel.csv"))

# Enteignung nach Forderung des Volksentscheids "Deutsche Wohnen & co. enteignen!"
### anteil_exprop_am_datensatz
anteil_exprop_an_mw_data <- mw_data %>%
  pull(is_anbieter_is_exprop) %>%
  table() %>%
  prop.table() %>%
  round(3)

# Nicht betrachtet werden folgende Firmen, sie sind nicht im Datensatz
other_exprop <- c("TAG", "IMW", "Gropius", "DVI", "Pears")
pattern_exprop <- paste(other_exprop,
  collapse = "|"
)

## pattern_search_other_firms
grep(pattern_exprop,
  mw_data$is_anbieter,
  ignore.case = TRUE,
  value = TRUE
)

### 3.3.2 Geolocations aller Wohnungen die enteignet werden sollen:
# (nicht gejitterd weil es keine zoom möglichkeit gibt)
p3.3.2data <- mw_data %>%
  filter(is_anbieter_is_exprop) %>%
  select(
    is_anbieter_exprop,
    geo_lat,
    geo_lon
  )

### 3.3.2 export csv
p3.3.2data %>%
  write_csv(path = paste0(websitePath, "3.3.2_Bestaende_zu_enteignen_staticmap.csv"))

### 3.3.4 Nettokaltmiete in den Beständen der zu enteignenden Firmen
# Szenario: 8,3 Mrd € Entschädigung - 3,70€/m² Nettokaltmiete
p3.3.4data <- mw_data %>%
  filter(is_anbieter_is_exprop) %>%
  # --> 3.70 € Nettokaltmiete bei Enteignung
  mutate(cst_ex_nettokalt_sqm = 3.70) %>%
  summarise(
    cst_nettokalt_sqm = mean(cst_nettokalt_sqm, na.rm = T),
    cst_md_nettokalt_sqm = mean(cst_md_nettokalt_sqm, na.rm = T),
    cst_ex_nettokalt_sqm = mean(cst_ex_nettokalt_sqm, na.rm = T),
    cst_km_kostdeckend_nettokalt_sqm = mean(cst_km_kostdeckend_nettokalt_sqm, na.rm = T)
  ) %>%
  mutate_if(is.numeric, round, 2)

# transpose
p3.3.4data <- tibble(
  Szenario = names(p3.3.4data),
  `Nettokaltmiete in €/m²` = (p3.3.4data %>%
    slice(1) %>%
    c(., recursive = TRUE) %>%
    unname())
)

# Umbenennen für Export
p3.3.4data_export <- p3.3.4data %>%
  mutate(Szenario = factor(Szenario)) %>%
  mutate(Szenario = fct_recode(Szenario,
    `aktuelle Nettokaltmiete` = "cst_nettokalt_sqm",
    `Nettokaltmiete mit Mietendeckel` = "cst_md_nettokalt_sqm",
    `Nettokaltmiete nach Enteignung` = "cst_ex_nettokalt_sqm",
    `kostendeckende Nettokaltmiete` = "cst_km_kostdeckend_nettokalt_sqm"
  )) %>%
  arrange(`Nettokaltmiete in €/m²`)

### 3.3.4 export csv
p3.3.4data_export %>%
  write_csv(path = paste0(websitePath, "3.3.4_nettokaltmiete_szenariovergleich.csv"))
