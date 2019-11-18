##############################################
#
# Chapter 1 `Leistbarkeit`
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
  odbc,
  RMySQL,
  R.utils,
  validate,
  enc,
  lubridate,
  validate,
  gghighlight,
  stringdist,
  RecordLinkage,
  knitr,
  ggmap,
  rgdal,
  sp,
  rgeos,
  sf,
  mapview,
  randomForest,
  RColorBrewer,
  lubridate,
  readxl,
  mgcv,
  viridis,
  qwraps2
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

# relative path to website-repo for csv-export
websitePath <- "../mw-frontend/static/visualization-data/"

# Um conflicts vorzubeugen
select <- dplyr::select

#### ANALYSIS ####
#+++++++++++++++++

### 1.4.4 stats
# create vector with % of flats affordable per household-size ###
net_income <- c(`1Pers` = 1375, `2Pers` = 2625, `3Pers` = 3075, `4Pers` = 3425, `5Pers+` = 3000)

# https://www.statistik-berlin-brandenburg.de/publikationen/stat_berichte/2017/SB_A01-11-00_2016j01_BE.pdf Seite 49
# Daten vom Mikrozensusbericht 2016. Der aktuellere Bericht 2017 enthält keine Aufschlüsselung des Einkommens nach Haushaltsgröße mehr.
# [ Mikrozensus 2017: https://www.statistik-berlin-brandenburg.de/publikationen/stat_berichte/2018/SB_A01-10-00_2017j01_BE.pdf ]

# indicate offers with suitable n of rooms
is.suitabl <- tbl_df(mw_data) %>%
  select(geo_bezirk, cst_gesamtmiete_sqm, cst_gesamtmiete, obj_zimmer, obj_wohnflaeche) %>%
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
  # reshape to long format for later conversion
  pivot_longer(names_to = "Personen im Haushalt", cols = c(6:10), values_to = "is.suit") %>%
  mutate(`Personen im Haushalt` = as.numeric(`Personen im Haushalt`)) # to use variable name as index

### 1.4.5 calculate affordable area
is.affdbl.area <- is.suitabl %>%
  # first for bezirke
  group_by(geo_bezirk, `Personen im Haushalt`) %>%
  dplyr::summarise(
    `Leistbare Fläche` = 0.3 * net_income[unique(`Personen im Haushalt`)] / median(cst_gesamtmiete_sqm [is.suit]),
    # anzahl angebote für den Fall von unsicherer datenlage
    n = sum(is.suit)
  ) %>%
  # then also for whole Berlin, binding rows
  bind_rows(
    is.suitabl %>%
      group_by(`Personen im Haushalt`) %>%
      # compute affordable areas for each household-size according to cost per square meter
      summarise(
        `Leistbare Fläche` = (0.3 * net_income[unique(`Personen im Haushalt`)]) / median(cst_gesamtmiete_sqm [is.suit]),
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
    }, .desc = TRUE)
  ) %>%
  trim_vars_4_plotting()

### 1.4.5 export csv
is.affdbl.area %>%
  select(-`Anzahl der Angebote`) %>%
  # wide for export
  spread(
    key = `Personen im Haushalt`,
    value = `Leistbare Fläche`
  ) %>%
  rename(
    `1 Person` = "1",
    `2 Personen` = "2",
    `3 Personen` = "3",
    `4 Personen` = "4",
    `5 Personen und mehr` = "5"
  ) %>%
  arrange(-`1 Person`) %>%
  write_csv(path = paste0(websitePath, "1.4.5_leistbare_wohnflaeche_dottplot.csv"))

### 1.4.9 calculate affordable offers for 1 Pers hh
is.affdbl.1p <- is.suitabl %>%
  filter(`Personen im Haushalt` == 1) %>%
  # first for bezirke
  group_by(geo_bezirk) %>%
  summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income[unique(`Personen im Haushalt`)]) / sum(is.suit)) %>%
  # then also for whole Berlin, binding rows
  bind_rows(
    is.suitabl %>%
      ungroup() %>%
      filter(`Personen im Haushalt` == 1) %>%
      summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income[unique(`Personen im Haushalt`)]) / sum(is.suit)) %>%
      mutate(geo_bezirk = rep("Berlin", times = length(`Leistbare Wohnungen`)))
  ) %>%
  mutate(
    # add variable nicht leistbar for stacked barplot
    `Nicht-leistbare Wohnungen` = 100 - `Leistbare Wohnungen`,
  ) %>%
  # round
  mutate_if(is.numeric, round, 1) %>%
  trim_vars_4_plotting()

bez.labels <- levels(is.affdbl.1p$Bezirk)

### 1.4.9 export csv
is.affdbl.1p %>%
  arrange(`Nicht-leistbare Wohnungen`) %>%
  write_csv(path = paste0(websitePath, "1.4.9_1-pers_hh_barchart.csv"))

### p1.4.9_stats
# stats for affordability before Leistbare Wohnfläche Durchschnittshaushalte
is.suitabl %>%
  filter(`Personen im Haushalt` == 1 & is.suit) %>%
  # a.) first for bezirke
  group_by(geo_bezirk) %>%
  summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
  # b.) for whole Berlin, binding rows
  bind_rows(
    is.suitabl %>%
      ungroup() %>%
      filter(`Personen im Haushalt` == 1 & is.suit) %>%
      summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
      mutate(geo_bezirk = rep("Berlin", times = length(`Leistbare Wohnungen`)))
  ) %>%
  # add variable nicht leistbar for stacked barplot
  mutate(`Nicht-leistbare Wohnungen` = 100 - `Leistbare Wohnungen`) %>%
  # NAs come from offers without Bezirk that were included in whole Berlin
  drop_na(geo_bezirk) %>%
  trim_vars_4_plotting()

### 1.5.4 offers affordable with kdu
# load KDU data
kdu <- c(
  `1Pers` = 472,
  `2Pers` = 553,
  `3Pers` = 713,
  `4Pers` = 802,
  `5Pers` = 933
)

# Load Kosten der Unterkunft data from
# --> https://www.berlin.de/jobcenter-tempelhof-schoeneberg/leistungsbereich/unterkunft-und-heizung/artikel.394407.php
# As heating type we assume oil - within oil we take the highest cost to stay conservative (area 100-250)

is.affdbl.kdu <- tbl_df(mw_data) %>%
  select(geo_bezirk, cst_gesamtmiete, obj_zimmer, geo_sbahn, is_eingestellt) %>%
  filter(!is.na(geo_bezirk)) %>%
  mutate(
    affdbl_1p = obj_zimmer >= 1 & obj_zimmer <= 2 & cst_gesamtmiete <= kdu["1Pers"], # is flat affordable?
    affdbl_2p = obj_zimmer >= 2 & obj_zimmer <= 3 & cst_gesamtmiete <= kdu["2Pers"],
    affdbl_3p = obj_zimmer >= 3 & obj_zimmer <= 4 & cst_gesamtmiete <= kdu["3Pers"],
    affdbl_4p = obj_zimmer >= 4 & obj_zimmer <= 5 & cst_gesamtmiete <= kdu["4Pers"],
    affdbl_5p = obj_zimmer >= 5 & obj_zimmer <= 6 & cst_gesamtmiete <= kdu["5Pers"]
  )

# by bezirk
is.affdbl.kdu.bez <- is.affdbl.kdu %>%
  group_by(geo_bezirk) %>%
  summarise(
    n_affdbl_1p = sum(affdbl_1p), # n. of flats affordable
    n_affdbl_2p = sum(affdbl_2p),
    n_affdbl_3p = sum(affdbl_3p),
    n_affdbl_4p = sum(affdbl_4p),
    n_affdbl_5p = sum(affdbl_5p),
    perc_affdble_1p = 100 * n_affdbl_1p / sum(obj_zimmer >= 1 & obj_zimmer <= 2), # % of flats affordable
    perc_affdble_2p = 100 * n_affdbl_2p / sum(obj_zimmer >= 2 & obj_zimmer <= 3),
    perc_affdble_3p = 100 * n_affdbl_3p / sum(obj_zimmer >= 3 & obj_zimmer <= 4),
    perc_affdble_4p = 100 * n_affdbl_4p / sum(obj_zimmer >= 4 & obj_zimmer <= 5),
    perc_affdble_5p = 100 * n_affdbl_5p / sum(obj_zimmer >= 5 & obj_zimmer <= 6),
  )
# by Sbahnring
is.affdbl.kdu.sbahn <- is.affdbl.kdu %>%
  group_by(geo_sbahn) %>%
  summarise(
    n_affdbl_1p = sum(affdbl_1p), # n. of flats affordable
    n_affdbl_2p = sum(affdbl_2p),
    n_affdbl_3p = sum(affdbl_3p),
    n_affdbl_4p = sum(affdbl_4p),
    n_affdbl_5p = sum(affdbl_5p),
    perc_affdble_1p = 100 * n_affdbl_1p / sum(obj_zimmer >= 1 & obj_zimmer <= 2), # % of flats affordable
    perc_affdble_2p = 100 * n_affdbl_2p / sum(obj_zimmer >= 2 & obj_zimmer <= 3),
    perc_affdble_3p = 100 * n_affdbl_3p / sum(obj_zimmer >= 3 & obj_zimmer <= 4),
    perc_affdble_4p = 100 * n_affdbl_4p / sum(obj_zimmer >= 4 & obj_zimmer <= 5),
    perc_affdble_5p = 100 * n_affdbl_5p / sum(obj_zimmer >= 5 & obj_zimmer <= 6),
  )

# melt dataframe for plotting
is.affdbl.kdu.bez.m <- gather(is.affdbl.kdu.bez, key = "rooms", value = "perc_affdbl", starts_with("perc_"))

### 1.5.4_export
is.affdbl.kdu.sbahn.m <- gather(is.affdbl.kdu.sbahn, key = `Personen im Haushalt`, value = "Leistbare Wohnungen in %", starts_with("perc_")) %>%
  mutate(
    `Personen im Haushalt` = substr(`Personen im Haushalt`, 14, 14),
    `Leistbare Wohnungen in %` = round(`Leistbare Wohnungen in %`, 1)
  ) %>%
  # mutate(geo_sbahn = factor( geo_sbahn, levels=c("ausserhalb", "innerhalb") )) %>%
  select(-starts_with("n_")) %>%
  trim_vars_4_plotting() %>%
  # reorder
  select(c(2, 3, 1)) %>%
  write_csv(path = paste0(websitePath, "1.5.4_leistbare_wohnungen_barchart.csv"))

### 1.5.7 Number of offers for KDU
# filter letzte 365 Tage
startDate <- max(is.affdbl.kdu$is_eingestellt) - 364

# filter last 12 months and add week for agregation
is.affdbl.kdu.t.w <- is.affdbl.kdu %>%
  filter(is_eingestellt >= startDate) %>%
  mutate(
    Woche = week(is_eingestellt), # data as character to create identical week
    geo_bezirk = as.character(geo_bezirk)
  ) %>% # to char to avoid completion of all factor levels when using complete()
  rename(Bezirk = geo_bezirk)

# summarise for Bezirke
is.affdbl.kdu.t <- is.affdbl.kdu.t.w %>%
  # Bezirke
  group_by(Bezirk, Woche) %>%
  summarise(`Anzahl Angebote pro Woche` = sum(affdbl_2p)) %>%
  # now complete missing combinations of Bezirk and Week and fill missing rows with 0 for offers.
  # (missign rows are actually weeks with zero offers)
  complete(Bezirk, Woche = 1:53, fill = list(`Anzahl Angebote pro Woche` = 0)) %>% # complete needs manual definition in case of numeric values
  filter(Bezirk %in% c("Friedrichshain-Kreuzberg", "Neukölln")) %>%
  mutate(Datum = startDate + weeks(Woche - 1)) %>%
  select(-Woche)

### 1.5.7 save csv
is.affdbl.kdu.t %>%
  spread(key = Bezirk, value = `Anzahl Angebote pro Woche`) %>%
  write_csv(path = paste0(websitePath, "1.5.7_exkurs_kdu_2pers_lineplot.csv"))

### 1.6.3 Heatmaps Verdrängung: Mietenhöhen, Prekarität, Verdrängungsindex
# compute discrete indices
indics <- indics_displace()

# 1.6.3 save csv for website
indics %>%
  st_set_geometry(NULL) %>% # stripping from spatial attribs
  select(-7, -8, -9, -10) %>%
  trim_vars_4_plotting() %>%
  # select(-sbahn_ring, )
  write_csv(path = paste0(websitePath, "1.6.3_Verdraengungskarten.csv"))
