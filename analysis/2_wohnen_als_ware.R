####################################################################
#
# Chapter 2 'Wohnen als Ware' 
#
####################################################################

#### SETUP ####
#++++++++++++++

### libraries
if (!require("pacman")) install.packages("pacman")

library("pacman")
p_load(
  tidyverse,
  scales,
  lubridate,
  knitr,
  rgdal, rgeos, sf, mapview, 
  randomForest,
  viridis,
  treemap,
  klaR,
  jsonlite,
  gbm,
  dismo
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
sapply(
  list.files(
    pattern = "[.]R$",
    path = "functions",
    full.names = TRUE
  ),
  source
)

# Handle package conflict with MASS
select <- dplyr::select

#### Analysis ####
#+++++++++++++++++

## Wer sind die Big Players? ####

### `p2.2.2` Marktanteile am Angebotsmarkt der Top 10-Anbieter ####

## ----p2.2.2--------------------------------------------------------------
p2.2.2_data <- mw_data %>%
  mutate(
    is_anbieter_agg_anteil = round(100 * is_anbieter_agg_anzahl / nrow(mw_data), 1)
    ) %>%
  filter(
    is_anbieter_bigplayer == TRUE
    ) %>%
  group_by(is_anbieter_agg) %>%
  summarise(
    is_anbieter_agg_anteil = first(is_anbieter_agg_anteil),
    is_anbieter_landeseigene = first(is_anbieter_landeseigene)
    ) %>%
  arrange(is_anbieter_agg_anteil) %>%
  rename_recode_cols_for_export()


## ----p2.2.2_export-------------------------------------------------------
p2.2.2_data %>%
  export_csv("2.2.2_marktanteile_bigplayers.csv")



## ----p2.2.2_stats--------------------------------------------------------
# Calculate aggregate market share
marktanteil_bigplayers <- mw_data %>% 
  pull(is_anbieter_bigplayer) %>%
  table %>% 
  prop.table() %>%
  last() %>%
  percent(1)

marktanteil_bigplayers


### `p2.2.7` Verteilung der Baujahre angebotener Wohnungen, Anteile je Anbieter ####

## ----mw_data_bigplayer---------------------------------------------------
# tibble with BigPlayers only
mw_data_bigplayer <- mw_data %>%
  filter(is_anbieter_bigplayer == TRUE) %>%
  # drop unused factor levels
  mutate(is_anbieter_agg = factor(is_anbieter_agg))


## ----p2.2.7--------------------------------------------------------------
# Distribution of building age for BigPlayers
p2.2.7_data <- mw_data_bigplayer %>%
  drop_na(obj_baujahr_10y) %>%
  mutate(
    is_anbieter_agg = factor(is_anbieter_agg)
    ) %>%
  # new count of is_anbieter_agg_anzahl count cuz we dropped NAs
  add_count(is_anbieter_agg, sort = TRUE, name = "is_anbieter_agg_anzahl") %>%
  group_by(is_anbieter_agg, obj_baujahr_10y) %>%
  summarise(
    anzahl_objekte = n(),
    anteil_objekte = round(100 * (anzahl_objekte / mean(is_anbieter_agg_anzahl)), 1)
    ) %>%
  ungroup() %>%
  complete(
    is_anbieter_agg,
    obj_baujahr_10y,
    fill = list(anzahl_objekte = 0,
                anteil_objekte = 0)
    ) %>%
  # We drop offers of age 1880 and before - its just 2 offers in sum!
  filter(obj_baujahr_10y > "1870-1880")



## ----p2.2.7_export-------------------------------------------------------
p2.2.7_data %>%
  select(-anzahl_objekte) %>% 
  rename_recode_cols_for_export() %>%
  spread(
    key = `Baujahr aggregiert (10 J.)`,
    value = anteil_objekte) %>%
  export_csv("2.2.7_gebaeudealtersverteilung_bigplayers.csv")


## Wie teuer vermieten die Big Players? ####

### `p2.3.2` Anteil der Angebote nach Preiskategorie (Nettokaltmiete) und Anbieter ###

## ----p2.3.2---------------------------------------------------
# decide on inverval breaks for categories of nettokalt per sqm
nettokalt_breaks <- c(0, 7, 11, 15, 30)

p2.3.2_data <- mw_data_bigplayer %>%
  dplyr::select(is_anbieter_agg, cst_nettokalt_sqm) %>%
  mutate(
    # create nettokaltmiete categories
    cst_nettokalt_sqm_cat = cut(cst_nettokalt_sqm,
                                breaks = nettokalt_breaks,
                                ordered_result = T)
  ) %>%
  group_by(is_anbieter_agg, cst_nettokalt_sqm_cat) %>%
  summarise(n = n()) %>%
  # calculate share of each category by is_anbieter_agg
  mutate(`Anteil in %` = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Rename categories
levels(p2.3.2_data$cst_nettokalt_sqm_cat) <- c(
  "weniger als 7 €/m²",
  "7 bis 11 €/m²",
  "11 bis 15 €/m²",
  "mehr als 15 €/m²"
)

# create order of levels - descending by share of
# cheap offers (<7€)
cat_level_order <- p2.3.2_data %>%
  group_by(is_anbieter_agg) %>%
  filter(cst_nettokalt_sqm_cat == "weniger als 7 €/m²") %>%
  arrange(-`Anteil in %`) %>%
  pull(is_anbieter_agg) %>%
  as.character()

# make wide data for plotly
p2.3.2_data <- p2.3.2_data %>%
  dplyr::select(-n) %>%
  mutate(
    # reorder sellers by share of cheap (<7€) offers
    is_anbieter_agg = fct_relevel(
      is_anbieter_agg,
      cat_level_order)
  ) %>%
  arrange(is_anbieter_agg) %>%
  pivot_wider(
    id_cols = is_anbieter_agg,
    values_from = `Anteil in %`,
    names_from = cst_nettokalt_sqm_cat,
    # fill with 0 if there is NA
    values_fill = list(`Anteil in %` = 0)
  )

## ----p2.3.2_export-------------------------------------------------------
p2.3.2_data %>% 
  arrange(`weniger als 7 €/m²`) %>% 
  rename_recode_cols_for_export() %>% 
  export_csv("2.3.2_miete_kategorien_bigplayers.csv")


## ----p2.3.2_stats---------------------------------------------
# Nettokalt Akelius
nettokalt_sqm_akelius <- mw_data_bigplayer %>% 
  filter(is_anbieter_agg == "AKELIUS GmbH") %>% 
  summarise(cst_nettokalt_sqm = mean(cst_nettokalt_sqm))

nettokalt_sqm_akelius



### `p2.3.4` Nettokaltmiete & Nebenkosten nach Anbieter im Vergleich zum Rest des Mietmarkts ####

## ----p2.3.4_stats---------------------------------------------
# Nebenkosten Deutsche Wohnen & Gewobag
nebenkosten_sqm_dw_gewobag <- mw_data_bigplayer %>% 
  group_by(is_anbieter_agg) %>% 
  filter(is_anbieter_agg %in% c("Deutsche Wohnen Gruppe", "Gewobag AG")) %>% 
  summarise(cst_nebenkosten_sqm = mean(cst_nebenkosten_sqm, na.rm=T))

nebenkosten_sqm_dw_gewobag

## ----p2.3.4--------------------------------------------------------------
# Define threshold: smallest number of offers amongst Big Players
# --> we'll need this to define "other sellers" in a bit
bigplayers_anzahl_threshold <- mw_data_bigplayer %>%
  select(is_anbieter_agg_anzahl) %>%
  pull() %>%
  min()

# Data for point cloud
p2.3.4data <- mw_data %>%
  # is_anbieter_agg here with "other sellers" (="andere Anbieter") for Non-Big Players
  mutate(
    is_anbieter_agg = fct_lump_min(is_anbieter_agg,
                                   min = bigplayers_anzahl_threshold,
                                   other_level = "andere Anbieter"),
    is_anbieter_agg = fct_explicit_na(is_anbieter_agg, "andere Anbieter")
    ) %>%
  arrange(-is_anbieter_agg_anzahl) %>%
  select(
    is_anbieter_agg,
    cst_nettokalt_sqm,
    cst_nebenkosten_sqm
    ) %>%
  filter(
    # We exclude extreme outliers here, to have a decent axis window in point cloud
    # These are around 800 offers (mostly temporary offers)
  cst_nebenkosten_sqm > 0 & cst_nebenkosten_sqm < 10) 



## ----p2.3.4_export-------------------------------------------------------
p2.3.4data %>%
  rename_recode_cols_for_export() %>%
  export_csv("2.3.6_miete_vs_nebenkosten_bigplayers.csv")


## Welche Faktoren beeinflussen den Mietpreis? ####

### `p2.4.2` Wichtigkeit von Angebotsmerkmalen zur Erklärung der Nettokaltmiete pro m². ####

## ----p2.4.2--------------------------------------------------------------
# run model fitting
p2.4.2data <- fit_randomforest(mw_data)


## ----2.4.2_export--------------------------------------------------------
p2.4.2data %>% 
  mutate(Einflussgröße = recode(Einflussgröße,
                                `Nebenkosten in €/m²` = "Nebenkosten",
                                `Wohnfläche in m²` = "Wohnfläche",
                                moebliert = "Möblierung")) %>% 
  export_csv("2.4.2_wichtigkeit_random_forest.csv")


## Wie groß sind die Gewinne? ####

### `p2.5.2` Berlinweite durchschnittliche Nettokaltmiete, kostendeckende Miete und Überschussmiete pro m² ####

## ----p2.5.2data----------------------------------------------------------
# Waterfallplot for rent - cost covering rent = surplus rent
# averages as means here - we are visualizing a sum
p2.5.2data <- mw_data %>%
  # this will consequently drop all offers without obj_baujahr and any vars nescessary for cost covering rent
  drop_na(cst_km_kostdeckend_nettokalt_sqm) %>%
  # avg for whole of Berlin
  summarise(
    `Nettokaltmiete` = round(mean(cst_nettokalt_sqm, na.rm = T), 2),
    `Kostendeckende Miete` = round(mean(cst_km_kostdeckend_nettokalt_sqm, na.rm = T), 2),
    `Überschussmiete` = round(mean(cst_km_gewinnekredite_sqm, na.rm = T), 2)
    ) %>%
  rename_recode_cols_for_export()

# Data needs to be transposed for plotly
p2.5.2data <- tibble(
  Komponente = names(p2.5.2data),
  `Berliner Durchschnitt` = (p2.5.2data %>%
                               slice(1) %>%
                               c(., recursive = TRUE) %>%
                               unname())
  )


## ----p2.5.2data_export---------------------------------------------------
p2.5.2data %>%
  export_csv("2.5.2_NKM_KostendeckMiete_SurplusMiete_Waterfall.csv")


### `p2.5.4` Tabelle: Kostendeckenden Miete und Überschussmiete bei Big Players ####

## ----p2.5.4data----------------------------------------------------------
# Table only for big players
p2.5.4data <- mw_data %>%
  # this will consequently drop all offers without obj_baujahr and any vars nescessary for cost covering rent
  drop_na(cst_km_kostdeckend_nettokalt_sqm) %>%
  filter(is_anbieter_bigplayer == TRUE) %>%
  group_by(is_anbieter_agg) %>%
  summarise(
    `Nettokaltmiete in €/m²` = round(mean(cst_nettokalt_sqm, na.rm = T), 2),
    `Kostendeckende Miete in €/m²` = round(mean(cst_km_kostdeckend_nettokalt_sqm, na.rm = T), 2),
    `Überschussmiete in €/m²` = round(mean(cst_km_gewinnekredite_sqm, na.rm = T), 2)
    ) %>%
  arrange(-`Nettokaltmiete in €/m²`) %>%
  rename_recode_cols_for_export()



## ----p2.5.4data_export---------------------------------------------------
p2.5.4data %>%
  export_csv("2.5.4_NKM_KostendeckMiete_SurplusMiete_BigPlayers_Tabelle.csv")

# Data is also needed as json
p2.5.4data_json <- toJSON(p2.5.4data)
write(p2.5.4data_json,
      "../mw-frontend/static/visualization-data/2.5.4_NKM_KostendeckMiete_SurplusMiete_BigPlayers_Tabelle.json")


 
## Was wird dagegen getan? ####

### `m2.6.1.3` Überschreitung des Mietspiegels nach Ortsteil ####

## ----mw_data_mietspiegel-------------------------------------------------
# We create a tibble that only contains offers with valid mietspiegel cell assigments
mw_data_mietspiegel <- mw_data %>%
  drop_na(rid_maxLocalRent) %>% 
  filter(
    rid_maxLocalRent > 0,
    # buildings that were built in 2018 or younger are exempt from Mietspiegel
    # https://www.stadtentwicklung.berlin.de/wohnen/mietspiegel/de/geltungsbereich.shtml
    obj_baujahr < as.Date("2018-01-01"),
    # subsidized housing, too, is exempt
    obj_is_gefoerdert == FALSE
  ) %>% 
  mutate(
    is_anbieter = fct_explicit_na(is_anbieter, na_level = "keine Angaben"),
    # We assign highest "Mietspiegel" value within a "Mietspiegelfeld" to offers which are either luxurious or modernized.
    # All others are assigned "average" (mid of "Mietspiegelspanne") values (= rid_baseLocalRent)
    cst_rid_LocalRent_sqm = case_when(eqp_innen == "luxury" | obj_zustand == "modernisiert" ~ rid_maxLocalRent,
                                      TRUE ~ rid_baseLocalRent),
    cst_over_rid_LocalRent_sqm = cst_nettokalt_sqm - cst_rid_LocalRent_sqm,
    cst_over_rid_LocalRent_sqm_rel = cst_over_rid_LocalRent_sqm / cst_rid_LocalRent_sqm
  ) 

## ----m2.6.1.3_stats_a-----------------------------------------
# Citywide average: How much do the offers exceed the Mietspiegel? (only for text)
# median used here!
m2.6.1.3_data_berlin <- mw_data_mietspiegel %>%
  summarise(
    cst_over_rid_LocalRent_sqm = median(cst_over_rid_LocalRent_sqm, na.rm = T),
    cst_over_rid_LocalRent_sqm_rel = median(cst_over_rid_LocalRent_sqm_rel, na.rm = T),
    n = n()
  ) %>%
  rename_recode_cols_for_export()

m2.6.1.3_data_berlin


## ----m2.6.1.3------------------------------------------------------------
# Exceeding the Mietspiegel by Ortsteil (Quarter)
m2.6.1.3data <- mw_data_mietspiegel %>%
  drop_na(geo_ortsteil) %>%
  group_by(geo_ortsteil_id) %>%
  summarise(
    geo_ortsteil = first(geo_ortsteil),
    cst_over_rid_LocalRent_sqm = median(cst_over_rid_LocalRent_sqm, na.rm = T),
    cst_over_rid_LocalRent_sqm_rel = median(cst_over_rid_LocalRent_sqm_rel, na.rm = T),
    n = n()
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  arrange(-cst_over_rid_LocalRent_sqm_rel)


## ----m2.6.1.3_export-----------------------------------------------------
# For export we define threshold number of offers for each Ortsteil to be NA.
min_anzahl_angebote_pro_ortsteil <- 50

m2.6.1.3data <- m2.6.1.3data %>%
  # We replace all values by NA if there is less than  min_anzahl_angebote_pro_ortsteil offers in Ortsteil.
  mutate(
    cst_over_rid_LocalRent_sqm = ifelse(n < min_anzahl_angebote_pro_ortsteil,
                                        NA, cst_over_rid_LocalRent_sqm),
    cst_over_rid_LocalRent_sqm_rel = ifelse(n < min_anzahl_angebote_pro_ortsteil,
                                            NA, cst_over_rid_LocalRent_sqm_rel),
    n = ifelse(n < min_anzahl_angebote_pro_ortsteil,
               NA, n)
  ) %>%
  arrange(geo_ortsteil_id)

m2.6.1.3data %>%
  rename_recode_cols_for_export() %>%
  export_csv(
    "2.6.1.3_Ueberschreitung_Mietspiegel_nach_Ortsteil.csv"
  )

## ----m2.6.1.3_stats_b----------------------------------------------------
# Highest absolute value of exceeding Mietspiegel
m2.6.1.3data %>% 
  arrange(-cst_over_rid_LocalRent_sqm) %>% 
  slice(1)

# Highest relative value of exceeding Mietspiegel
m2.6.1.3data %>% 
  arrange(-cst_over_rid_LocalRent_sqm_rel) %>% 
  slice(1)



## ----m2.6.1.3_stats_c----------------------------------------------------
# Looking at the data, buildings of construction year 1800-1918 seem to have highest value of exceeding Mietspiegel
# This is the category of "Mietspiegel" for oldest buildings --> "Altbau"
cst_over_rid_LocalRent_sqm_1800_1918_rel <- mw_data_mietspiegel %>%
  filter(
    obj_baujahr %within% interval(
      ymd("1800-01-01"),
      ymd("1918-12-31")
    )
  ) %>%
  summarise(cst_over_rid_LocalRent_sqm_1800_1918_rel = median(cst_over_rid_LocalRent_sqm_rel, na.rm = TRUE)) %>%
  pull(cst_over_rid_LocalRent_sqm_1800_1918_rel) %>%
  percent()

cst_over_rid_LocalRent_sqm_1800_1918_rel


## Die Mietpreisbremse? Zieht nicht. ####

### `p2.6.2.2` Anteil der Angebote, der die Kappungsgrenzen der Mietpreisbremse über- oder unterschritt. ####

## ----mw_data_mietbremse--------------------------------------------------
# We create a tibble that only contains offers valid for Mietpreisbremse
mw_data_mietbremse <- mw_data %>%
  # where there is no valid Mietspiegel, no Mietpreisbremse can be calculated
  drop_na(rid_maxLocalRent) %>% 
  filter(
    rid_maxLocalRent > 0,
    # for buildings that were built in 2018 or younger we assume they are rented out for the first time
    obj_baujahr < as.Date("2018-01-01"),
    # We also filter via "Erstbezug" to be sure and to also exlclude modernizations in between contracts 
    !obj_zustand %in% c("Erstbezug", "Erstbezug nach Sanierung"),
    # subsidized housing, too, is exempt
    obj_is_gefoerdert == FALSE
  ) %>% 
  mutate(
    is_anbieter = fct_explicit_na(is_anbieter, na_level = "keine Angaben"),
    # We assign highest "Mietspiegel" value within a "Mietspiegelfeld" to offers which are either luxurious or modernized.
    # All others are assigned "average" (mid of "Mietspiegelspanne") values (= rid_baseLocalRent)
    cst_rid_LocalRent_sqm = case_when(eqp_innen == "luxury" | obj_zustand == "modernisiert" ~ rid_maxLocalRent,
                                      TRUE ~ rid_baseLocalRent),
    cst_over_mietbremse = cst_nettokalt_sqm - (cst_rid_LocalRent_sqm * 1.1), # Mietspiegel * 110% is threshold
    cst_over_mietbremse_logical = cst_over_mietbremse > 0
  ) 

## ----p2.6.2.2_stats------------------------------------------------------
# Summary stat for whole of Berlin: Share of offers exceeding Mietpreisbremse Kappungsgrenze
overMaxMietbremse_percent <- mw_data_mietbremse %>%
  pull(cst_over_mietbremse_logical) %>%
  table() %>% 
  prop.table() %>% 
  .["TRUE"] %>% # filter value for which exceeding Mietpreisbremse is TRUE
  as.vector() %>% 
  scales::percent()

overMaxMietbremse_percent


## ----p2.6.2.2------------------------------------------------------------
### By Borough (Bezirk)

p2.6.2.2_data_bezirke <- mw_data_mietbremse %>%
  # we have to drop all offers which dont have a valid geo_bezirk
  drop_na(geo_bezirk) %>%
  group_by(geo_bezirk) %>%
  summarise(`Teurer als Mietpreisbremse` = round(sum(cst_over_mietbremse_logical) * 100 / n(), 1)) %>%
  mutate(`Günstiger als Mietpreisbremse` = round(100 - `Teurer als Mietpreisbremse`, 1)) 

### Whole of Berlin

p2.6.2.2_data_berlin <- mw_data_mietbremse %>%
  summarise(`Teurer als Mietpreisbremse` = round(sum(cst_over_mietbremse_logical) * 100 / n(), 1)) %>%
  mutate(`Günstiger als Mietpreisbremse` = round(100 - `Teurer als Mietpreisbremse`, 1)) %>%
  mutate(geo_bezirk = "Berlin") %>%
  select(geo_bezirk, 
         `Teurer als Mietpreisbremse`, 
         `Günstiger als Mietpreisbremse`)

### Bind boroughs and whole of Berlin

p2.6.2.2_data <- bind_rows(
  p2.6.2.2_data_bezirke,
  p2.6.2.2_data_berlin) %>%
  arrange(`Teurer als Mietpreisbremse`)  %>% 
  rename_recode_cols_for_export() 


## ----p2.6.2.2_export------------------------------------------
p2.6.2.2_data %>%
  export_csv("2.6.2.2_Anteil_Angebote_ueber_Mietpreisbremse_nach_Bezirk.csv")


## Bauen, bauen, bauen? Hilft nicht. ####

### ` p.2.6.3.2` Durchschnittliche Nettokaltmieten je Baujahrsdekade nach Eigentümer in €/m². ####

## ---- p.2.6.3.2----------------------------------------------------------
# set first year for baujahr
# (there is only a few offers with construction year before 1881,
# but that is too little to group by decade by borough on plot after. 
# For consistency we cut off at 1881 here.)
start_year <- as.Date("1881-01-01")

p2.6.3.2_data <- mw_data %>%
  # also kick out offers without valid construction year class
  filter(
    !is.na(obj_baujahr_10y),
    obj_baujahr >= start_year
    ) %>%
  select(
    obj_baujahr_10y, 
    is_anbieter_landeseigene, 
    cst_nettokalt_sqm
    ) %>%
  group_by(
    is_anbieter_landeseigene, 
    obj_baujahr_10y
    ) %>%
  summarise(
    `Nettokaltmiete in €/m²` = round(mean(cst_nettokalt_sqm), 2),
    n = n()
    )


## ----p2.6.3.2_export-----------------------------------------------------
p2.6.3.2_data %>%
  rename_recode_cols_for_export() %>% 
  # kick out number of offers
  select(-`Anzahl der Angebote`) %>%
  export_csv("2.6.3.2_lineplot_neubau.csv")


### `p2.6.3.3` Durchschnittliche Nettokaltmiete je Baujahrsdekade nach Bezirken in €/m². ####

## ----p2.6.3.3------------------------------------------------------------
# Heatmap of rents 

p2.6.3.3_data <- mw_data %>%
  filter(
    !is.na(obj_baujahr_10y),     # first two conditions like in plot before
    obj_baujahr >= start_year,
    !is.na(geo_bezirk)) %>%     # also kick out offers without valid borough here
  select(
    cst_nettokalt_sqm, 
    geo_bezirk, 
    obj_baujahr, 
    obj_baujahr_10y
    ) %>% 
  group_by(
    geo_bezirk,
    obj_baujahr_10y
    ) %>%
  summarise(
    cst_nettokalt_sqm = median(cst_nettokalt_sqm, na.rm = T),
    n = n()
  ) %>%
  ungroup() %>% 
  # drop unused factor levels to avoid complete() including them
  mutate(
    obj_baujahr_10y = fct_drop(obj_baujahr_10y)
    ) %>% 
  # complete missing combinations of Bezirk and Year
  complete(
    geo_bezirk, 
    obj_baujahr_10y = unique(.$obj_baujahr_10y)
    ) %>%
  mutate(
    # now remove entries with < 100 offers
    cst_nettokalt_sqm = ifelse(n < 100, NA, cst_nettokalt_sqm)
  ) %>%
  arrange(
    desc(geo_bezirk)
    ) 


## ----p2.6.3.3_export-----------------------------------------------------
p2.6.3.3_data %>%  
  rename_recode_cols_for_export() %>% 
  # kick out number of offers
  select(-`Anzahl der Angebote`) %>%
  export_csv("2.6.3.3_heatmap_neubau.csv")

