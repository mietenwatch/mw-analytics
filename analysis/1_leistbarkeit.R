####################################################################
#
# Chapter 1 'Leistbarkeit' 
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
  odbc,
  RMySQL,
  R.utils,
  enc,
  lubridate,
  stringdist,
  RecordLinkage,
  DT,
  knitr,
  rgdal,
  sp,
  rgeos,
  sf,
  mapview,
  lubridate,
  readxl,
  mgcv,
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
sapply(
  list.files(
    pattern = "[.]R$",
    path = "functions",
    full.names = TRUE
    ),
  source
  )

# We better make sure MASS does not interfere with dplyr
select <- dplyr::select


#### Analysis ####
#+++++++++++++++++

#### Berlin – Stadt für alle? ####

### 1.4.4 Stats before plots

# For assessment of affordability: define suitability as function of household-size
# Define average householf income by houshold size
# --> Mikrozensus 2016 https://www.statistik-berlin-brandenburg.de/publikationen/stat_berichte/2017/SB_A01-11-00_2016j01_BE.pdf p.49

# define as named vector
net_income <- c(
  `1Pers` = 1375,
  `2Pers` = 2625,
  `3Pers` = 3075,
  `4Pers` = 3425,
  `5Pers+` = 3000
)

# indicate offers with suitable n of rooms
is_suitable_income <- tbl_df(mw_data) %>%
  # we can only analyse offers with valid cst_gesamtmiete
  drop_na(cst_gesamtmiete) %>% 
  select(geo_bezirk, cst_gesamtmiete_sqm, is_eingestellt, cst_gesamtmiete, 
         obj_zimmer, obj_wohnflaeche, obj_is_gefoerdert,
         geo_sbahn) %>%
  mutate(
    `1` = obj_zimmer >= 1 & obj_zimmer <= 2, # is flat suitable for 1 person?
    `2` = obj_zimmer >= 2 & obj_zimmer <= 3, # ... 2 persons ...
    `3` = obj_zimmer >= 3 & obj_zimmer <= 4,
    `4` = obj_zimmer >= 4 & obj_zimmer <= 5,
    `5` = obj_zimmer >= 5 #for affordability include all flat sizes >5 rooms because it is defnied as 5 and more persons
  ) %>% 
  # make wide data long
  pivot_longer(
    cols = c(`1`:`5`),
    names_to = "Personen im Haushalt",
    values_to = "is_suitable"
  ) %>% 
  mutate(`Personen im Haushalt` = as.numeric(`Personen im Haushalt`))

### now calculate stats ###
## for 1 Person
is_suitable_income %>%
  # subsidized housing is not available for NON WBS-holders
  filter(obj_is_gefoerdert == FALSE) %>% 
  filter(`Personen im Haushalt` == 1 & is_suitable) %>%
  # a.) first for sBahnring
  group_by(geo_sbahn) %>%
  summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
  # b.) for whole Berlin, binding rows
  bind_rows(
    is_suitable_income %>%
      ungroup() %>%
      filter(obj_is_gefoerdert == FALSE) %>% 
      filter(`Personen im Haushalt` == 1 & is_suitable ) %>%
      summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
      mutate(geo_sbahn = rep("Berlin", times = length(`Leistbare Wohnungen`)))
  ) %>%
  rename_recode_cols_for_export()

## same for 5+ Persons
is_suitable_income %>%
  # subsidized housing is not available for NON WBS-holders
  filter(obj_is_gefoerdert == FALSE) %>% 
  filter(`Personen im Haushalt` == 5 & is_suitable) %>%
  # a.) first for sBahnring
  group_by(geo_sbahn) %>%
  summarise(Angebotszahl=n(),
    `Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["5Pers+"]) / Angebotszahl) %>%
  # b.) for whole Berlin, binding rows
  bind_rows(
    is_suitable_income %>%
      ungroup() %>%
      filter(obj_is_gefoerdert == FALSE) %>% 
      filter(`Personen im Haushalt` == 5 & is_suitable) %>%
      summarise(Angebotszahl= n(),
        `Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["5Pers+"]) / Angebotszahl) %>%
      mutate(geo_sbahn = rep("Berlin", times = length(`Leistbare Wohnungen`)),
             )
  ) %>%
  rename_recode_cols_for_export()


### `p1.4.5` Wohnfläche, die sich ein Haushalt mit Durchschnittseinkommen leisten kann – nach Personenanzahl und Bezirk. ####

## ----p1.4.5--------------------------------------------------------------
# calculate affordable area according to median price per square meter for suitable flats
p1.4.5_data <- is_suitable_income %>%
  # subsidized housing is not available for NON WBS-holdes
  filter(obj_is_gefoerdert == FALSE) %>% 
  # remove offers without Bezirk here, but not for calculation for whole Berlin
  filter(!is.na(geo_bezirk)) %>%
  # a.) for Bezirke
  group_by(geo_bezirk, `Personen im Haushalt`) %>%
  dplyr::summarise(
    `Leistbare Fläche` = (0.3 * net_income[unique(`Personen im Haushalt`)]) / 
      median(cst_gesamtmiete_sqm[is_suitable]),
    # number of offers to check for small sample size
    n = sum(is_suitable)
  ) %>% 
  # b.) for whole Berlin
  bind_rows(
    is_suitable_income %>%
      # subsidized housing is not available for NON WBS-holdes
      filter(obj_is_gefoerdert == FALSE) %>% 
      group_by(`Personen im Haushalt`) %>%
      summarise(
        `Leistbare Fläche` = (0.3 * net_income[unique(`Personen im Haushalt`)]) /
          median(cst_gesamtmiete_sqm[is_suitable]),
        n = sum(is_suitable, na.rm = T)
      ) %>%
      mutate(geo_bezirk = rep("Berlin", times = length(`Leistbare Fläche`)))
  ) %>%
  ungroup() %>% 
  mutate(
    geo_bezirk = factor(geo_bezirk),
    # set affordable area to NA if sample size too small
    `Leistbare Fläche` = ifelse(n <= 100, NA, `Leistbare Fläche`),
    `Leistbare Fläche` = round(`Leistbare Fläche`, 1),
    # relevel factors for plot-order
    geo_bezirk = fct_reorder2(geo_bezirk,
      `Leistbare Fläche`,
      `Personen im Haushalt`,
      function(leist, pers) {
        unique(leist[pers == 1])
      },
      .desc = TRUE
    )
  ) %>%
  rename_recode_cols_for_export()


## ----p1.4.5_export-------------------------------------------------------
p1.4.5_data %>%
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
  export_csv("1.4.5_leistbare_wohnflaeche_dottplot.csv")



### `p1.4.9` Anteil leistbarer 1- bis 2-Zimmer-Wohnungen für durchschn 1-Personen-Haushalt ####

## ----p1.4.9--------------------------------------------------------------
# calculate fraction of affordable flats for single households by Bezirk
p1.4.9_data <- is_suitable_income %>%
  # subsidized housing is not available for NON WBS-holders
  filter(obj_is_gefoerdert == FALSE) %>% 
  filter(`Personen im Haushalt` == 1 & is_suitable) %>%
  # a.) first for bezirke
  group_by(geo_bezirk) %>%
  summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
  # b.) for whole Berlin, binding rows
  bind_rows(
    is_suitable_income %>%
      ungroup() %>%
      filter(obj_is_gefoerdert == FALSE) %>% 
      filter(`Personen im Haushalt` == 1 & is_suitable) %>%
      summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
      mutate(geo_bezirk = rep("Berlin", times = length(`Leistbare Wohnungen`)))
  ) %>%
  # add variable nicht leistbar for stacked barplot
  mutate(`Nicht-leistbare Wohnungen` = 100 - `Leistbare Wohnungen`) %>%
  # NAs come from offers without Bezirk that were included in whole Berlin
  drop_na(geo_bezirk) %>%
  rename_recode_cols_for_export()


## ----p1.4.9_export-------------------------------------------------------
p1.4.9_data %>%
  # reorder for plotly sequence
  arrange(-`Leistbare Wohnungen`) %>% 
  export_csv("1.4.9_1-pers_hh_barchart.csv")


## ----p1.4.9_stats---------------------------------------------
# stats for affordability before Leistbare Wohnfläche Durchschnittshaushalte
 is_suitable_income %>%
  # subsidized housing is not available for NON WBS-holders
  filter(obj_is_gefoerdert == FALSE) %>% 
  filter(`Personen im Haushalt` == 1 & is_suitable) %>%
  # a.) first for bezirke
  group_by(geo_bezirk) %>%
  summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
  # b.) for whole Berlin, binding rows
  bind_rows(
    is_suitable_income %>%
      ungroup() %>%
      filter(obj_is_gefoerdert == FALSE) %>% 
      filter(`Personen im Haushalt` == 1 & is_suitable) %>%
      summarise(`Leistbare Wohnungen` = 100 * sum(cst_gesamtmiete <= 0.3 * net_income["1Pers"]) / n()) %>%
      mutate(geo_bezirk = rep("Berlin", times = length(`Leistbare Wohnungen`)))
  ) %>%
  # add variable nicht leistbar for stacked barplot
  mutate(`Nicht-leistbare Wohnungen` = 100 - `Leistbare Wohnungen`) %>%
  # NAs come from offers without Bezirk that were included in whole Berlin
  drop_na(geo_bezirk) %>%
  rename_recode_cols_for_export() 


#### Wo wohnen mit Hartz IV? ####

### `p1.5.4` Anteil der Angebote, bei denen die Mietkosten vom Jobcenter übernommen würden ####

## ----p1.5.4--------------------------------------------------------------
# indicate offers with suitable n of rooms
is_suitable_kdu <- tbl_df(mw_data) %>%
  # we can only analyse offers with valid cst_gesamtmiete
  drop_na(cst_gesamtmiete) %>% 
  select(geo_bezirk, cst_gesamtmiete_sqm, is_eingestellt, cst_gesamtmiete, 
         obj_zimmer, obj_wohnflaeche, obj_is_gefoerdert,
         geo_sbahn) %>%
  mutate(
    `1` = obj_zimmer >= 1 & obj_zimmer <= 2, # is flat suitable for 1 person?
    `2` = obj_zimmer >= 2 & obj_zimmer <= 3, # ... 2 persons ...
    `3` = obj_zimmer >= 3 & obj_zimmer <= 4,
    `4` = obj_zimmer >= 4 & obj_zimmer <= 5,
    `5` = obj_zimmer >= 5 & obj_zimmer <= 6 # this line is different than .._income because KDU data only include up 5 Person households
  ) %>% 
  # make wide data long
  pivot_longer(
    cols = c(`1`:`5`),
    names_to = "Personen im Haushalt",
    values_to = "is_suitable"
  ) %>% 
  mutate(`Personen im Haushalt` = as.numeric(`Personen im Haushalt`))


# Load Kosten der Unterkunft data from
# --> https://www.berlin.de/jobcenter-tempelhof-schoeneberg/leistungsbereich/unterkunft-und-heizung/artikel.394407.php 
# As heating type we assume oil - within oil we take the highest cost to stay conservative (area 100-250)
kdu <- c(
  `1Pers`=472,
  `2Pers`=553,
  `3Pers`=713,
  `4Pers`=802,
  `5Pers`=933
)
# identify offers compatible with KDU by S-Bahnring
is_affordable_kdu <- is_suitable_kdu %>%
  select(`Personen im Haushalt`, is_suitable, is_eingestellt, geo_bezirk, geo_sbahn, cst_gesamtmiete) %>%
  # identify offers that are suitable and affordable
  mutate(is_affdble = is_suitable & cst_gesamtmiete <= kdu[`Personen im Haushalt`])

p1.5.4_data <- is_affordable_kdu %>% 
  # we drop all offers which dont have a valid location via geo_bezirk
  drop_na(geo_bezirk) %>% 
  group_by(geo_sbahn, `Personen im Haushalt`) %>%
  # calculate % affordable offers
  summarise(`Leistbare Wohnungen in %` = round(100 * sum(is_affdble) / sum(is_suitable), 1)) %>%
  # rename columns for export
  rename_recode_cols_for_export()



## ----p.1.5.4_export------------------------------------------------------
p1.5.4_data %>%
  # reorder cols
  select(c("Personen im Haushalt", "Leistbare Wohnungen in %", "S-Bahn-Ring")) %>% 
  arrange(`Personen im Haushalt`) %>% 
  export_csv("1.5.4_leistbare_wohnungen_barchart.csv")


### `p1.5.7` Anzahl der Wohnungsinserate pro Woche, bei denen Wohnkosten für 2-Personen-Haushalt KdU übernommen werden ####

## ----p1.5.7--------------------------------------------------------------
## Timeseries to display number of offers affordable with KDU for 2 person hh
# filter last 12 months and add week for aggregation
start_date <- as.Date("2018-09-22") 

is_affordable_kdu_ts_week <- is_affordable_kdu %>%
  filter(is_eingestellt >= start_date) %>%
  mutate(
    geo_bezirk = as.character(geo_bezirk), # to char to avoid completion of all factor levels when using complete()
    Woche = week(is_eingestellt)
  ) %>% # data as character to create identical week
  rename(Bezirk = geo_bezirk)

# summarise for Bezirke
p1.5.7_data <- is_affordable_kdu_ts_week %>%
  # filter to only 2 person hh
  filter(`Personen im Haushalt` == 2) %>% 
  # Bezirke
  group_by(Bezirk, Woche) %>%
  summarise(`Anzahl Angebote pro Woche` = sum(is_affdble)) %>%
  # now complete missing combinations of Bezirk and Week and fill missing rows with 0 for offers.
  # missing rows are actually weeks with zero offers
  complete(Bezirk, Woche = 1:53, fill = list(`Anzahl Angebote pro Woche` = 0)) %>% # complete needs manual definition in case of numeric values
  filter(Bezirk %in% c("Friedrichshain-Kreuzberg", "Neukölln")) %>%
  mutate(Datum = start_date + weeks(Woche - 1)) %>%
  select(-Woche)


## ----p1.5.7_export-------------------------------------------------------
p1.5.7_data %>%
  # long to wide for plotly
  spread(key = Bezirk, value = `Anzahl Angebote pro Woche`) %>% 
  export_csv("1.5.7_exkurs_kdu_2pers_lineplot.csv")


## ----p1.5.7_stats---------------------------------------------
p1.5.7_data %>%
  group_by(Bezirk) %>%
  summarise(`Summe der Angebote` = sum(`Anzahl Angebote pro Woche`))


## Hier droht Verdrängung ####

### `m1.6.3` Karten auf LOR Planungsraumebene zu Verdrängungsdruck, Mietenhöhe, Prekarität ####

## ----m1.6.3--------------------------------------------------------------
# Calculate displacement pressure and the other indices
m1.6.3 <- indics_displacement()

## ----m1.6.3_export-------------------------------------------------------
m1.6.3 %>%
  st_set_geometry(NULL) %>% # stripping from spatial attribs
  select(-7, -8, -9, -10) %>%
  rename_recode_cols_for_export() %>% 
  export_csv("1.6.3_Verdraengungskarten.csv")

