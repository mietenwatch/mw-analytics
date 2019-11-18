####################################################################
#
# Chapter 3 'Antworten' 
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
  viridis,
  treemap,
  klaR,
  jsonlite
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

## Mietendeckel – runter mit der Miete ####

### `p3.2.4` Wohnfläche, die sich ein Haushalt mit Durchschn. EK bei Mietobergrenzen leisten könnte ####

## ----p.3.2.4-------------------------------------------------------------
# Analogue calculation to p1.4.5: 
# For assessment of affordability: define suitability as function of household-size

# Define average household income by houshold size
# --> Mikrozensus 2016 https://www.statistik-berlin-brandenburg.de/publikationen/stat_berichte/2017/SB_A01-11-00_2016j01_BE.pdf p.49

# define as named vector
net_income <- c(
  `1Pers` = 1375,
  `2Pers` = 2625,
  `3Pers` = 3075,
  `4Pers` = 3425,
  `5Pers` = 3000
)

# indicate offers with suitable n of rooms
is_suitable <- tbl_df(mw_data) %>%
  drop_na(cst_md_gesamtmiete) %>% 
  select(geo_bezirk,
         cst_md_gesamtmiete_sqm, 
         cst_md_gesamtmiete,
         is_eingestellt,
         obj_zimmer,
         obj_wohnflaeche,
         geo_sbahn) %>%
  mutate(
    `1` = obj_zimmer >= 1 & obj_zimmer <= 2, # is flat suitable for 1 person?
    `2` = obj_zimmer >= 2 & obj_zimmer <= 3, # ... 2 persons ...
    `3` = obj_zimmer >= 3 & obj_zimmer <= 4,
    `4` = obj_zimmer >= 4 & obj_zimmer <= 5,
    `5` = obj_zimmer >= 5 # for affordability include all flat sizes >5 rooms because it is defnied as 5 and more persons
  ) %>%
  # make wide data long
  pivot_longer(
    cols = c(`1`:`5`),
    names_to = "Personen im Haushalt",
    values_to = "is_suitable"
  ) %>%
  mutate(`Personen im Haushalt` = as.numeric(`Personen im Haushalt`))

# calculate affordable area according to median price per square meter for suitable flats
p3.2.4data <- is_suitable %>%
  # remove offers without Bezirk here, but not for calculation for whole Berlin
  filter(!is.na(geo_bezirk)) %>%
  # a.) for Bezirke
  group_by(geo_bezirk, `Personen im Haushalt`) %>%
  dplyr::summarise(
    `Leistbare Fläche` = 0.3 * net_income[unique(`Personen im Haushalt`)] /
      median(cst_md_gesamtmiete_sqm[is_suitable]),
    # number of offers to check for small sample size
    n = sum(is_suitable)
  ) %>%
  # b.) for whole Berlin
  bind_rows(
    is_suitable %>%
      group_by(`Personen im Haushalt`) %>%
      summarise(
        `Leistbare Fläche` = (0.3 * net_income[unique(`Personen im Haushalt`)]) /
          median(cst_md_gesamtmiete_sqm[is_suitable]),
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


## ----p3.2.4_export-------------------------------------------------------
p3.2.4data %>%
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
  export_csv("3.2.4_leistbare_wohnflaeche_mietendeckel.csv")


### `p3.2.5` Durchschnittliche Nettokaltmiete bei Angeboten ohne und mit Mietendeckel nach Baujahresklassen. ####

## ----p3.2.5--------------------------------------------------------------
# Class breaks for rent cap (= Mietendeckel)
md_klasse_breaks <- c(min(year(mw_data$obj_baujahr), na.rm = TRUE),
                      1919, 1950, 1965, 1973, 1991, 2003, 2014, 
                      year(Sys.Date()))

p3.2.5data <- mw_data %>% 
  # no construction year, no valid calculation of rent cap (-> Mietobergrenze!)
  drop_na(obj_baujahr) %>% 
  filter(year(obj_baujahr) <= year(Sys.Date())) %>% 
  mutate(
    obj_baujahr_md_klasse = cut(year(obj_baujahr),
                                breaks = md_klasse_breaks,
                                labels = c("bis 1918", 
                                           "1919-1949",
                                           "1950-1964",
                                           "1965-1972",
                                           "1973-1990",
                                           "1991-2002",
                                           "2003-2013",
                                           "ab 2014"),
                                include.lowest = TRUE,
                                ordered_result = TRUE,
                                right = FALSE,
                                dig.lab = 4)
    ) %>%
  group_by(
    obj_baujahr_md_klasse
    ) %>% 
  # Here we use mean to make 1 Euro addition according to §5(3) visible
  summarise(
    cst_nettokalt_sqm = mean(cst_nettokalt_sqm),
    cst_md_nettokalt_sqm = mean(cst_md_nettokalt_sqm)
    ) 



## ----p3.2.5data_export---------------------------------------------------
p3.2.5data %>% 
  rename_recode_cols_for_export() %>% 
  rename(
    "Nettokaltmiete ohne Mietendeckel" = "Nettokaltmiete in €/m²",
    "Nettokaltmiete mit Mietendeckel" = "Nettokaltmiete mit Mietendeckel in €/m²"
    ) %>% 
  export_csv("3.2.5_md_ohne_mit_mietendeckel.csv")


## Wohnungen privater Immobilienkonzerne vergesellschaften ####

### `m3.3.2` Wo liegen die Angebote der zu enteignenden Unternehmen? (Static Map) ####

## ----m3.3.2_stats--------------------------------------------------------
# We disregard other firms that would get expropriated, 
# we only regard the ones mentioned in functions/create_is_anbieter_exprop.R

# For the following other sellers, there are no offers in the data!
other_exprop <- c("TAG","IMW","Gropius","DVI","Pears")
pattern_exprop <- paste(other_exprop,
                        collapse = "|")

# This what we find if we search for the strings: 
#  --> None of the real firms to be expropriated among them
grep(pattern_exprop,
     mw_data$is_anbieter, 
     ignore.case = TRUE,
     value=TRUE) 

# So we have this many apartments of firms to be expropriated in the dataset:
mw_data %>% 
  filter(is_anbieter_is_exprop == TRUE) %>% 
  nrow()



## ----m3.3.2--------------------------------------------------------------
# Geolocations of all apartments that would get exproriated
m3.3.2data <- mw_data %>% 
  filter(is_anbieter_is_exprop == TRUE) %>% 
  # no jittering nescessary - its a non-zoomable static map
  select(is_anbieter_exprop,
         geo_lat,
         geo_lon) 



## ----m3.3.2_export-------------------------------------------------------
m3.3.2data %>% 
  export_csv("3.3.2_Bestaende_zu_enteignen_staticmap.csv")


### `p3.3.4` Durchschnittliche Nettokaltmieten bei Angeboten der zu enteignenden Konzerne nach Szenario. ####


## ----p3.3.4--------------------------------------------------------------
p3.3.4data <- mw_data %>% 
  filter(is_anbieter_is_exprop) %>%
  mutate(cst_ex_nettokalt_sqm = 3.70) %>%
  summarise(
    cst_nettokalt_sqm = mean(cst_nettokalt_sqm, na.rm = T),
    cst_md_nettokalt_sqm = mean(cst_md_nettokalt_sqm, na.rm = T),
    cst_ex_nettokalt_sqm = mean(cst_ex_nettokalt_sqm, na.rm = T),
    cst_km_kostdeckend_nettokalt_sqm = mean(cst_km_kostdeckend_nettokalt_sqm, na.rm = T)
  ) %>% 
  mutate_if(is.numeric, round, 2) 

#  Transpose tibble for plotly
p3.3.4data <- tibble(
  Szenario = names(p3.3.4data),
  `Nettokaltmiete in €/m²` = (p3.3.4data %>% 
                                slice(1) %>% 
                                c(., recursive=TRUE) %>% 
                                unname)
  ) 

# Rename für export
p3.3.4data<- p3.3.4data %>% 
  mutate(Szenario = factor(Szenario)) %>% 
  mutate(Szenario = fct_recode(Szenario,
                               `aktuelle Nettokaltmiete` = "cst_nettokalt_sqm", 
                               `Nettokaltmiete mit Mietendeckel` = "cst_md_nettokalt_sqm",
                               `Nettokaltmiete nach Enteignung` = "cst_ex_nettokalt_sqm",
                               `kostendeckende Nettokaltmiete` = "cst_km_kostdeckend_nettokalt_sqm")) %>% 
  arrange(`Nettokaltmiete in €/m²`) 


## ----p3.3.4_export-------------------------------------------------------
p3.3.4data %>% 
  export_csv("3.3.4_nettokaltmiete_szenariovergleich.csv")

