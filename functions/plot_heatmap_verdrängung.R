#-------------------

## PLOTS DER VERDRÄNGUNG

# ======= Indikatoren vorbereiten ========================
indics_displace <- function(datDir = "data/") {

  ### Lade Daten
  load(paste0(datDir, "geodata/mw_geodata.RData")) # for lor shapes

  socialstatus <- read_excel(paste0(datDir, "MonitoringSozialeStadtenwicklung2017/3.IndexInd_z_WerteMSS2017.xlsx"), range = "A19:J446", col_names = T)
  names(socialstatus)[c(1, 2, 8)] <- c("ID", "PLR", "prekaritaet_index")
  socialstatus <- socialstatus %>% mutate(prekaritaet_index = ifelse(prekaritaet_index == 0, NA, prekaritaet_index))

  mw_data_sf <- mw_data_sf %>%
    ungroup() %>%
    mutate(prekaritaet_index = socialstatus$prekaritaet_index[ match(geo_lor_planungsraum_id, socialstatus$ID) ])

  # add LOR - Planungsräume, create a multipolygon object with indicators for plotting
  mw_data_sf.poly <- sf_LOR %>%
    select(geo_lor_planungsraum_id, geometry) %>%
    st_join(mw_data_sf, by = "geo_lor_planungsraum_id", left = F) %>% # inner join because we need only data for existing PLRs. We loose 1 PLR
    rename(geo_lor_planungsraum_id = geo_lor_planungsraum_id.x) %>% # keep id from sf_LOR
    dplyr::select(geo_lor_planungsraum_id, geo_lor_planungsraum, prekaritaet_index, cst_gesamtmiete_sqm, pol_milieu_s, geo_sbahn) %>%

    # mitteln für Angebote innerhalb der PLR
    group_by(geo_lor_planungsraum_id) %>%
    summarise(
      cst_gesamtmiete_sqm = median(cst_gesamtmiete_sqm), # Mean über PLRs
      prekaritaet_index = unique(prekaritaet_index), # social status ist homogen innerhalb PLRs
      geo_sbahn = MaxTable(geo_sbahn), # ist mehrheit der Wohnungsangebote innerhalb oder ausserhalb?
      Milieuschutz = MaxTable(pol_milieu_s), # liegen Wohnungsangebote überwiegend in Milieuschutzgebiet(en)?
      geo_lor_planungsraum = unique(geo_lor_planungsraum),
      Angebotszahl = n()
    ) %>% # erzeuge z-transformierte variablen für spätere generierung von Verdrängungs-indikator
    mutate(
      gesamtmiete_ztransf = scale(cst_gesamtmiete_sqm),
      prekaritaet_index = scale(prekaritaet_index)
    )

  # calculate min for index creation
  prekaritaet_index_min <- min(mw_data_sf.poly$prekaritaet_index, na.rm = T)
  gesamtmiete_ztransf_min <- min(mw_data_sf.poly$gesamtmiete_ztransf, na.rm = T)

  # mininam n of offers for definition of datalack:
  n.offers <- 25
  ### Erzeuge Verdrängungsdruck aus diskretisierung eines kontinuierlichen Index Verdrängungsdruck
  # Index ist Summe aus Prekarität und GEsamtmiete, quadriert um WErte großer Prekarität oder Miete auch höher zu gewichten.
  # Index-Werte mit unterdurchschnittlicher Prekarität werden auf 0 gesetzt um von NA=unbekannte PLR zu unterscheiden. Deshalb minimum des Index +E-10 addiert
  # Danach diskretisieren mit cut( breaks=quantile( <Index ohne 0> )) Index=0 -> Kategorie "Kein Brennpunkt)
  mw_data_sf.poly <- mw_data_sf.poly %>%
    mutate(
      # unsichere Datenlage für Miete
      cst_gesamtmiete_sqm = ifelse(Angebotszahl < 25, NA, cst_gesamtmiete_sqm),
      # 1) Verdrängungsdruck kategorisieren
      `Index Verdrängungsdruck` = (prekaritaet_index - prekaritaet_index_min + 1E-10)^2 + (gesamtmiete_ztransf - gesamtmiete_ztransf_min + 1E-10)^2, # create mean of two z-transformed values to create indicator with equal weights for both variables.
      `Index Verdrängungsdruck` = ifelse(prekaritaet_index > 0, `Index Verdrängungsdruck`, 0), # PLRs deren Prekarität unterdurchschnittlich ist auf null setzen, um zu vermeiden superluxus-PLRs in Mitte als Verdrängungsgebiete darzustellen.
      `Index Verdrängungsdruck` = ifelse(Angebotszahl < 25, NA, `Index Verdrängungsdruck`), # Unsichere Datenalage wenn prekarität ==NA oder Angebotszahl <25
      Verdrängungsdruck = cut(
        x = `Index Verdrängungsdruck`, # jetzt diskretisieren
        breaks = c(0, quantile(`Index Verdrängungsdruck`[`Index Verdrängungsdruck` > 0],
          probs = c(0, 0.33, 0.66, 1), na.rm = T
        )), # wenn index=0 setze Kategorie = 0 (Kein Brennpunkt) da dort Prekarität nicht überdurchschnittlich
        labels = c("kein Brennpunkt", "mittel", "hoch", "sehr hoch"),
        include.lowest = T,
        ordered_result = T
      ),
      # 2) Prekarität Kategorisiseren
      Prekarität = cut(
        x = prekaritaet_index,
        breaks = quantile(prekaritaet_index, probs = c(0, 0.5, 0.5 + 0.5 * 1 / 3, 0.5 + 0.5 * 2 / 3, 1), na.rm = T),
        labels = c("unterdurchschnittlich", "mittel", "hoch", "sehr hoch"),
        ordered_result = T
      ),
      # 3) Mietenhöhe kategorisisen
      Mietenhöhe = cut(cst_gesamtmiete_sqm,
        breaks = quantile(cst_gesamtmiete_sqm, probs = c(0, 0.5, 0.5 + 0.5 * 1 / 3, 0.5 + 0.5 * 2 / 3, 1), na.rm = T),
        labels = c("unterdurchschnittlich", "mittel", "hoch", "sehr hoch")
      ),
      ordered_result = T
    ) %>% # für 1-3: defininere NA als Kategorie "unsichere Datenlage"
    mutate(
      Verdrängungsdruck = fct_explicit_na(Verdrängungsdruck, "unsichere Datenlage"),
      Prekarität = fct_explicit_na(Prekarität, "unsichere Datenlage"),
      Mietenhöhe = fct_explicit_na(Mietenhöhe, "unsichere Datenlage")
    ) %>%
    select(geo_lor_planungsraum_id, geo_lor_planungsraum, Verdrängungsdruck, Prekarität, Mietenhöhe, cst_gesamtmiete_sqm, geo_sbahn, Milieuschutz, `Index Verdrängungsdruck`)
  return(mw_data_sf.poly)
}
