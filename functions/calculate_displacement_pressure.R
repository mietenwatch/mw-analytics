#-------------------
# Plots of displacement
# output: 3 categorial indicators: precarity, rent-height and pressure of displacement.

indics_displacement <- function(datDir = "data/") {
  ### load data
  load(paste0(datDir, "geodata/mw_geodata.RData")) # for lor shapes

  # Data on precarity from Monitoring Soziale Stadtentwicklung
  # https://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/monitoring/index.shtml
  socialstatus <- read_excel(paste0(datDir, "MonitoringSozialeStadtenwicklung2017/3.IndexInd_z_WerteMSS2017.xlsx"), range = "A19:J446", col_names = T)
  names(socialstatus)[c(1, 2, 8)] <- c("ID", "PLR", "prekaritaet_index")
  # a precarity index of 0 indicates NA
  socialstatus <- socialstatus %>% mutate(prekaritaet_index = ifelse(prekaritaet_index == 0, NA, prekaritaet_index))
  # match socialstatus to our data
  mw_data_sf <- mw_data_sf %>%
    ungroup() %>%
    mutate(prekaritaet_index = socialstatus$prekaritaet_index[ match(geo_lor_planungsraum_id, socialstatus$ID) ])

  # add LOR - Planungsräume, create a multipolygon object with indicators for plotting
  mw_data_sf.poly <- sf_LOR %>%
    select(geo_lor_planungsraum_id, geometry) %>%
    st_join(mw_data_sf, by = "geo_lor_planungsraum_id", left = F) %>% # inner join sf_LOR with our data because we need only data for existing PLRs. We loose 1 PLR
    rename(geo_lor_planungsraum_id = geo_lor_planungsraum_id.x) %>% # keep id from sf_LOR
    select(geo_lor_planungsraum_id, geo_lor_planungsraum, prekaritaet_index, cst_gesamtmiete_sqm, pol_milieu_s, geo_sbahn) %>%
    # calculate average values inside PLR
    group_by(geo_lor_planungsraum_id) %>%
    summarise(
      cst_gesamtmiete_sqm = median(cst_gesamtmiete_sqm, na.rm = TRUE),
      prekaritaet_index = unique(prekaritaet_index), # social status is homogenous inside PLR
      geo_sbahn = max_table(geo_sbahn), # is mayority of offers inside or outside of Sbahnring?
      geo_lor_planungsraum = unique(geo_lor_planungsraum),
      # compute number of offers per PLR
      Angebotszahl = n()
    ) %>% # create z-transformed variables for later creation of displacement-indicator
    mutate(
      gesamtmiete_ztransf = scale(cst_gesamtmiete_sqm),
      prekaritaet_index_ztransf = scale(prekaritaet_index)
    )
  # calculate min for index creation
  prekaritaet_index_ztransf_min <- min(mw_data_sf.poly$prekaritaet_index_ztransf, na.rm = T)
  gesamtmiete_ztransf_min <- min(mw_data_sf.poly$gesamtmiete_ztransf, na.rm = T)

  # mininum number of offers for definition of data-gap:
  n.offers <- 25

  ### Create categorial indicator for displacement:
  # 1) create continuous index for displacement from z-transformed precarity and rent-height.
  # 2) discretise displacement-index into 4 categories:
  # for all PLR with below-average precarity set displacement to 0. This is done because we only look at hotspots of displacement here,
  # meaning locations with high precarity. On average, locations with high rents but low precarity are less prone to be hotspots of displacement
  # because people have higher income-security.
  # 3) discretise precarity-index into 4 categories: Below average, medium, high, very high
  # 4) discretise rent-index into 4 categories: Below average, medium, high, very high

  mw_data_sf.poly <- mw_data_sf.poly %>%
    mutate(
      # set rents to NA if small sample size
      cst_gesamtmiete_sqm = ifelse(Angebotszahl < n.offers, NA, cst_gesamtmiete_sqm),
      # 1) create index for displacement. Square to increase weight for offers with higher precarity or rent.
      # Add very small dummy value +1E-10 to distinguish values of minimum precarity from PLRs with precarity below average (the latter are afterwards set to zero)
      `Index Verdrängungsdruck` = (prekaritaet_index_ztransf - prekaritaet_index_ztransf_min + 1E-10)^2 + (gesamtmiete_ztransf - gesamtmiete_ztransf_min + 1E-10)^2,
      # set displacement in PLRs of below-average precarity to zero. Note that this does not represent zero precarity but below average precarity 
      # and is set distinct from NA to distiguish later in visualization.
      `Index Verdrängungsdruck` = ifelse(prekaritaet_index >= 0, `Index Verdrängungsdruck`, 0),
      # set to NA if small sample size
      `Index Verdrängungsdruck` = ifelse(Angebotszahl < n.offers, NA, `Index Verdrängungsdruck`),
      # 2) now discretize displacement. First class = below average precarity. Terciles for above average.
      Verdrängungsdruck = cut(
        x = `Index Verdrängungsdruck`,
        breaks = c(0, quantile(`Index Verdrängungsdruck`[`Index Verdrängungsdruck` > 0],
          probs = c(0, 1/3, 2/3, 1), na.rm = T
        )), 
        labels = c("unterdurchschnittlich", "mittel", "hoch", "sehr hoch"),
        include.lowest = T,
        ordered_result = T
      ),
      # 3) discretise precarity
      Prekarität = cut(
        x = prekaritaet_index,
        breaks = quantile(prekaritaet_index, probs = c(0, 0.5, 0.5 + 0.5 * 1 / 3, 0.5 + 0.5 * 2 / 3, 1), na.rm = T),
        labels = c("unterdurchschnittlich", "mittel", "hoch", "sehr hoch"),
        ordered_result = T
      ),
      # 4) discretise rent height
      Miethöhe = cut(cst_gesamtmiete_sqm,
                     breaks = quantile(cst_gesamtmiete_sqm, probs = c(0, 0.5, 0.5 + 0.5 * 1 / 3, 0.5 + 0.5 * 2 / 3, 1), na.rm = T),
                     labels = c("unterdurchschnittlich", "mittel", "hoch", "sehr hoch")
      ),
      ordered_result = T
    ) %>% # define class for NA
    mutate(
      Verdrängungsdruck = fct_explicit_na(Verdrängungsdruck, "unsichere Datenlage"),
      Prekarität = fct_explicit_na(Prekarität, "unsichere Datenlage"),
      Miethöhe = fct_explicit_na(Miethöhe, "unsichere Datenlage")
    ) %>%
  select(geo_lor_planungsraum_id, geo_lor_planungsraum, Verdrängungsdruck, Prekarität, Miethöhe)
  return(mw_data_sf.poly)
}
