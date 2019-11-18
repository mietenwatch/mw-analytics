#### Calculate kostendeckende Miete Analysis ####
#################################################

calculate_kostendeckende_miete <- function(mw_data) {

  # Nur mit Daten rechnen die ein obj_baujahr haben!
  mw_data_for_km <- mw_data %>% 
    drop_na(obj_baujahr)
  
  ### Verwaltungskosten
  #####################
  verwaltungskosten_wohnung <- 284.62 # Verwaltungskostenpauschale pro Wohnung
  verwaltungskosten_stellplatz <- 37.12  # Verwaltungskostenpauschale Pro Je Garagen- oder Einstellplatz
  
  ### Instandhaltung
  #####################
  instand_weniger_22J <- 8.78 # in Wohnungen, deren Bezugsfertigkeit am Ende des Kalenderjahres …  weniger als 22 Jahre zurückliegt
  instand_22_32J <- 11.14 # … mindestens 22 Jahre zurückliegt
  instand_mehr_32J <- 14.23 # … mindestens 32 Jahre zurückliegt
  
  instand_aufzug <- 1.24 	# Zuschlag für Aufzug
  instand_parkplatz <- 84.16 # Pro Garagen- oder Einstellplatz (muss durch QM der Wohnung geteilt werden später!)
  
  ### Mietausfallwagnis
  #####################
  mietausfallwagnis <- 0.02 # 2 Prozent der Kosten
  
  ### Baukosten
  #############
  
  ### Baukosten
  # Basisbaukosten pro m²
  bau_base <- 1342 + # Basis Baukosten (noch ohne Keller, Aufzug etc.) 1400 - 58 Euro Balkone Ausbau
    # nicht optional: immer dabei:
    400 + # Baunebenkosten
    40 + # Aussenanlage
    137 # Baustellenlogistik
  
  # Baukosten pro m² optional: Was kommt dazu falls vorhanden?
  bau_keller <- 122 # Baukosten Keller
  bau_aufzug <- 68 # Baukosten Aufzug
  bau_balkon <- 58 # Baukosten Balkon
  # Für "Tiefgaragenplatz" werden 292 Euro veranschlagt. In Berlin gibt es kaum 
  # Tiefgaragenplätze, wir nehmen deshalb stattdessen an es werden normale Parkplätze gebaut. 
  # Für die veranschlagen wir 100 Euro:
  bau_parkplatz <- 100 # Baukosten Parkplatz
  
  ### Baukosten Dämmung (für besonders gut isolierte Wohnungen)
  # per m²
  bau_daemmung_AB <- 100 # Für Wohnungen mit Energieverbauchswert >25 & <75kwH/m²a (Kategorie A & B)
  bau_daemmung_Aplus <- 200 # Für Wohnungen mit Energieverbauswert <=25 kwH/m²a (Kategorie A+)
  
  ### Baukosten EXTRA (für hochwertige Wohnungen)
  # Pauschal, NICHT per m²
  bau_einbaukueche <- 10000 # Baukosten für Einbauküche
  bau_moebliert <- 10000 # Baukosten für Möblierung 
  # Extrabaukosten für besonder hochwertige Innenausstattung (=sophisticated),
  # für Luxusinnnenausstattung veranschlagen wir 2*bau_extrainnenauststattung (siehe unten)
  bau_extrainnenaustattung_sophisticated <- 5000 
  bau_extrainnenaustattung_luxury <- 10000
  
  ### Abschreibungssätze
  #######################
  
  # Abschreibungssatz für normale Baukosten
  abschreibungssatz_bau <- 1/80 # 100% des Hauses werden über 80 Jahre abgeschrieben - in Prozent
  min_gebaeudewert <- 30 # Das Haus wird bis minimal 30% seines Wertes abgeschrieben.
  max_abschreibungsdauer <- ((100-min_gebaeudewert)/abschreibungssatz_bau)/100 # maximale Abschreibungsdauer in Jahren fürs Gebäude
  
  # Abschreibungssatz Dämmung
  abschreibungssatz_daemmung <- 1/30 # 100% der Dämmung werden in 30 Jahren abgeschrieben.
  
  # Abschreibungssatz für Baukosten EXTRA, also hochwertige Ausstattung die schneller abnutzt
  abschreibungssatz_extra <- 1/20 # 100% der Ausstattung werden über 20 Jahre abgeschrieben - in Prozent
  
  ### Monatsfaktor
  ################
  # um später alle Jahreswerte auf Monat runter zu rechnen:
  monatlich <- 1/12
  
  mw_data_with_kdm <- mw_data_for_km %>% 
    rowwise() %>% # rowwise hier ganz wichtig, sonst wird die summe über den ganzen Datensatz genommen!
    mutate(# Die folgenden Baukosten werden in abhängigkeit des Gebäudealters abgeschrieben. 
      # Bei Gebäudealter mehr als 70 Jahre keine weitere Abschreibung.
      cst_km_baukosten = sum(bau_base,
                             # Wenn keine Angaben für Keller, Balkon, Aufzug gehen wir davon aus,
                             # dass diese Ausstattungsmerkmale nicht vorhanden sind. 
                             # Vermieter würden sowas angeben wenn es sowas gibt.
                             bau_keller * replace_na(eqp_keller, FALSE),
                             bau_aufzug * replace_na(eqp_aufzug, FALSE),
                             bau_balkon * replace_na(eqp_balkon, FALSE),
                             bau_parkplatz * replace_na(eqp_parkplatz, FALSE)),
      # Baukosten für besonders gute Dämmung:
      cst_km_baukosten_daemmung = case_when(egy_verbrauchswert<=25 ~ bau_daemmung_Aplus,
                                            egy_verbrauchswert> 25 & egy_verbrauchswert<=75 ~ bau_daemmung_AB,
                                            egy_verbrauchswert>75 | is.na(egy_verbrauchswert) ~ 0), 
      # Die Extrabaukosten werden wir altersunabhängig abschreiben, weil wenn diese 
      # Ausstattungsmerkmale gegeben sind, dann sind sie noch und in gutem Zustand (=konservative Rechnung)
      cst_km_baukosten_extra = sum(bau_einbaukueche * replace_na(eqp_kueche, FALSE),
                                   bau_moebliert * replace_na(eqp_moebliert, FALSE),
                                   # Extrakosten für hochwertige Innenausstattung
                                   case_when(eqp_innen =="sophisticated" ~ bau_extrainnenaustattung_sophisticated,
                                             eqp_innen =="luxury" ~ bau_extrainnenaustattung_luxury, 
                                             eqp_innen %in% c("no_information","normal","simple") ~ 0)),
      # Abschreibung beträgt abschreibungssatz_bau % der Baukosten/Jahr 
      # (= lineare Abschreibung, nicht geometrisch), sofern das Gebäude <= max_abschreibungsdauer Jahren alt ist
      # Bei älteren Gebäuden wird nicht weiter abgeschrieben.
      # Sie behalten auf ewig einen Restwert von min_gebaeudewert der Baukosten. 
      # Denn nach max_abschreibungsdauer in Jahren mal abschreibungssatz_bau sind 100-min_gebaeudewert % des Hauses abgeschrieben 
      # und es vergleibt ein Restwert von min_gebaeudewert %
      cst_km_abschreibung = monatlich * case_when(obj_alter<= max_abschreibungsdauer ~ (cst_km_baukosten*abschreibungssatz_bau), 
                                                  obj_alter > max_abschreibungsdauer ~ 0),
      # Abschreibung der Dämmung (bei besonder guter Dämmung, sonst 0)
      cst_km_abschreibung_daemmung = monatlich * (cst_km_baukosten_daemmung * abschreibungssatz_daemmung),
      # Abschreibung der Extra Baukosten
      cst_km_abschreibung_extra = monatlich * (cst_km_baukosten_extra * abschreibungssatz_extra)/obj_wohnflaeche,
      # Verwaltungskosten werden pro Wohnung gezahlt, wir müssen es aber auf den m² runterrechnen
      cst_km_verwaltung = monatlich * (sum(verwaltungskosten_wohnung,
                                           verwaltungskosten_stellplatz*eqp_parkplatz)/obj_wohnflaeche), 
      cst_km_instandhaltung_fix = monatlich * (case_when((obj_alter < 22) ~ instand_weniger_22J,
                                                         (obj_alter >= 22 & obj_alter < 32) ~ instand_22_32J, 
                                                         (obj_alter >= 32) ~ instand_mehr_32J)),
      # Wenn keine Angaben für Aufzug oder Parkplatz, gehen wir davon aus,
      # dass diese Ausstattungsmerkmale nicht vorhanden sind. 
      # Vermieter würden sowas angeben wenn es sowas gibt.
      cst_km_instandhaltung_var = monatlich * (sum(instand_aufzug * replace_na(eqp_aufzug, FALSE),
                                                   (instand_parkplatz * replace_na(eqp_parkplatz, FALSE))/obj_wohnflaeche)),
      cst_km_instandhaltung = sum(cst_km_instandhaltung_fix,
                                  cst_km_instandhaltung_var),
      # Jetzt Summe bildern aus allen Einzelposten
      cst_km_kostdeckend_nettokalt_sqm = round(sum(cst_km_verwaltung,
                                                   cst_km_instandhaltung,
                                                   cst_km_abschreibung,
                                                   cst_km_abschreibung_daemmung,
                                                   cst_km_abschreibung_extra)*(1+mietausfallwagnis),
                                               2),
      # Differenz zwischen Angebotsmiete und kostendeckender MIete: Gewinne und Kreditzahlungen
      cst_km_gewinnekredite_sqm = round(cst_nettokalt_sqm - cst_km_kostdeckend_nettokalt_sqm,
                                        2)
      ) %>% 
    # Wir joinen gleich, der Rest der Daten kann raus aus mw_data_with_kdm
    dplyr::select(is_id, 
                  starts_with("cst_km"))
  
  # Daten wieder zusammenführen: mw_data hat die Angebote ohne Baujahr drinnen, mw_data_with_km nicht
  mw_data_with_kdm <- left_join(mw_data, mw_data_with_kdm)
  
  return(mw_data_with_kdm)
}