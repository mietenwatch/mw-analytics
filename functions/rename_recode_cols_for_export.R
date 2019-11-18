# Function to rename and recode cols to create publisheable names for plotting
# and to round numeric cols be used before write_csv and ggplot

rename_recode_cols_for_export <- function ( tbbl, rename=T, mutate=T ) {
  
  stopifnot("tbl" %in% class(tbbl) )
  #ungroup to avoid error of mutating a groupign variable
  tbbl <- tbbl %>%  ungroup()
 
if (mutate==TRUE) {
  #===== 1) Mutate variables ========
  mutate_var <- function (tibb, mutate.tib) {
    #inner function to mutate (round) specifically by variable if variables exist in tibble
    #for loop is a shitty hack but I couldnt figure out how to mutate variables by individual functions depending on variable name, given the variable exists in the data
    
    #select only existing vars in tibb
    n.xst <- names(mutate.tib) [ names (mutate.tib) %in% names( tibb ) ]
    #apply mutating functions to each variable 
    for (var in n.xst) {
      tibb <- tibb %>% mutate_at(.vars = var, .funs=mutate.tib %>% select(var) %>% unlist) 
    }
    return (tibb)
  }
  
  #create tibble of functions named with variables to be mutated
  mutating <- tibble ( #runden 
    obj_wohnflaeche = list( ~round(., 0) ),
    cst_gesamtmiete = list( ~round(., 0) ),
    cst_gesamtmiete_sqm =list( ~round(., 2)),
    cst_nebenkosten =list( ~round(., 0)),
    cst_nebenkosten_sqm =list( ~round(., 2)),
    cst_nettokalt =list( ~round(., 0)),
    cst_nettokalt_sqm =list( ~round(., 2)),
    cst_md_nettokalt =list( ~round(., 0)),
    cst_md_nettokalt_sqm =list( ~round(., 2)),
    cst_md_nebenkosten_sqm =list( ~round(., 2)),
    cst_md_gesamtmiete_sqm =list( ~round(., 3)),
    cst_md_gesamtmiete =list( ~round(., 0)),
    cst_md_diff_gesamtmiete_sqm =list( ~round(., 2)),
    cst_over_rid_LocalRent_sqm_rel = list( ~round(.*100, 0)),
    cst_over_rid_LocalRent_sqm = list( ~round(., 2)),
    # affordable area fox x persons: 
    `1 Person` = list( ~round(., 1)),
    `2 Personen` = list( ~round(., 1)),
    `3 Personen` = list( ~round(., 1)),
    `4 Personen` = list( ~round(., 1)),
    `5 Personen und mehr` = list( ~round(., 1)),
    # share of affordable apartments:
    `Leistbare Wohnungen` = list( ~round(., 1)),
    `Nicht-leistbare Wohnungen` = list( ~round(., 1)),
    # factor level update
    is_anbieter_landeseigene = list( ~factor(., labels = c("privat", 
                                                           "städtisch"))),
    # shorten names of sellers
    is_anbieter_agg = list( ~fct_recode(., 
                                        Degewo = "degewo AG",
                                        `Deutsche Wohnen` = "Deutsche Wohnen Gruppe",
                                        Vonovia = "Vonovia SE",
                                        Gewobag = "Gewobag AG",
                                        Akelius = "AKELIUS GmbH",
                                        `Ado Immobilien` = "ADO Immobilien Management GmbH",
                                        Howoge = "HOWOGE GmbH",
                                        Gesobau = "GESOBAU AG",
                                        `Romi Immobilien` = "ROMI Immobilien GmbH",
                                        Homefully = "homefully GmbH",
                                        Immonexxt = "Immonexxt GmbH",
                                        `Stadt und Land` = "Stadt und Land GmbH",
                                        Covivio = "Covivio Immobilien GmbH")
    ),
    geo_ortsteil_id = list( ~ as.character(.)) # convert num to char for to make congruent with old code
  )
  
  #mutate variables
  tbbl <- mutate_var (tbbl, mutating)
  
}   
  
if (rename==TRUE)  {
  #======= 2) rename Variables ==========
  #create tibble mapping old names to new names
  renaming <- tibble (  is_anbieter_agg     = "Anbieter",
                        geo_bezirk = "Bezirk",
                        geo_sbahn           = "S-Bahn-Ring",
                        `S-Bahn Ring`       = "S-Bahn-Ring", 
                        geo_lor_planungsraum= "Planungsraum",
                        geo_stockwerk       = "Stockwerk der Wohnung",
                        obj_wohnflaeche     = "Wohnfläche in m²",
                        obj_baujahr         = "Baujahr",
                        obj_baujahr_5y      = "Baujahr aggregiert (5 J.)",
                        obj_baujahr_10y     = "Baujahr aggregiert (10 J.)",
                        obj_zustand         = "Zustand",
                        eqp_balkon          = "Balkon",
                        eqp_aufzug          ="Aufzug",
                        eqp_garten          = "Garten",
                        eqp_moebliert       = "moebliert",
                        cst_gesamtmiete_sqm = "Gesamtmiete in €/m²",
                        cst_nettokalt_sqm   = "Nettokaltmiete in €/m²",
                        cst_nebenkosten_sqm = "Nebenkosten in €/m²",
                        cst_md_nettokalt_sqm = "Nettokaltmiete mit Mietendeckel in €/m²",
                        cst_md_nebenkosten_sqm = "Nebenkosten mit Mietendeckel in €/m²",
                        cst_md_gesamtmiete_sqm = "Gesamtmiete mit Mietendeckel in €/m²",
                        cst_md_diff_gesamtmiete_sqm = "Differenz Nettokaltmiete ohne/mit Mietendeckel in €/m²",
                        obj_baujahr_md_klasse = "Baujahresklasse",
                        obj_alter = "Gebäudealter in Jahren",
                        rid_objectStatus_19 = "Wohnlage",
                        rid_noiseStatus_19  = "Lärmstatus",
                        is_anbieter_agg_anteil = "Marktanteil",
                        is_anbieter_landeseigene = "Eigentümer",
                        cst_over_rid_LocalRent_sqm_rel = "Überschreitung des Mietspiegels in %",
                        cst_over_rid_LocalRent_sqm = "Überschreitung des Mietspiegels in €/m²",
                        n = "Anzahl der Angebote")
  
  #index to identify vars existing  in tbbl
  n.exist <- names (tbbl) [ names(tbbl) %in% names (renaming) ]
  #rename all vars of tbbl that exist in renaming-tibble
  names <- function (old.n) unlist( dplyr::select(renaming, old.n) ) 
  
tbbl <- tbbl %>% 
    
    rename_at ( .vars= vars(n.exist),
                .funs= names)
  
}
  return(tbbl)
}
