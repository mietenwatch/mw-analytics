####################################################
# Function to detect if apartment is subsidized
####################################################

# means "öffentlich geförderte Wohnungen". All WBS apartments.
# This includes apartments that are not social housing, but only WBS.
# Compare https://www.berliner-mieterverein.de/recht/infoblaetter/
# info-112-wohnungsarten-und-wohnformen-in-berlin-welche-ist-die-passende.htm#5-Sogenannte-WBS-Wohnungen

create_obj_is_gefoerdert <- function(mw_data_2) {
  
  # all potential patterns - based on manual look into data
  # should produce very few errors (false negatives mostly)
  wbs_patterns_positive <- c(
    "wbs",
    "wohnberechtigungsschein"
  )
  
  # These patterns should be excluded, they result in FALSE
  wbs_patterns_negative <- c(
    "kein wbs",
    "ohne wbs",
    "kein wohnberechtigungsschein",
    "ohne wohnberechtigungsschein",
    "wbs ist aktuell nicht erforderlich",
    "wbs ist nicht nötig",
    "wbs ist nicht notwendig",
    "wbs ist nicht erforderlich",
    "wohnberechtigungsschein ist nicht nötig",
    "wohnberechtigungsschein ist nicht notwendig",
    "wohnberechtigungsschein ist nicht erforderlich"
  )
  
  # Unclear whether it is subsidized housing or not.
  wbs_patterns_neutral <- c(
    "wbs-inhaber und berechtigte können diese wohnung ggf. zu vergünstigten konditionen anmieten"
  )
  
  # Create variable with regex
  mw_data_obj_is_gefoerdert <- mw_data_2 %>%
    mutate( # bind all descriptions in one long string
      des_all = (paste(des_eqp, des_objekt, des_sonstiges) %>% tolower())
    ) %>%
    mutate( # first du positive matching to create obj_is_gefoerdert
      obj_is_gefoerdert = replace_na(
        str_detect(
          des_all,
          paste(wbs_patterns_positive,
                collapse = "|"
          )
        ),
        FALSE
      )
    ) %>%
    mutate( # now do negative matching to update obj_is_gefoerdert
      obj_is_gefoerdert = case_when(
        str_detect(
          des_all,
          paste(wbs_patterns_negative,
                collapse = "|"
          )
        ) == TRUE ~ FALSE,
        TRUE ~ obj_is_gefoerdert
      )
    ) %>% 
    mutate( # now do neutral matching to update obj_is_gefoerdert
      obj_is_gefoerdert = case_when(
        str_detect(
          des_all,
          paste(wbs_patterns_neutral,
                collapse = "|"
          )
        ) == TRUE ~ NA,
        TRUE ~ obj_is_gefoerdert
      )
    ) %>% 
    select(-des_all)
  
  return(mw_data_obj_is_gefoerdert)
}
