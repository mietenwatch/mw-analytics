################################################################
#
# Filter out all duplicates as sharply as possible
#
################################################################

# With the duplicate definitions below around 16 000 offers will be excluded 
# from the incoming data. Cross-Tabulations were carried out for validation.

# Amongst others,
# Degewo loses a lot of offers via is_dup1 flag (SAME floor duplicates),
# Romi Immobilien loses a lot of offers in is_dup2 flag (DIFFERENT floor duplicates.)

# There are no cases where there is no seller and no house number info while having valid 
# geolocation. 
# -> We can rule out that there is a duplicate "false negative" blindspot in the data.

exclude_duplicates <- function(mw_data) {

  # Define minimum period of days after which an offer
  # will be allowed to reappear in the dataset
  min_days_for_reoffer <- 92 # 3 months + 2 days of buerocracy equals minimum rent period

  # We exclude duplicates in two steps:

  #######################################
  #### 1. Flag SAME floor area dups #####
  #######################################

  # Here we define as duplicate all offers with:
  # SAME seller, street, house nr, floor nr, floor area
  # AND a repost of that flat within min_days_for_reoffer

  mw_data_dups1 <- mw_data %>%
    filter(
      # When there is no street or floor nr. info, it is likely that is not a duplicate.
      # Hence we exclude them from duplicate flagging.
      geo_strasselang != "no_information",
      !is.na(geo_stockwerk) # there remain no missings on geo_hausnummer after this filter
    ) %>%
    dplyr::select(
      is_id,
      is_anbieter, geo_strasselang, geo_hausnummer, geo_stockwerk, obj_wohnflaeche,
      is_eingestellt
    ) %>%
    group_by(
      is_anbieter, geo_strasselang, geo_hausnummer, geo_stockwerk, obj_wohnflaeche
    ) %>%
    # Sort by date offer went online to determine whether
    # repost was within min_days_for reoffer or not.
    arrange(is_eingestellt, .by_group = TRUE) %>%
    mutate(
      is_eingestellt_lead = lag(is_eingestellt), # date last time offer went online
      is_eingestellt_leaddiff = is_eingestellt - is_eingestellt_lead, # difference between last time and this time
      # Now flag duplicates:
      is_dup1 = case_when(
        is.na(is_eingestellt_lead) ~ FALSE, # no lead means offer didnt go online before -> no dup
        is_eingestellt_leaddiff <= min_days_for_reoffer ~ TRUE, # lead exists and is lower than min_days_for_reoffer --> dup
        TRUE ~ FALSE
      ) # all other cases --> no dup
    ) %>%
    ungroup()
  # View(mw_data_dups1)

  # Delete all cols that we dont need
  mw_data_dups1_for_join <- mw_data_dups1 %>%
    dplyr::select(
      is_id,
      is_dup1
    )

  ###########################################
  #### 2. Flag DIFFERENT floor area dups ####
  ###########################################

  # Here we define as duplicate all offers with:
  # SAME seller, street, house nr, floor nr
  # AND a repost of that flat within min_days_for_reoffer
  # AND floor area of date ordered offers stays within +-0.5sqm

  # --> some sellers appear to repost the same apartment again and again
  # camouflaging that it is the very same apartment by changing the floor area
  # by a few decimals.

  # Example: First post  "2018-11-12" Floor area "76,00 sqm" (same address, seller, floor nr.)
  #          Second post "2018-11-14" Floor area "76,01 sqm" (same address, seller, floor nr.)
  # --> In this case we would flag the second post as a duplicate and only leave the first
  # in the dataset.

  mw_data_dups2 <- mw_data %>%
    filter(
      # When there is no street or floor nr. info, it is likely that is not a duplicate.
      # Hence we exclude them from duplicate flagging.
      geo_strasselang != "no_information",
      !is.na(geo_stockwerk)
    ) %>%
    dplyr::select(
      is_id,
      is_anbieter, geo_strasselang, geo_hausnummer, geo_stockwerk, obj_wohnflaeche,
      is_eingestellt
    ) %>%
    # note here that obj_wohnflaeche ist not part of the grouping! (different from Step 1!)
    group_by(
      is_anbieter, geo_strasselang, geo_hausnummer, geo_stockwerk
    ) %>%
    arrange(
      is_eingestellt, obj_wohnflaeche,
      .by_group = TRUE
    ) %>%
    mutate(
      obj_wohnflaeche_int_lo = obj_wohnflaeche - 0.5, # create accepted range of floor area
      obj_wohnflaeche_int_hi = obj_wohnflaeche + 0.5,
      obj_wohnflaeche_lead = lag(obj_wohnflaeche), # floor area of previous post
      obj_wohnflaeche_inint = between(
        obj_wohnflaeche_lead, # check if floor area is with accepted range
        obj_wohnflaeche_int_lo,
        obj_wohnflaeche_int_hi
      ),
      is_eingestellt_lead = lag(is_eingestellt),
      is_eingestellt_leaddiff = is_eingestellt - is_eingestellt_lead,
      is_dup2 = case_when(
        is.na(is_eingestellt_lead) ~ FALSE,
        is_eingestellt_leaddiff <= min_days_for_reoffer & obj_wohnflaeche_inint == TRUE ~ TRUE, # flagging dups of 2.Step
        TRUE ~ FALSE
      )
    ) %>%
    ungroup()

  # Delete all cols that we dont need
  mw_data_dups2_for_join <- mw_data_dups2 %>%
    dplyr::select(
      is_id,
      is_dup2
    )

  #######################################################
  #### Join all datasets and filter out flagged dups ####
  #######################################################

  # Left join with original dataset of first flag (is_dup1)
  mw_data_with_dupflag1 <- left_join(
    mw_data,
    mw_data_dups1_for_join
  )

  # Left join with first flag dataset and second flag dataset
  mw_data_with_dupflag12 <- left_join(
    mw_data_with_dupflag1, # contains flag 1 (is_dup1)
    mw_data_dups2_for_join # contains flag 2 (is_dup2)
  ) %>%
    # Final duplication flag is given if any of the flags is TRUE
    mutate(
      is_dup = case_when(
        is_dup1 == TRUE | is_dup2 == TRUE ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # exclude all offers flagged as duplicates
  mw_data_without_duplicates <- mw_data_with_dupflag12 %>%
    filter(is_dup == FALSE) %>% 
    select(
      -is_dup1,
      -is_dup2,
      -is_dup
    )

  return(mw_data_without_duplicates)
}
