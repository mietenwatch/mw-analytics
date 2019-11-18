################################################################
#
# Filter out short term rentals / "housing as a service"
#
################################################################

# Here we want to exclude atypical rent offers - boarding house like - 
# short term rentals, or "housing as a service".

# With the definition below, dataset loses around 1700 offers.
# Limits were checked in distributions, above 18sqm small, "normal" apartments
# start to appear.
# Homefully and similar sellers gets eliminated almost entirely here.
# --> Many very expensive offers drop out here.

exclude_shortterm_rentals <- function(mw_data) {

  # set limits
  max_cst_nettokalt_sqm <- 30
  min_obj_wohnflaeche <- 18

  mw_data_without_shortterm_rentals <- mw_data %>%
    # Filter short term rentals as COMBINED condition
    filter(
      cst_nettokalt_sqm <= max_cst_nettokalt_sqm,
      obj_wohnflaeche > min_obj_wohnflaeche
      )

  return(mw_data_without_shortterm_rentals)
}
