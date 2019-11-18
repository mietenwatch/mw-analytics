######################################################
#
# Find big players by number of valid offers
#
######################################################

find_big_players <- function(mw_data_without_duplicates) {
  mw_data_without_duplicates %>%
    # Create Big Player col via count
    # count of offers by seller "is_anbieter_agg_anzahl"
    add_count(is_anbieter_agg,
      sort = TRUE,
      name = "is_anbieter_agg_anzahl"
    ) %>%
    # logical whether is amongst 10 sellers with most offers
    mutate(is_anbieter_bigplayer = ifelse(group_indices(
      .,
      factor(is_anbieter_agg,
        levels = unique(is_anbieter_agg)
      )
    ) <= 10,
    TRUE,
    FALSE
    ))
}
