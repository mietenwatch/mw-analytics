filter_offers_by_date <- function(mw_data_raw, from, to) {

  mw_data_raw_date_filtered <- mw_data_raw %>%
    filter(as.Date(is_eingestellt) >= as.Date(from)) %>% # "2018-04-01"
    filter(as.Date(is_eingestellt) <= as.Date(to)) 

  return(mw_data_raw_date_filtered)
}