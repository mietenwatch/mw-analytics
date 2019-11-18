#### Secondary data cleaning & Sorting ####
###########################################

final_clean_and_sort <- function(mw_data) {
  
  cleaned_mw_data <- mw_data %>% 
    mutate(
      pol_rentLimit = na_if(pol_rentLimit, 0),
      # encoding problem with "é" 
      geo_lor_planungsraum = str_replace(as.character(geo_lor_planungsraum),"¿", "é") %>% factor()) %>% 
    # we delete some unnecessary cols that came in through sf operations
    dplyr::select(-starts_with("pol_milieu_b")) %>% 
    dplyr::select(-GK_ABL_DAT) %>% 
    # was needed for social housing detection
    dplyr::select(-starts_with("des_"))
  
  # clean levels of Baujahr groups  
  cleaned_mw_data$obj_baujahr_5y  <- clean_baujahr_groups(cleaned_mw_data$obj_baujahr_5y)
  cleaned_mw_data$obj_baujahr_10y <- clean_baujahr_groups(cleaned_mw_data$obj_baujahr_10y)
  
  # Reorder Variables alphabetically
  cleaned_sorted_mw_data <- cleaned_mw_data %>% 
    dplyr::select(sort(peek_vars())) %>%
    # then reorder Variables according to prefix
    dplyr::select(starts_with("is_"),
                  starts_with("geo_"),
                  starts_with("obj_"),
                  starts_with("eqp_"),
                  starts_with("egy_"),
                  starts_with("des_"),
                  starts_with("cst_"),
                  starts_with("rid_"),
                  starts_with("pol_")) 
  
  return(cleaned_sorted_mw_data)
  
}