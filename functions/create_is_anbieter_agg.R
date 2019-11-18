######################################################
#
# Create an aggregated version of is_anbieter
#
######################################################

# This aggregates sellers' daughter firms and renames them

# There dont seem to be any daughter firms of other big players 
# beyond the public housing companies in the dataset.

create_is_anbieter_agg <- function(is_anbieter) {
  
  is_anbieter_agg <- is_anbieter %>% 
    as.character() %>% 
    str_replace(regex("degewo.*", ignore_case = T),                                         
                "degewo AG") %>% 
    str_replace(regex("gewobag.*", ignore_case = T),
                "Gewobag AG") %>% 
    str_replace(regex("HOWOGE.*", ignore_case = T),
                "HOWOGE GmbH") %>% 
    str_replace(regex("GESOBAU.*", ignore_case = T),
                "GESOBAU AG") %>% 
    str_replace(regex("STADT UND LAND.*", ignore_case = T),
                "Stadt und Land GmbH") %>% 
    na_if("keine Angaben") %>% 
    factor()
  
  return(is_anbieter_agg)
}
