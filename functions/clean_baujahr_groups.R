clean_baujahr_groups <- function(obj_baujahr_Xy) {
  levels(obj_baujahr_Xy) <- levels(obj_baujahr_Xy) %>%
    str_replace(",", "-") %>%
    str_replace("\\(", "") %>%
    str_replace("\\]", "") %>%
    str_replace("2020", "2019")
  
  return(obj_baujahr_Xy)
}