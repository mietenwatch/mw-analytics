# Function to remove unncescessary variables in preparing data

remove_unnesc_cols <- function(mw_data) {
  
  mw_data <- mw_data %>%
    dplyr::select(-geo_strasse, # geo_data unsuable. We have to map the coords by ourselves.
                  -geo_kreis,
                  -geo_bundesland,
                  -geo_land,
                  -geo_bezirk,
                  -des_lage,
                  -des_fotoanzahl,
                  -obj_foto)  
    return(mw_data)

  }