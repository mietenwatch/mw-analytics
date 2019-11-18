##################################################################
#
# Imports & filters data, converts/generates cols, saves mw_data 
#
##################################################################

create_mw_data <- function() {
  
  # Only use this if you want to download fresh data from
  # sql database 
  mw_data_raw <- download_is24view_from_database()
  
  mw_data_without_geodata <- mw_data_raw %>%
    filter_offers_by_date(
      from = "2018-04-01",
      to = "2019-10-09") %>% 
    # kick out implausible offers 
    exclude_implausible_offers() %>%
    # kick out unnecessary variables --> they bloat the dataset
    remove_unnesc_cols() %>%
    # convert to correct types & levels - no value mutation
    convert_cols() %>% 
    # create new cols from existing ones or correct values
    create_cols() %>% 
    # exclude short term rental offers - "housing as a service" out
    exclude_shortterm_rentals() %>% 
    # exclude all offers that appear as duplicates
    exclude_duplicates() %>% 
    # now with correct number of cases we can calculate 
    # who the big players are according to valid offers in dataset
    find_big_players()

  # Create SF object with geometries & geodata
  mw_data_sf <- mw_data_without_geodata %>%
    create_mw_data_sf() %>% 
    final_clean_and_sort()

  # We kick out the geometries here for "mw_data" dataset.
  # Variables such as geo_sbahn, geo_lat, geo_lon etc. stay
  mw_data <- st_set_geometry(mw_data_sf, NULL)

  ### Save all data locally
  # Create new dataname first
  mw_data_filename <- paste("mw_data_",
    Sys.Date(),
    ".RData",
    sep = "")
  
  save(mw_data_sf, # with geometries
       mw_data, # without geometries
       file = paste("./data/",
                    mw_data_filename,
                    sep = ""))

  }
