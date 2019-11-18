#++++++++++++++++++++++++++++++++++++++++++++++
# Perfom all mietenwatch analytics tasks
#++++++++++++++++++++++++++++++++++++++++++++++

### A.) Setup and data creation ####
#+++++++++++++++++++++++++++++++++++

# 1 . Generate all geodata (= no mietenwatch data)
source("setup/1_create_geodata.R",
       print.eval  = TRUE)

# 2. Generate mietenwatch data

# Including download from SQL database, filtering, wrangling 
# and creation of new variables
# -- may take a while, resource intensive operations --
source("setup/2_load_functions_and_create_mietenwatch_data.R",
       print.eval  = TRUE)

#### B.) Analysis & Export ####
#++++++++++++++++++++++++++++++

# 0. Export entire jittered dataset for maps
source("analysis/0_export_mw_data_jittered.R",
       print.eval  = TRUE)

# 1. Leistbarkeit
source("analysis/1_leistbarkeit.R",
       print.eval  = TRUE)

# 2. Wohnen als Ware
source("analysis/2_wohnen_als_ware.R",
       print.eval  = TRUE)

# 3. Antworten
source("analysis/3_antworten.R",
       print.eval  = TRUE)

