#++++++++++++++++++++++++++++++++++++++++++++++
# MW ANALYSTICS MASTER
#++++++++++++++++++++++++++++++++++++++++++++++

### A.) Setup ####
#++++++++++++++++++

# Datengenerierung geodata
source("setup/1_create_geodata.R",
       print.eval  = TRUE)

# Datengenerierung mit Download von SQL und wrangling
source("setup/2_load_libs_funs_data.R",
       print.eval  = TRUE)

#### B.) Analyse & Export ####
#++++++++++++++++++++++++++++++++

# 0. Export of jittered complete data for maps to api
source("analysis/0_export_mw_data_jittered.R",
       print.eval  = TRUE)

## Kapitel ##
#-------------

# 1. Leistbarkeit
source("analysis/1_leistbarkeit.R",
       print.eval  = TRUE)

# 2. Wohnen als Ware
source("analysis/2_wohnen_als_ware.R",
       print.eval  = TRUE)

# 3. Antworten
source("analysis/3_antworten.R",
       print.eval  = TRUE)
