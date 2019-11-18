# Mietenwatch Analytics - `v1`

This repos includes all code for data import, processing, analysis and export for [mietenwatch.de](www.mietenwatch.de) analyzing Berlin rent market from April 2018 to end of September 2019.

`v1` represents the code basis for all statistics presented at the first release (8/9th Oct. 2019) of [mietenwatch.de](www.mietenwatch.de).

## Workflow and Usage

* `master.R` performs the following workflow in order
* `/functions` folder contains all helper functions used throughout the workflow

### Setup and data creation
`/setup` folder

1. Generate all geodata (`/setup/1_create_geodata.R`)
	* Imports geodata from different sources
	* Processes and reprojects geodata
	* Saves `mw_geodata.RData` in `/data/geodata` folder
2. Generate mietenwatch dataset (`/setup/2_load_libs_funs_data.R`)
	* Imports raw data that was already filtered in SQL database
	* Recodes, updates and creates columns
	* Saves `mw_data` in `/data` folder

### Analysis and export of results

`/analysis` folder

0. Export of jittered full dataset for maps (`/analysis/0_export_mw_data_jittered.R`)
1. Analysis "1. Leistbarkeit" and export of CSVs (`/analysis/1_leistbarkeit.R`)
2. Analysis "2. Wohnen als Ware" and export of CSVs (`/analysis/2_wohnen_als_ware.R`)
3. Analysis "3. Antworten" and export of CSVs (`/analysis/3_antworten.R`)

This project was funded by the German Federal Ministry of Education and Research within the Prototype Fund funding line organized by Open Knowledge Fundation.


![gefördert vom BMBF](https://raw.githubusercontent.com/mietenwatch/mietenwatch/master/static/bmbfgefoerdert.jpg)

