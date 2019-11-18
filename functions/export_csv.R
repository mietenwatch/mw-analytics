########################################################
#
# Export csvs for plots directly to mw-frontend 
#
########################################################


export_csv <- function(plot_data , filename) {
  # Export csvs for website to this path
  export_path <- "../mw-frontend/static/visualization-data/"
  
  write_csv(x = plot_data,
            path = paste0(export_path,
                         as.character(filename))
            )
  }

