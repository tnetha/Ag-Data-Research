NOAA_json <- function(fips_codes, Start_date, End_date, Weather_element) {
  library(jsonlite)
  library(tibble)
  library(parallel)
  # Pulls data from MultiStnData at rcc-acis.org
  # Inputs:
  ## fips_codes = county FIPS codes
  ## Start_date
  ## End_date
  ## Weather_element
  county_dfs_list <- mclapply(fips_codes, function(x) {
    county_json <- fromJSON(paste('http://data.rcc-acis.org/MultiStnData?county=',
                                  x,'&sdate=',Start_date,'&edate=',End_date,'&elems=',Weather_element, sep = ''))
    county_json_mat <- tryCatch({
      do.call(cbind, county_json$data$data)
      }, error = function(err) {return(matrix(rep(NA_integer_, times = 4 * length(seq(as.Date(Start_date), as.Date(End_date), by = 'days'))), ncol = 4))}
      )
    county_json_mat[county_json_mat == 'M'] <- NA
    county_json_mat[county_json_mat == 'T'] <- .05
    county_json_mat <- matrix(as.numeric(unlist(county_json_mat)), nrow = nrow(county_json_mat))
    county_json_mat_means <- rowMeans(county_json_mat, na.rm = T)
    county_df <- tibble(dates = seq(as.Date(Start_date), as.Date(End_date), by = 'days'), fips_code = x,
                        elem_mean = county_json_mat_means)
    county_df
  }, mc.cores = 40)
  counties_df <- do.call(rbind, county_dfs_list)
  counties_df
}

