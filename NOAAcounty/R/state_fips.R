state_fips <- function(states) {
  fips_code <- readRDS(gzcon(url('https://github.com/tnetha/Ag-Data-Research/blob/master/Data%20Sources/fips_code.rds?raw=true')))
  fips_code$FIPS[fips_code$State %in% states]
}
