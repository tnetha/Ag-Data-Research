state_fips <- function(states) {
  fips_code <- readRDS('https://www.dropbox.com/s/eurfbus8jwd7o8u/fips_code.rds?dl=0')
  fips_code$FIPS[fips_code$State %in% states]
}
