pull_state_codes <- function(state_abb) {
  # pulling fips codes from the Census Bureau
  counties1 <- read.table('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt',header = F, sep = ',', 
                        fill = T, stringsAsFactors = F, quote = '')

  # pull out just the midwest ones
  counties <- counties1[counties1$V1 %in% state_abb,]

  # keep the 0s that R wants to delete
  counties$V2 <- sprintf('%02d',counties$V2)
  counties$V3 <- sprintf('%03d',counties$V3)

  # combine the state and county fips codes
  fips_code <- paste(counties$V2,counties$V3,sep = '')
  fips_code = as.numeric(as.character(fips_code))
  fips_code
}

length(pull_state_codes(c('IN','OH')))
