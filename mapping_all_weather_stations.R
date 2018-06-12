
get_loc_df <- function(fips) {
  library(jsonlite)
  json_file = paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = '')
  json_list <- fromJSON(json_file)
  loc_list <- json_list$data$meta$ll
  station_lat <- 0
  station_lon <- 0
  for (i in 1:length(loc_list)) {
    # if (is.null(loc_list[[i]][1])) {
    #   station_lon[i] = NA
    # } else {
      station_lon[i] = loc_list[[i]][1]
    # if (is.null(loc_list[[i]][2])) {
    #   station_lat[i] = NA
    # } else {
      station_lat[i] = loc_list[[i]][2]
  }
  loc_df <- data.frame(lon = station_lon,lat = station_lat)
}

# getting fips codes
counties1 <- read.table('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt',header = F, sep = ',', 
                        fill = T, stringsAsFactors = F, quote = '')

# pull out just the midwest ones
midwest_counties <- counties1[counties1$V1 == 'OH' | counties1$V1 == 'IN' | counties1$V1 == 'IL'
                              | counties1$V1 == 'IA' | counties1$V1 == 'MO' | counties1$V1 == 'KS' |
                                counties1$V1 == 'NE' | counties1$V1 == 'SD' | counties1$V1 == 'MN',]

# keep the 0s that R wants to delete
midwest_counties$V2 <- sprintf('%02d',midwest_counties$V2)
midwest_counties$V3 <- sprintf('%03d',midwest_counties$V3)

# combine the state and county fips codes
fips_code <- paste(midwest_counties$V2,midwest_counties$V3,sep = '')
fips_code = as.numeric(as.character(fips_code))
fips_code <- fips_code[!(fips_code == 17087 | fips_code == 46113 | fips_code == 29019)]

getStations = sapply(fips_code, get_loc_df, simplify = F)

