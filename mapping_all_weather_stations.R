install.packages('ggmap')
library(ggmap)
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

getStations[[1]]

getStations = do.call(rbind, getStations)

midwest_center = as.numeric(geocode("Minneapolis, MN"))
cleMap = ggmap(get_googlemap(center = midwest_center, zoom = 6, maptype = 'roadmap'))
cleMap = cleMap + geom_point(data = getStations[5183:6075,], size = 1, mapping = aes(x = lon, y = lat, color = lat))
cleMap

# IL - 1480
# IN - 2850
# IA - 3570
# KS - 5182
# 


# states = map_data('state')
# mdw_states = subset(states, region %in% c("ohio", "indiana", "illinois", "iowa", 
#                                           "minnesota", "missouri", "kansas", "nebraska",
#                                           "south dakota"))
# mdw_base = ggplot(data = mdw_states, mapping = aes(x = mdw_states$long, y = mdw_states$lat, group=mdw_states$group)) + coord_fixed(1.3) + geom_polygon(color = 'black', fill = NA, size = 0.2)
# mdw_base = mdw_base + geom_point(data = getStations, color = 'red', size = 0.1)
# map1
# mdw_base
