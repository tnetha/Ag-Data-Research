library(ggthemes)
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

# IL - 1480
# IN - 2886
# IA - 3579
# KS - 5182
# MN - 6073
# MO - 7172
# NE - 9077
# OH - 9788
# SD - 10615

states = map_data('state')
counties = map_data('county')
counties_mdw = subset(counties, region %in% c("ohio", "indiana", "illinois", "iowa", 
                                              "minnesota", "missouri", "kansas", "nebraska",
                                              "south dakota"))
mdw_states = subset(states, region %in% c("ohio", "indiana", "illinois", "iowa", 
                                           "minnesota", "missouri", "kansas", "nebraska",
                                           "south dakota"))
getStations$state = c(rep('IL', times = 1480), rep('IN', times = 1406), rep('IA', times = 693), rep('KS', times = 1603), rep('MN', times = 891), rep
                       ('MO', times = 1099), rep('NE', times = 1905), rep('OH', times = 711), rep('SD', times = 827))
a = scale_colour_discrete(h = c(120, 300))
mdw_base = ggplot() + coord_fixed(1.3) + geom_point(data = getStations, aes(x=lon, y=lat, color = state), size = 0.1) + geom_polygon(data=counties_mdw, mapping = aes(x = long, y = lat, group = group),color = 'black', fill = NA, size = 0.1) + 
  geom_polygon(data = mdw_states, mapping = aes(x = mdw_states$long, y = mdw_states$lat, group=mdw_states$group),color = 'black', fill = NA, size = 0.7) + a + 
  xlab('Longitude') + ylab('Latitude') + theme_light()
mdw_base
 
 