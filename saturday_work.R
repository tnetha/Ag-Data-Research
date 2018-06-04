library(jsonlite)
test <- fromJSON('/scratch/mentors/dbuckmas/json_files/46003.json')
View(test)

test$data$meta$ll[[1]]

get_loc_df <- function(json_file) {
  library(jsonlite)
  json_list <- fromJSON(json_file)
  loc_list <- json_list$data$meta$ll
  station_lat <- 0
  station_lon <- 0
  for (i in 1:length(loc_list)) {
    station_lat[i] <- loc_list[[i]][1]
    station_lon[i] <- loc_list[[i]][2]
  }
  loc_df <- data.frame(lat=station_lat,lon=station_lon)
  loc_df
}
library(stringr)
County_remove <- function(x) str_replace(x, ' County', '')
midwest_counties$V4 <- sapply(midwest_counties$V4, County_remove, USE.NAMES = F)

euc_dist <- function(vec1,vec2) {
  dist <- sqrt(sum((vec1-vec2)^2))
  dist
}

find_closest_station <- function(fips) {
  library(plyr)
  loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  county_center <- data.frame(Lon=newDF$avgLon[newDF$FIPScode == fips], Lat=newDF$avgLat[newDF$FIPScode == fips])
  vec <- 0
  for (i in 1:nrow(loc_df)) {
    vec[i] <- euc_dist(loc_df[i,],county_center)
  }
  which.min(vec)
  #distances <- laply(loc_df,euc_dist,county_center)
  #print(distances)
  #print(loc_df)
  #print(county_center)
}
index_46137 <- find_closest_station(46137)

test <- fromJSON('/scratch/mentors/dbuckmas/json_files/46137.json')
View(test)
class(newDF$avgLat)
test$data$data[3]

test_DF <- data.frame(date = seq(as.Date('1970-01-01'),as.Date('2015-12-31'),by='days'), pcpn = test$data$data[3])
