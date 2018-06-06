library(jsonlite)
library(stringr)
library(plyr)
library(mefa)

#test <- fromJSON('/scratch/mentors/dbuckmas/json_files/46003.json')
#View(test)

#test$data$meta$ll[[1]]

# function get the lat/lons of stations in a county with a specific fips code aka json file
get_loc_df <- function(json_file) {
  library(jsonlite)
  json_list <- fromJSON(json_file)
  loc_list <- json_list$data$meta$ll
  station_lat <- 0
  station_lon <- 0
  for (i in 1:length(loc_list)) {
    station_lon[i] <- loc_list[[i]][1]
    station_lat[i] <- loc_list[[i]][2]
  }
  loc_df <- data.frame(lon = station_lon,lat = station_lat)
  loc_df
}

# remove the word county from the FIPS code data we downloaded from the Census Bureau (i think)
County_remove <- function(x) str_replace(x, ' County', '')
midwest_counties$V4 <- sapply(midwest_counties$V4, County_remove, USE.NAMES = F)

# creating distance formula function
euc_dist <- function(vec1,vec2) {
  dist <- sqrt(rowSums((vec1-vec2)^2))
}

# figure out which station is the closest:
# get the lats and lons from a file with a specific fips code
# use the distance formula to find the shortest distance
# returns the index of the closest station to the center
find_closest_station <- function(fips) {
  loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  county_center <- data.frame(Lon=rep(newDF$avgLon[newDF$FIPScode == fips], nrow(loc_df)), Lat=rep(newDF$avgLat[newDF$FIPScode == fips],nrow(loc_df)))
  # vec <- 0
  # for (i in 1:nrow(loc_df)) {
  #   vec[i] <- euc_dist(loc_df[i,],county_center)
  # }
  # which.min(vec)
  distances <- euc_dist(loc_df, county_center)
  which.min(distances)
  #distances <- sapply(loc_df,euc_dist,county_center)
  #print(distances)
  #print(loc_df)
  #print(county_center)
}

# testing using fips 46137
find_closest_station(46137)
loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',46137,'.json',sep = ''))
loc_df[find_closest_station(46137),]

getPrecipClosestStn = function(fips) {
  loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  jsonData = as.data.frame(fromJSON(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))) # precip data for all stations
  pcpn = jsonData$data.data[find_closest_station(fips)]
  dates = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 1)
  precipData = data.frame(date = dates, precip = pcpn)
  precipData
}
getPrecipClosestStn(39035)

index_46137 <- find_closest_station(46137)

test <- fromJSON('/scratch/mentors/dbuckmas/json_files/46137.json')
View(test)
class(newDF$avgLat)
test$data$data[3]

test_DF <- data.frame(date = seq(as.Date('1970-01-01'),as.Date('2015-12-31'),by='days'), pcpn = test$data$data[3])
