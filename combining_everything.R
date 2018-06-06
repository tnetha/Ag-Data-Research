library(tidyverse)
library(ggplot2)
library(data.table)
library(maps)
library(stringr)

counties = map_data('county')
counties$statecounty = paste(counties$region, counties$subregion, sep = ',')
midwestCounties = counties[counties$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]
groups = unique(midwestCounties$group)
statecounty = unique(midwestCounties$statecounty)
centroids = data.frame(groups, statecounty)

getCenter = function(Group) {
  lat_lons = subset(midwestCounties, midwestCounties$group == Group)
  maxLon = max(lat_lons$long)
  minLon = min(lat_lons$long)
  maxLat = max(lat_lons$lat)
  minLat = min(lat_lons$lat)
  avgLon = (maxLon + minLon) / 2
  avgLat = (maxLat + minLat) / 2
  latLon = c(avgLon, avgLat)
}

getYields = function(file_name) {
  crop = read.delim(file_name, header=T, sep ="\t")
  crop = subset(crop, SOURCE_DESC == 'SURVEY')
  crop = subset(crop, AGG_LEVEL_DESC == 'COUNTY')
  crop = subset(crop, SHORT_DESC == 'WHEAT - YIELD, MEASURED IN BU / ACRE') ## change this to your crop's yield column name
  crop = crop[, c(15, 21, 17, 22, 31, 38)]
  crop$STATE_FIPS_CODE = sprintf('%02d',crop$STATE_FIPS_CODE)
  crop$COUNTY_CODE = sprintf('%03d',crop$COUNTY_CODE)
  cropCut = subset(crop, STATE_NAME %in% c("OHIO","INDIANA","ILLINOIS","IOWA","MISSOURI",
                                                       "NEBRASKA","KANSAS","SOUTH DAKOTA","MINNESOTA"))
  cropCut$statecounty = data.frame(paste(cropCut$STATE_NAME, cropCut$COUNTY_NAME, sep = ","))
  cropCut$statecounty = sapply(cropCut$statecounty, tolower)
  cropCut = subset(cropCut, YEAR >= 1980 & YEAR <= 2010)
  crop80 = subset(cropCut, YEAR >= 1981 & YEAR <= 1990)
  crop90 = subset(cropCut, YEAR >= 1991 & YEAR <= 2000)
  crop00 = subset(cropCut, YEAR >= 2001 & YEAR <= 2010)
  return(cropCut)
}

find_closest_station <- function(fips) {
  loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  county_center <- data.frame(Lon=rep(allData$long[allData$FIPScode == fips][1], nrow(loc_df)), Lat=rep(allData$lat[allData$FIPScode == fips][1],nrow(loc_df)))
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
loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips_code[1],'.json',sep = ''))
fips_code[1]
getPrecipClosestStn = function(fips) {
  loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  jsonData = as.data.frame(fromJSON(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))) # precip data for all stations
  loc_df[find_closest_station(fips),]
  # dates = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 1)
  # precipData = data.frame(date = dates, precip = pcpn)
  # precipData
}

wheat = getYields("/scratch/mentors/dbuckmas/head.txt")

test = sapply(centroids$group, getCenter)
test = as.data.frame(t(test))
names(test) = c('long', 'lat')
temp = sapply(fips_code, getPrecipClosestStn)
centroids = cbind(centroids, test)

centroids = merge(centroids, wheat, by = 'statecounty')
centroids$FIPScode = paste(centroids$STATE_FIPS_CODE, centroids$COUNTY_CODE, sep = '')
allData = centroids[, c(11,1,3,4,9,10)]
allData = allData[-c(1100:1124),]
fips_code = fips_code[-44]

fips_code[44]
t = fromJSON('/scratch/mentors/dbuckmas/json_files/',17087,'.json',sep = '')
