# load libraries
library(tidyverse)
library(ggplot2)
library(data.table)
library(maps)
library(stringr)

#### building a data frame of just the elements we're concerened with right now ####

# make a list of the 'state,county' to use as a cross reference with other data sets
counties = map_data('county')
counties$statecounty = paste(counties$region, counties$subregion, sep = ',')

# just the midwest ones
midwestCounties = counties[counties$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

# this is used to cross reference counties with their lats and lons
groups = unique(midwestCounties$group)

# pull each county once since the previous county data has multiple entries of each county
statecounty = unique(midwestCounties$statecounty)

# start the df that will eventually contain all of the data
centroids = data.frame(groups, statecounty)

# pulling fips codes from the Census Bureau
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

# function to get the center of each county
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

# fuction to get the lats and lons of all weather stations in each county from the
# json files downloaded previously
get_loc_df <- function(json_file) {
  library(jsonlite)
  json_list <- fromJSON(json_file)
  loc_list <- json_list$data$meta$ll
  station_lat <- 0
  station_lon <- 0
  for (i in 1:length(loc_list)) {
    if (is.null(loc_list[[i]][1])) {
      station_lon[i] = NA
    } else {
      station_lon[i] = loc_list[[i]][1]
    }
    if (is.null(loc_list[[i]][2])) {
      station_lat[i] = NA
    } else {
      station_lat[i] = loc_list[[i]][2]
    }
  }
  loc_df <- data.frame(lon = station_lon,lat = station_lat)
  loc_df
}

# function to calculate distance between stations
euc_dist <- function(vec1,vec2) {
  dist <- sqrt(rowSums((vec1-vec2)^2))
}

# function to get the yields, states, counties, fips codes, years for a particular crop 
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

# function used to find the closest station to the center of a county
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

# function used to get the lat lon of the closest station
# commented out is the code to get the precip data for that station
getPrecipClosestStn = function(fips) {
  loc_df <- get_loc_df(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  jsonData = as.data.frame(fromJSON(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))) # precip data for all stations
  loc_df[find_closest_station(fips),]
  # dates = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 1)
  # precipData = data.frame(date = dates, precip = pcpn)
  # precipData
}

# get the yields, etc for wheat
wheat = getYields("/scratch/mentors/dbuckmas/head.txt")

# get the centers of all midwest counties
test = sapply(centroids$group, getCenter)
test = as.data.frame(t(test))
names(test) = c('long', 'lat')

# add the centers to the df with all the data
centroids = cbind(centroids, test)

# merge the yields df with the all-data df using the 'state,county' field
centroids = merge(centroids, wheat, by = 'statecounty')

# merge the fips codes again
centroids$FIPScode = paste(centroids$STATE_FIPS_CODE, centroids$COUNTY_CODE, sep = '')

# just pull out the columns we need, aka take out the columns only used to cross reference
allData = centroids[, c(11,1,3,4,9,10)]

# take out one county in illinois because it was being dumb and had no weather stations
allData = allData[-c(1100:1124),]
fips_code = fips_code[-44]
<<<<<<< HEAD
closest_stations <- sapply(fips_code, getPrecipClosestStn)
write.csv(closest_stations, '/scratch/mentors/dbuckmas/closest.csv')
=======

# calculate the closest station's lats and lons for each county and store in a csv file
# so we don't have to run it every time bc it takes forever and is being stupid
write.csv(sapply(fips_code, getPrecipClosestStn), '/scratch/mentors/dbuckmas/closest.csv')
>>>>>>> 85748e182d466ad0bf083282276ea63b7fd5713a
