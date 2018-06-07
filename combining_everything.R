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
fips_code <- paste(midwest_counties$V2,midwest_counties$V3,sep = '')
fips_code = as.numeric(as.character(fips_code))

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

euc_dist <- function(vec1,vec2) {
  dist <- sqrt(rowSums((vec1-vec2)^2))
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
centroids = cbind(centroids, test)

centroids = merge(centroids, wheat, by = 'statecounty')
centroids$FIPScode = paste(centroids$STATE_FIPS_CODE, centroids$COUNTY_CODE, sep = '')
allData = centroids[, c(11,1,3,4,9,10)]
allData = allData[-c(1100:1124),]
fips_code = fips_code[-44]
write.csv(sapply(fips_code, getPrecipClosestStn), '/scratch/mentors/dbuckmas/closest.csv')
