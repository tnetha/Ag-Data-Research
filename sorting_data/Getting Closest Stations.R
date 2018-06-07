library(tidyverse)
library(ggplot2)
library(data.table)
library(maps)
library(stringr)
# code used to pull json data down, DO NOT RERUN
#command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=pcpn&meta=ll\' > /scratch/mentors/dbuckmas/json_files/', x,'.json', sep = ''))
#sapply(fips_code, command)

# get counties
counties = map_data('county')

# add a column to the counties data frame of the county name and state
counties$countyNames = paste(counties$region, ",", counties$subregion)

# subset midwest counties only
midwestCounties = counties[counties$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

# make a list of unique groups - i think this is just like a county id type thing
groups = unique(midwestCounties$group)

# make a list of unique county names since the counties data comes with muliple 
midwestCountyNames = unique(midwestCounties$countyNames)
head(midwestCountyNames)

# creating a data frame to store the average lats and lons
centroids = data.frame(groups, midwestCountyNames)
avgLats = numeric()
avgLons = numeric()

# probably a more efficient way to do this - if you can find one go ahead and change it
# iterate through each "id" in groups, get the subset of county border data corresponding to that "id"
# find the min and max for lat and lon, take the average of those, append it to a numeric list of averages
for (grp in groups) {
  lat_lons = subset(midwestCounties, midwestCounties$group == grp)
  maxLon = max(lat_lons$long)
  minLon = min(lat_lons$long)
  maxLat = max(lat_lons$lat)
  minLat = min(lat_lons$lat)
  avgLon = (maxLon + minLon) / 2
  avgLat = (maxLat + minLat) / 2
  avgLats = append(avgLats, avgLat)
  avgLons = append(avgLons, avgLon)
}

# rbind the averages to the centroids data
# centroids contains each group "id", the county, and the average lat and lon based on the max and min of the county data - could
# be inaccurate in some cases where county lines are super weird but we're going with this for now
centroids = t(centroids)
centroids = rbind(centroids, avgLats)
centroids = rbind(centroids, avgLons)
centroids = t(centroids)

# next step is to pull lat and lons from weather station data and compare which one in each county is closest to the average lat and lon
# planning on doing this using something along the lines of the distance formula
# the hard part here is figuring out how to cross reference the huge database of weather station data with county names
# before finals i think i was trying to use fips codes, i believe the weather data has fips codes somewhere in it but don't count
# on that being true
centroids_DF <- as.data.frame(centroids)
centroids_DF <- separate(centroids_DF,2,c('state', 'county'),sep = '\ ,\ ',remove = F)

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
class(fips_code$fips_code)

# remove the word county, make it lowercase to match to other data frames
County_remove <- function(x) str_replace(x, ' County', '')
midwest_counties$V4 <- sapply(midwest_counties$V4, County_remove, USE.NAMES = F)
midwest_counties$V4 = sapply(midwest_counties$V4, tolower)

# add a state column
midwest_counties$state = NA 

stateName = function(abr){
    if (abr == "IL") {
      midwest_counties$state[midwest_counties$V1 == "IL"] = "illinois"
    } else if (abr == "IN") {
      midwest_counties$state[midwest_counties$V1 == "IN"] = "indiana"
    } else if (abr == "IA") {
      midwest_counties$state[midwest_counties$V1 == "IA"] = "iowa"
    } else if (abr == "KS") {
      midwest_counties$state[midwest_counties$V1 == "KS"] = "kansas"
    } else if (abr == "MN") {
      midwest_counties$state[midwest_counties$V1 == "MN"] = "minnesota"
    } else if (abr == "MO") {
      midwest_counties$state[midwest_counties$V1 == "MO"] = "missouri"
    } else if (abr == "NE") {
      midwest_counties$state[midwest_counties$V1 == "NE"] = "nebraska"
    } else if (abr == "OH") {
      midwest_counties$state[midwest_counties$V1 == "OH"] = "ohio"
    } else if (abr == "SD") {
      midwest_counties$state[midwest_counties$V1 == "SD"] = "south dakota"
    }
}
midwest_counties$state = sapply(midwest_counties$V1, stateName, USE.NAMES = F)

# create a column of state, county
midwest_counties$stateCounty = paste(midwest_counties$state, ",", midwest_counties$V4)

# combining things from both data frames
newDF = data.frame(centroids_DF$midwestCountyNames, as.numeric(as.character(centroids_DF$avgLats)), as.numeric(as.character(centroids_DF$avgLons)), paste(midwest_counties$V2,midwest_counties$V3, sep = ""))
names(newDF) = c("midwestCountyNames", "avgLat", "avgLon", "FIPScode")
fips_code <- as.numeric(fips_code)
class(fips_code)
fips_code

# newDF contains "state,county", center of each county, and its fips code

<<<<<<< HEAD
#command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=pcpn&meta=ll\' > /scratch/mentors/dbuckmas/json_files/', x,'.json', sep = ''))
#sapply(fips_code, command)

=======
>>>>>>> dde6ff1382f059f83beb467d7f4b6c2c5272d9af
