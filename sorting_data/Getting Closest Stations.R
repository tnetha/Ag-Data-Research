library(ggplot2)
library(data.table)
library(maps)

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



