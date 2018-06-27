library(jsonlite)
library(ggplot2)
library(plyr)
library(ggmap)
# pulling fips codes from the Census Bureau, creating a df of states, counties, and fips codes
counties1 <- read.table('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt',header = F, sep = ',', 
                        fill = T, stringsAsFactors = F, quote = '')

counties1$V4 <- tolower(sub('([A-Za-z]+) County', '\\1', counties1$V4, perl = T))
names(counties1) = c('state.abb', 'sCode', 'cCode', 'name', 'not sure')
states1 = data.frame(state.abb, tolower(state.name))
counties1 = join(counties1, states1, by = 'state.abb', type = 'left', match = 'all')

# pull out just the midwest ones
midwest_counties <- counties1[counties1$state.abb == 'OH' | counties1$state.abb == 'IN' | counties1$state.abb == 'IL'
                              | counties1$state.abb == 'IA' | counties1$state.abb == 'MO' | counties1$state.abb == 'KS' |
                                counties1$state.abb == 'NE' | counties1$state.abb == 'SD' | counties1$state.abb == 'MN',]

# keep the 0s that R wants to delete
midwest_counties$sCode <- sprintf('%02d',midwest_counties$sCode)
midwest_counties$cCode <- sprintf('%03d',midwest_counties$cCode)
midwest_counties$fips_code <- paste(midwest_counties$sCode,midwest_counties$cCode,sep = '')
midwest_counties$statecounty = paste(midwest_counties$tolower.state.name.,',',midwest_counties$name, sep = '')

# making a df with lat and lons
counties = map_data('county')
counties$statecounty = paste(counties$region, counties$subregion, sep = ',')

# just the midwest ones
midwestCounties = counties[counties$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

# match up the county names
midwest_counties$statecounty = unique(midwestCounties$statecounty)

# list of fips codes to run function on - takes out missing data ones
fips_code = midwest_counties$fips_code
fips_code <- fips_code[!fips_code %in% c(17087, 46113, 29019)]

# making skeleton for df that will contain data for one day
oneDayData = data.frame(fips_code)
oneDayData$avgt = rep(NA, times = length(oneDayData$fips_code))

# this currently has the maxt files, but you can easily change it to pcpn or avgt by just changing what directory you pull from
getDay = function(date1, type) {
  for (fips in fips_code) {
    s = read.csv(paste('/scratch/mentors/dbuckmas/',type,'/',fips,'.csv', sep=''), header = T, sep = ',')
    dates = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 'days')
    s = cbind(dates, s)
    oneDayData$avgt[oneDayData$fips_code == fips] = s$x[s$dates == as.Date(date1)]
  }
  oneDayData
}

# pulling together the state, county, fips, lat, and lon
allData = join(midwest_counties, midwestCounties, by = 'statecounty', type = 'right', match = 'all')

# pulling in state data for state borders for later
states = map_data('state')
midwestStates = states[states$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

# cities to put on map
names = c('Chicago, IL', 'St. Louis, MO', 'Minneapolis, MN', 'Indianapolis, IN', 'Lincoln, NE', 'Wichita, KS')
cities = cbind(names, geocode(as.character(names)))
names(cities) = c('names', 'long', 'lat')

# this one i use for temperature maps
drawTempMap = function(date) {
  oneDay = getDay(date,'maxt_means')
  tempData = join(oneDay, allData, by = 'fips_code', type = 'left', match = 'all')
  tempData = tempData[,c(1,2,3,10:15)]
  tempData = tempData[order(tempData$order),]
  ggplot(data = tempData, aes(x = long, y = lat)) + geom_polygon(aes(fill = tempData$avgt,group = group), size = 0.2) + 
    coord_fixed(1.3) + scale_fill_gradientn(na.value = 'lightgray', limits = c(-10,110), colors = c('plum1','purple','blue','deepskyblue','light blue','green','green3','yellow','orange','orangered','red','red3'),
                                            breaks = c(-10,0,10,20,30,40,50,60,70,80,90,100,110)) + geom_polygon(data = midwestStates, aes(fill = tempData$avgt, x = long, y = lat, group = group), color = 'black', fill = NA, size = 0.2) + 
    guides(fill = guide_colorbar(title = 'temp (F)', barwidth = 1, barheight = 15)) + geom_point(data = cities, color = 'black') + 
    geom_text(data = cities, aes(label = paste("  ", as.character(names), sep=""),hjust = 0), color = "black", size = 3.5)
  
}
# draw a map of whatever day between 1970-01-01 and 2015-12-31
drawTempMap('2001-12-25')

# this one i created specifically for rain becasue it has different scales
drawRainMap = function(date) {
  oneDay = getDay(date, 'pcpn_means')
  tempData = join(oneDay, allData, by = 'fips_code', type = 'left', match = 'all')
  tempData = tempData[,c(1,2,3,10:15)]
  tempData = tempData[order(tempData$order),]
  ggplot(data = tempData, mapping = aes(x = long, y = lat)) + geom_polygon(aes(fill = tempData$avgt, group = group), size = 0.2) + 
    coord_fixed(1.3) + scale_fill_gradientn(na.value = 'gray100', limits = c(0,2), colors = c('white','lightblue','deepskyblue','blue')) + 
    geom_polygon(data = midwestStates, aes(fill = tempData$avgt, x = long, y = lat, group = group), color = 'black', fill = NA, size = 0.2) + 
    guides(fill = guide_colorbar(title = 'rain (in)', barwidth = 1, barheight = 15)) + geom_point(data = cities, color = 'black') + 
    geom_text(data = cities, aes(label = paste("  ", as.character(names), sep=""),hjust = 0), color = "black", size = 3.5)
}

# draw a map of whatever day between 1970-01-01 and 2015-12-31
drawRainMap('2000-04-12')

gddDF = read.csv('/scratch/mentors/dbuckmas/gdd32_means/17001.csv')
gddDF$date = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 'days')
gddDF = gddDF[,c(2:3)]
names(gddDF) = c('gdd', 'date')
gddDF$year = format(gddDF$date, '%Y')

s = c()
t = c()

getSum = function(year, fips, file) {
  for (i in 1:length(file$gdd[file$year == year])) {
    a = sum(file$gdd[file$year == year][1:i], na.rm = T)
    s = rbind(s, a)
  }
  s
}

getTotal = function(fips) {
  r = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means/',fips,'.csv', sep = ''), header = T, sep = ',')
  r$date = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 'days')
  r = r[,c(2:3)]
  names(r) = c('gdd', 'date')
  r$year = format(r$date, '%Y')
  a = unlist(sapply(1970:2015, getSum, fips, r, simplify = F))
  r = cbind(r, a)
  r = as.data.frame(r)
  write.csv(r, paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',fips,'.csv', sep = ''))
}

sapply(fips_code, getTotal, simplify = F)

t = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means/',17003,'.csv', sep = ''), header = T, sep = ',')
t$date = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 'days')
t = t[,c(2:3)]
names(t) = c('gdd', 'date')
t$year = format(t$date, '%Y')
a = unlist(sapply(1970:2015, getSum, fips, t, simplify = F))
t = cbind(t, a)
write.csv(t, paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',17003,'.csv', sep = ''))

sum(gddDF$gdd[gddDF$year == 1971][1:nrow(gddDF)], na.rm = T)
table(is.na(gddDF$gdd))
r = gddDF$gdd[gddDF$year == 1971]

