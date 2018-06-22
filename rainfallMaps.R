library(jsonlite)
library(ggplot2)
library(plyr)
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

fips_code <- fips_code[!fips_code %in% c(17087, 46113, 29019)]

oneDayData = data.frame(fips_code)
oneDayData$avgt = rep(NA, times = length(oneDayData$fips_code))


# this currently has the maxt files, but you can easily change it by just changing what directory you pull from
getDay = function(date1) {
  for (fips in fips_code) {
    s = read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',fips,'.csv', sep=''), header = T, sep = ',')
    dates = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 'days')
    s = cbind(dates, s)
    oneDayData$avgt[oneDayData$fips_code == fips] = s$x[s$dates == as.Date(date1)]
  }
  oneDayData
}

states = map_data('state')
midwestStates = states[states$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

counties = map_data('county')
counties$statecounty = paste(counties$region, counties$subregion, sep = ',')

# just the midwest ones
midwestCounties = counties[counties$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

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

wheat = getYields("/scratch/mentors/dbuckmas/head.txt")
wheat$FIPScode = paste(wheat$STATE_FIPS_CODE, wheat$COUNTY_CODE, sep = '')
wheat$STATE_NAME = sapply(wheat$STATE_NAME, tolower)
wheat$COUNTY_NAME = sapply(wheat$COUNTY_NAME, tolower)
allData = join(wheat, midwestCounties, by = 'statecounty', type = 'right', match = 'all')

# tempData = merge(oneDayData, allData, by.x = 'fips_code', by.y = 'FIPScode')
# tempData = tempData[,c(1,2,3,10:15)]
# tempData = tempData[order(tempData$order),]

ggplot(data = tempData, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = tempData$avgt), size = 0.2) + 
  coord_fixed(1.3) + scale_fill_gradientn(na.value = 'gray100', limits = c(-10,100), colors = c('plum1','purple','blue','deepskyblue','light blue','green','green3','yellow','orange','darkorange','red','red3'), breaks = c(-10,0,10,20,30,40,50,60,70,80,90,100))
range(tempData$avgt, na.rm = T)

# this one i use for temperature maps
drawMap = function(date) {
  oneDay = getDay(date)
  tempData = merge(oneDay, allData, by.x = 'fips_code', by.y = 'FIPScode')
  tempData = tempData[,c(1,2,3,10:15)]
  tempData = tempData[order(tempData$order),]
  ggplot(data = tempData, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = tempData$avgt), size = 0.2) + 
    coord_fixed(1.3) + scale_fill_gradientn(na.value = 'blue', limits = c(-10,110), colors = c('plum1','purple','blue','deepskyblue','light blue','green','green3','yellow','orange','orangered','red','red3'),
                                            breaks = c(-10,0,10,20,30,40,50,60,70,80,90,100))
}
drawMap('2007-08-12')

# this one i created specifically for rain becasue it has different scales
drawRainMap = function(date) {
  oneDay = getDay(date)
  tempData = merge(oneDay, allData, by.x = 'fips_code', by.y = 'FIPScode')
  tempData = tempData[,c(1,2,3,10:15)]
  tempData = tempData[order(tempData$order),]
  ggplot(data = tempData, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = tempData$avgt), size = 0.2) + 
    coord_fixed(1.3) + scale_fill_gradientn(na.value = 'gray100', limits = c(0,2), colors = c('white','lightblue','blue'))
}
drawRainMap('2000-04-12')
