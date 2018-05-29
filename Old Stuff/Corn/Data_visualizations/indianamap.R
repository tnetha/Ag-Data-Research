#Loading packages
library(ggplot2)
library(data.table)
#Getting corn yields
IN_field_corn <- IN_field_corn[IN_field_corn$YEAR >= 1981 & IN_field_corn$YEAR <= 2010, ]
IN_field_corn <- IN_field_corn[ IN_field_corn$SOURCE_DESC == "SURVEY", ]
IN_field_cornyields <- IN_field_corn[ IN_field_corn$SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / NET PLANTED ACRE", ]
IN_field_cornyields <- IN_field_cornyields[IN_field_cornyields$AGG_LEVEL_DESC == "COUNTY",]
IN_field_cornyields$COUNTY_NAME <- factor(IN_field_cornyields$COUNTY_NAME)

# Corn Yield Averages
cornyieldmean <- tapply(as.numeric(as.character(gsub(",", ".", IN_field_cornyields$VALUE))), IN_field_cornyields$COUNTY_NAME, mean)

# Getting a list of IN counties
map.county <- map_data('county')
counties <- unique(map.county[,5:6])
incounties <- counties[counties$region == "indiana",]
incounties <- incounties[order(incounties$subregion),]
# Making a map of IN and prepping the data for plotting
yieldmap <- data.frame(state_names = incounties$region,county_names = incounties$subregion, yield = cornyieldmean )
map.county <- data.table(map.county[map.county$region=="indiana",])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]

# Making the map
ggplot(map.df, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()
cornyieldmean
yieldmap
