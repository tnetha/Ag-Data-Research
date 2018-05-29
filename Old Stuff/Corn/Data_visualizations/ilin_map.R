#Loading packages
library(ggplot2)
library(data.table)
library(maps)

# Subset the data just for the Midwest
# The Census Bureau defines the Midwest as Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota,
# Missouri, Nebraska, North Dakota, Ohio, South Dakota, and Wisconsin

## Just doing Indiana and Illinois first

# Subset the data for just survey data
midwest_field_corn <- subset(field_corn, field_corn$SOURCE_DESC == "SURVEY")
# Now for states wanted
ill_in_field_corn <- subset(midwest_field_corn, midwest_field_corn$STATE_NAME == "ILLINOIS" | midwest_field_corn$STATE_NAME == "INDIANA" )
# Now 1981-2010
ill_in_field_corn <- subset(ill_in_field_corn, ill_in_field_corn$YEAR >= 1981 & ill_in_field_corn$YEAR <= 2010)
# AGG_LEVEL = COUNTY
ill_in_field_corn <- subset(ill_in_field_corn, ill_in_field_corn$AGG_LEVEL_DESC == "COUNTY")
ill_in_field_corn$COUNTY_NAME <- factor(ill_in_field_corn$COUNTY_NAME)
# Now limit to crop yield
ill_in_cropyield <- subset(ill_in_field_corn, ill_in_field_corn$SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / NET PLANTED ACRE")
# Create a new column in ill_in_cropyield with state,county in lowercase
ill_in_cropyield$statecounty <- data.frame(paste(ill_in_cropyield$STATE_NAME,ill_in_cropyield$COUNTY_NAME, sep = ","))
ill_in_cropyield$statecounty <- sapply(ill_in_cropyield$statecounty, tolower)

# Now find the average corn yield per (state, county)
ill_in_avgyield <- tapply(as.numeric(gsub(",",".", ill_in_cropyield$VALUE)), ill_in_cropyield$statecounty, mean)
ill_in_avgyield


# List of counties for IN and IL
map.county <- map_data('county')
counties <- unique(map.county[,5:6])
il_incounties <- counties[counties$region == "indiana" | counties$region == "illinois",]
il_incounties <- il_incounties[order(il_incounties$region),]
# What counties in IL don't have crop yield data?


V1 <- paste(il_incounties$region,il_incounties$subregion,sep = ",")
V2 <- names(ill_in_avgyield)

V1[!(V1 %in% V2)]

myDF <- rbind

# Making a map of IN and IL and prepping the data for plotting
yieldmap <- data.frame(state_names = il_incounties$region,county_names = il_incounties$subregion, yield = ill_in_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois",])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]

ggplot(map.df, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ggtitle("Average Corn Yields 1981-2010")

yieldmap
