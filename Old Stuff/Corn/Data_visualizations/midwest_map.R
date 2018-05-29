## Midwest Corn Yield Map
# The Census Bureau defines the Midwest as Illinois, Indiana, Iowa, Kansas, Michigan, Minnesota,
# Missouri, Nebraska, North Dakota, Ohio, South Dakota, and Wisconsin

# Load packages
library(ggplot2)
library(data.table)
library(maps)
install.packages("stringr")
library(stringr)
# Subset data for just survey data
survey_field_corn <- subset(field_corn, field_corn$SOURCE_DESC == "SURVEY")

# Subset for only Midwestern states
midwestfieldcorn <- subset(survey_field_corn, survey_field_corn$STATE_NAME == "ILLINOIS" | survey_field_corn$STATE_NAME == "INDIANA" | 
                             survey_field_corn$STATE_NAME == "IOWA" | survey_field_corn$STATE_NAME == "KANSAS" | 
                             survey_field_corn$STATE_NAME == "MICHIGAN" | survey_field_corn$STATE_NAME == "MINNESOTA" | 
                             survey_field_corn$STATE_NAME == "MISSOURI" | survey_field_corn$STATE_NAME == "NEBRASKA" |
                             survey_field_corn$STATE_NAME == "NORTH DAKOTA" | survey_field_corn$STATE_NAME == "OHIO" |
                             survey_field_corn$STATE_NAME == "SOUTH DAKOTA" | survey_field_corn$STATE_NAME == "WISCONSIN")

# 1981 - 2010
midwestfieldcorn <- subset(midwestfieldcorn, midwestfieldcorn$YEAR >= 1981 & midwestfieldcorn$YEAR <= 2010)

# Only Counties
midwestfieldcorn <- subset(midwestfieldcorn, midwestfieldcorn$AGG_LEVEL_DESC =="COUNTY")
midwestfieldcorn$COUNTRY_NAME <- factor(midwestfieldcorn$COUNTY_NAME)
# Corn Yields only
midwestcornyields <- subset(midwestfieldcorn,midwestfieldcorn$SHORT_DESC=="CORN, GRAIN - YIELD, MEASURED IN BU / NET PLANTED ACRE")

# Make a new column with (state,county)
midwestcornyields$statecounty <- data.frame(paste(midwestcornyields$STATE_NAME,midwestcornyields$COUNTY_NAME,sep = ","))
midwestcornyields$statecounty <- sapply(midwestcornyields$statecounty,tolower)

# average corn yields
midwestavgyields <- tapply(as.numeric(as.character(gsub(",",".", midwestcornyields$VALUE))),midwestcornyields$statecounty, mean)

# List of counties for Midwest
map.county <- map_data('county')
counties <- unique(map.county[,5:6])
midwestcounties <- counties[counties$region == "indiana" | counties$region == "illinois" | counties$region == "iowa" |
                              counties$region == "kansas" | counties$region == "michigan" | counties$region == "minnesota" | 
                              counties$region == "missouri" | counties$region == "nebraska" | counties$region == "north dakota" |
                              counties$region == "ohio" | counties$region == "south dakota" | counties$region == "wisconsin",]
midwestcounties <- midwestcounties[order(midwestcounties$subregion),]
midwestcounties <- midwestcounties[order(midwestcounties$region),]

names(midwestavgyields) <- gsub("\\.","", names(midwestavgyields))

midwestavgyields <- data.frame(names(midwestavgyields),midwestavgyields)
head(midwestavgyields)
midwestcounties

V1 <- paste(midwestcounties$region,midwestcounties$subregion,sep = ",")
V2 <- midwestavgyields$names.midwestavgyields.
V2 <- as.character(V2)
length(V1[ !(V1 %in% V2)])
midwestavgyields <- midwestavgyields[midwestavgyields$names.midwestavgyields. != "minnesota,other (combined) counties",]
midwestavgyields <- midwestavgyields[midwestavgyields$names.midwestavgyields. != "ohio,other (combined) counties",]
midwestavgyields <- midwestavgyields[midwestavgyields$names.midwestavgyields. != "south dakota,other (combined) counties",]
midwestavgyields <- midwestavgyields[midwestavgyields$names.midwestavgyields. != "south dakota,oglala lakota",]

midwestavgyields <- data.frame(str_split_fixed(midwestavgyields$names.midwestavgyields., ",", 2), midwestavgyields$midwestavgyields)
midwestavgyields$statecounty <- data.frame(paste(midwestavgyields$X1,midwestavgyields$X2,sep = ","))
midwestcounties$statecounty <- data.frame(paste(midwestcounties$region,midwestcounties$subregion,sep = ","))
rbind(midwestavgyields, data.frame(midwestcounties$region[!(midwestcounties$statecounty %in% midwestavgyields$statecounty)],
                                   midwestcounties$subregion[!(midwestcounties$statecounty %in% midwestavgyields$statecounty)],midwestcounties$yield) )

length(midwestavgyields$statecounty)
length(midwestcounties$statecounty)
length(midwestcounties[!(midwestcounties$statecounty %in% midwestavgyields$statecounty)])
length(midwestcounties$subregion[!(midwestcounties$subregion %in% midwestavgyields$X2)])

midwestcounties$yield <- NULL
length(midwestavgyields)
midwestavgyields
# Making the map
yieldmap <- data.frame(state_names = midwestcounties$region,county_names = midwestcounties$subregion, yield = midwestavgyields )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois",])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]

ggplot(map.df, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ggtitle("Average Corn Yields 1981-2010")
