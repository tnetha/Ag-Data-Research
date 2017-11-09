#chloropleth data for the midwest using data already given to us
#loads packages
library(ggplot2)
library(data.table)
library(maps)

#reads data into workspace
corn <- read.table("/scratch/mentors/dbuckmas/corn_data.txt", header = TRUE, sep = "\t", fill = TRUE)


#Subset the data for just survey data
midwest_corn <- subset(corn, corn$SOURCE_DESC == "SURVEY")

# Now for states wanted, Corn belt = Indiana, Illinois, Iowa, Missouri, eastern Nebraska, and eastern Kansas
mid_corn <- subset(midwest_corn, midwest_corn$STATE_NAME == "ILLINOIS" | midwest_corn$STATE_NAME == "INDIANA" 
                   | midwest_corn$STATE_NAME == "IOWA" | midwest_corn$STATE_NAME == "MISSOURI" | midwest_corn$STATE_NAME == "NEBRASKA" 
                   | midwest_corn$STATE_NAME == "KANSAS" | midwest_corn$STATE_NAME == "SOUTH DAKOTA" | midwest_corn$STATE_NAME == "MINNESOTA" 
                   | midwest_corn$STATE_NAME == "OHIO" )

# Now for each decade
mid_8190_corn <- subset(mid_corn, mid_corn$YEAR >= 1981 & mid_corn$YEAR <= 1990)
mid_9100_corn <- subset(mid_corn, mid_corn$YEAR >= 1991 & mid_corn$YEAR <= 2000)
mid_0100_corn <- subset(mid_corn, mid_corn$YEAR >= 2001 & mid_corn$YEAR <= 2010)

# AGG_LEVEL = COUNTY
mid_8190_corn <- subset(mid_8190_corn, mid_8190_corn$AGG_LEVEL_DESC == "COUNTY")
mid_8190_corn$COUNTY_NAME <- factor(mid_8190_corn$COUNTY_NAME)
mid_9100_corn <- subset(mid_9100_corn, mid_9100_corn$AGG_LEVEL_DESC == "COUNTY")
mid_9100_corn$COUNTY_NAME <- factor(mid_9100_corn$COUNTY_NAME)
mid_0100_corn <- subset(mid_0100_corn, mid_0100_corn$AGG_LEVEL_DESC == "COUNTY")
mid_0100_corn$COUNTY_NAME <- factor(mid_0100_corn$COUNTY_NAME)

# Now limit to crop yield
mid_8190_cropyield <- subset(mid_8190_corn, mid_8190_corn$SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE")
mid_9100_cropyield <- subset(mid_9100_corn, mid_9100_corn$SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE")
mid_0100_cropyield <- subset(mid_0100_corn, mid_0100_corn$SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE")

# Create a new column in _cropyield with state,county in lowercase
mid_8190_cropyield$statecounty <- data.frame(paste(mid_8190_cropyield$STATE_NAME,mid_8190_cropyield$COUNTY_NAME, sep = ","))
mid_8190_cropyield$statecounty <- sapply(mid_8190_cropyield$statecounty, tolower)
mid_9100_cropyield$statecounty <- data.frame(paste(mid_9100_cropyield$STATE_NAME,mid_9100_cropyield$COUNTY_NAME, sep = ","))
mid_9100_cropyield$statecounty <- sapply(mid_9100_cropyield$statecounty, tolower)
mid_0100_cropyield$statecounty <- data.frame(paste(mid_0100_cropyield$STATE_NAME,mid_0100_cropyield$COUNTY_NAME, sep = ","))
mid_0100_cropyield$statecounty <- sapply(mid_0100_cropyield$statecounty, tolower)

#average soybean yield for state,county
mid_8190_avgyield <- tapply(as.numeric(gsub(",",".", mid_8190_cropyield$VALUE)), mid_8190_cropyield$statecounty, mean)
mid_9100_avgyield <- tapply(as.numeric(gsub(",",".", mid_9100_cropyield$VALUE)), mid_9100_cropyield$statecounty, mean)
mid_0100_avgyield <- tapply(as.numeric(gsub(",",".", mid_0100_cropyield$VALUE)), mid_0100_cropyield$statecounty, mean)

# List of counties for corn belt, Indiana, Illinois, Iowa, Missouri, eastern Nebraska, and eastern Kansas
map.county <- map_data('county')
counties <- unique(map.county[,5:6])
midwest_counties <- counties[counties$region == "indiana" | counties$region == "illinois" | counties$region == "iowa" | counties$region == "missouri" | counties$region == "kansas" | counties$region == "nebraska" | counties$region == "south dakota" | counties$region == "minnesota" | counties$region == "ohio",]
midwest_counties <- midwest_counties[order(midwest_counties$region),]
#midwest_counties_ne <- subset(midwest_counties, midwest_counties$region == "nebraska" & midwest_counties$subregion != "dawes" & midwest_counties$subregion != "kimball" & midwest_counties$subregion != "grant" & midwest_counties$subregion != "hooker")
#midwest_counties_notne <- subset(midwest_counties, midwest_counties$region != "nebraska")
#midwest_counties_8190 <- rbind(midwest_counties_ne, midwest_counties_notne)

# What counties in midwest don't have crop yield data?
names(mid_0100_avgyield)
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "missouri,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "kansas,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "nebraska,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "indiana,other (combined) counties"] 
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "ohio,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "illinois,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "iowa,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "minnesota,other (combined) counties"]
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "south dakota,other (combined) counties"]


mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "missouri,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "kansas,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "nebraska,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "indiana,other (combined) counties"] 
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "ohio,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "illinois,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "iowa,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "minnesota,other (combined) counties"]
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "south dakota,other (combined) counties"]

mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "missouri,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "kansas,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "nebraska,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "indiana,other (combined) counties"] 
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "ohio,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "illinois,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "iowa,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "minnesota,other (combined) counties"]
mid_0100_avgyield <- mid_0100_avgyield[names(mid_0100_avgyield) != "south dakota,other (combined) counties"]

V1 <- paste(midwest_counties$region,midwest_counties$subregion,sep = ",")
V2 <- names(mid_8190_avgyield)
V3 <- names(mid_9100_avgyield)
V4 <- names(mid_0100_avgyield)

#need these counties removed from midwest counties for 8190

missing <- V2[!(V2 %in% V11)] 
V11[!(V11 %in% V2)] 
missing
missing <- missing[missing != "iowa,o brien"]
missing <- missing[missing != "indiana,st. joseph"]
V11 <- paste(midwest_counties$region,midwest_counties$subregion,sep = ",")

mid_8190_avgyield <- data.frame(names(mid_8190_avgyield), mid_8190_avgyield)
mid_8190_avgyield <- mid_8190_avgyield[mid_8190_avgyield$names.mid_8190_avgyield. != "missouri,ste. genevieve",]
mid_8190_avgyield <- mid_8190_avgyield[mid_8190_avgyield$names.mid_8190_avgyield. != "south dakota,oglala lakota",]

midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="cook"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="lake"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="south dakota" & midwest_counties$subregion=="shannon"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="st louis city"),]

# Making a map of corn belt for 1981-1990
yieldmap <- data.frame(state_names = midwest_counties$region,county_names = midwest_counties$subregion, yield = mid_8190_avgyield$mid_8190_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois" | map.county$region=="kansas" | map.county$region =="nebraska" | map.county$region == "missouri" | map.county$region == "iowa" | map.county$region == "south dakota" | map.county$region == "minnesota" | map.county$region == "ohio", ])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]
states = map_data('state')
mdw_states = subset(states, region %in% c("ohio", "indiana", "illinois", "iowa", 
                                          "minnesota", "missouri", "kansas", "nebraska",
                                          "south dakota"))
mdw_base = ggplot(data = mdw_states, mapping = aes(x = long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color = 'black', fill = NA, size = 0.2)
mdw_base + geom_polygon(data = map.df, aes(fill = yield)) + geom_polygon(color = "black", fill = NA, size = 0.2) + ggtitle("Average Corn Yields 1981-1990") + scale_fill_gradient(low = 'light green',high = 'dark green',limits=c(0,200))


#need these counties removed from midwest counties for 91-00
                    
V3[!(V3 %in% V11)] 
V11[!(V11 %in% V3)] 
V11 <- paste(midwest_counties$region,midwest_counties$subregion,sep = ",")

mid_9100_avgyield <- data.frame(names(mid_9100_avgyield), mid_9100_avgyield)
mid_9100_avgyield <- mid_9100_avgyield[mid_9100_avgyield$names.mid_9100_avgyield. != "south dakota,oglala lakota",]

midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="st louis"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="cook"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="lake"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="lake of the woods"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="ramsey"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="camden"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="carter"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="douglas"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="iron"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="mcdonald"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="madison"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="oregon"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="ozark"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="reynolds"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="st louis city"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="shannon"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="stone"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="taney"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="washington"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="wright"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="ohio" & midwest_counties$subregion=="cuyahoga"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="ohio" & midwest_counties$subregion=="lake"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="south dakota" & midwest_counties$subregion=="shannon"),]



# Making a map of corn belt for 1991-2000
yieldmap <- data.frame(state_names = midwest_counties$region,county_names = midwest_counties$subregion, yield = mid_9100_avgyield$mid_9100_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois" | map.county$region=="kansas" | map.county$region =="nebraska" | map.county$region == "missouri" | map.county$region == "iowa" | map.county$region == "south dakota" | map.county$region == "minnesota" | map.county$region == "ohio", ])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]
states = map_data('state')
mdw_states = subset(states, region %in% c("ohio", "indiana", "illinois", "iowa", 
                                          "minnesota", "missouri", "kansas", "nebraska",
                                          "south dakota"))
mdw_base = ggplot(data = mdw_states, mapping = aes(x = long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color = 'black', fill = NA, size = 0.2)
mdw_base + geom_polygon(data = map.df, aes(fill = yield)) + geom_polygon(color = "black", fill = NA, size = 0.2) + ggtitle("Average Corn Yields 1991-2000") + scale_fill_gradient(low = 'light green',high = 'dark green',limits=c(0,200))


V4[!(V4 %in% V11)] 
V11[!(V11 %in% V4)] 
V11 <- paste(midwest_counties$region,midwest_counties$subregion,sep = ",")

mid_0100_avgyield <- data.frame(names(mid_0100_avgyield), mid_0100_avgyield)
mid_0100_avgyield <- mid_0100_avgyield[mid_0100_avgyield$names.mid_0100_avgyield. != "south dakota,oglala lakota",]

midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="st louis"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="cook"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="itasca"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="koochiching"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="lake"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="lake of the woods"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="minnesota" & midwest_counties$subregion=="ramsey"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="camden"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="carter"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="dallas"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="dent"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="douglas"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="iron"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="madison"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="oregon"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="ozark"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="phelps"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="pulaski"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="reynolds"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="st louis city"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="shannon"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="taney"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="missouri" & midwest_counties$subregion=="washington"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="nebraska" & midwest_counties$subregion=="grant"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="nebraska" & midwest_counties$subregion=="hooker"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="ohio" & midwest_counties$subregion=="cuyahoga"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="ohio" & midwest_counties$subregion=="lake"),]
midwest_counties <- midwest_counties[!(midwest_counties$region=="south dakota" & midwest_counties$subregion=="shannon"),]



# Making a map of corn belt for 2001-2010
yieldmap <- data.frame(state_names = midwest_counties$region,county_names = midwest_counties$subregion, yield = mid_0100_avgyield$mid_0100_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois" | map.county$region=="kansas" | map.county$region =="nebraska" | map.county$region == "missouri" | map.county$region == "iowa" | map.county$region == "south dakota" | map.county$region == "minnesota" | map.county$region == "ohio", ])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]
states = map_data('state')
mdw_states = subset(states, region %in% c("ohio", "indiana", "illinois", "iowa", 
                                          "minnesota", "missouri", "kansas", "nebraska",
                                          "south dakota"))
mdw_base = ggplot(data = mdw_states, mapping = aes(x = long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color = 'black', fill = NA, size = 0.2)
mdw_base + geom_polygon(data = map.df, aes(fill = yield)) + geom_polygon(color = "black", fill = NA, size = 0.2) + ggtitle("Average Corn Yields 2001-2010") + scale_fill_gradient(low = 'light green',high = 'dark green',limits=c(0,200))





