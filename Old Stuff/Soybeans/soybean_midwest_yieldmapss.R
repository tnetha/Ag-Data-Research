#chloropleth data for the midwest using data already given to us
#loads packages
library(ggplot2)
library(data.table)
library(maps)

#reads data into workspace
beans <- read.table("/scratch/mentors/dbuckmas/soybeans_data.txt", header = TRUE, sep = "\t", fill = TRUE)


#Subset the data for just survey data
midwest_beans <- subset(beans, beans$SOURCE_DESC == "SURVEY")

# Now for states wanted, Corn belt = Indiana, Illinois, Iowa, Missouri, eastern Nebraska, and eastern Kansas
mid_beans <- subset(midwest_beans, midwest_beans$STATE_NAME == "ILLINOIS" | midwest_beans$STATE_NAME == "INDIANA" | midwest_beans$STATE_NAME == "IOWA" | midwest_beans$STATE_NAME == "MISSOURI" | midwest_beans$STATE_NAME == "NEBRASKA" | midwest_beans$STATE_NAME == "KANSAS" | midwest_beans$STATE_NAME == "SOUTH DAKOTA" | midwest_beans$STATE_NAME == "MINNESOTA" | midwest_beans$STATE_NAME == "OHIO" )

# Now for each decade
mid_8190_beans <- subset(mid_beans, mid_beans$YEAR >= 1981 & mid_beans$YEAR <= 1990)
mid_9100_beans <- subset(mid_beans, mid_beans$YEAR >= 1991 & mid_beans$YEAR <= 2000)
mid_0100_beans <- subset(mid_beans, mid_beans$YEAR >= 2001 & mid_beans$YEAR <= 2010)

# AGG_LEVEL = COUNTY
mid_8190_beans <- subset(mid_8190_beans, mid_8190_beans$AGG_LEVEL_DESC == "COUNTY")
mid_8190_beans$COUNTY_NAME <- factor(mid_8190_beans$COUNTY_NAME)
mid_9100_beans <- subset(mid_9100_beans, mid_9100_beans$AGG_LEVEL_DESC == "COUNTY")
mid_9100_beans$COUNTY_NAME <- factor(mid_9100_beans$COUNTY_NAME)
mid_0100_beans <- subset(mid_0100_beans, mid_0100_beans$AGG_LEVEL_DESC == "COUNTY")
mid_0100_beans$COUNTY_NAME <- factor(mid_0100_beans$COUNTY_NAME)

# Now limit to crop yield
mid_8190_cropyield <- subset(mid_8190_beans, mid_8190_beans$SHORT_DESC == "SOYBEANS - YIELD, MEASURED IN BU / ACRE")
mid_9100_cropyield <- subset(mid_9100_beans, mid_9100_beans$SHORT_DESC == "SOYBEANS - YIELD, MEASURED IN BU / ACRE")
mid_0100_cropyield <- subset(mid_0100_beans, mid_0100_beans$SHORT_DESC == "SOYBEANS - YIELD, MEASURED IN BU / ACRE")

# Create a new column in _cropyield with state,county in lowercase
mid_8190_cropyield$statecounty <- data.frame(paste(mid_8190_cropyield$STATE_NAME,mid_8190_cropyield$COUNTY_NAME, sep = ","))
mid_8190_cropyield$statecounty <- sapply(mid_8190_cropyield$statecounty, tolower)
mid_9100_cropyield$statecounty <- data.frame(paste(mid_9100_cropyield$STATE_NAME,mid_9100_cropyield$COUNTY_NAME, sep = ","))
mid_9100_cropyield$statecounty <- sapply(mid_9100_cropyield$statecounty, tolower)
mid_0100_cropyield$statecounty <- data.frame(paste(mid_0100_cropyield$STATE_NAME,mid_0100_cropyield$COUNTY_NAME, sep = ","))
mid_0100_cropyield$statecounty <- sapply(mid_0100_cropyield$statecounty, tolower)

#average soybean yield for state,county
mid_8190_avgyield <- tapply(as.numeric(gsub(",",".", mid_8190_cropyield$VALUE)), mid_8190_cropyield$statecounty, mean)
mid_8190_avgyield
mid_9100_avgyield <- tapply(as.numeric(gsub(",",".", mid_9100_cropyield$VALUE)), mid_9100_cropyield$statecounty, mean)
mid_9100_avgyield
mid_0100_avgyield <- tapply(as.numeric(gsub(",",".", mid_0100_cropyield$VALUE)), mid_0100_cropyield$statecounty, mean)
mid_0100_avgyield

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

V2[!(V2 %in% V11)] 
#"missouri,ste. genevieve" is in the data but not one of the counties in V1
mid_8190_avgyield <- mid_8190_avgyield[names(mid_8190_avgyield) != "missouri,ste. genevieve"]
V1[!(V1 %in% V2)] 
ill_8190 <- subset(midwest_counties, midwest_counties$region == "illinois")
in_8190 <- subset(midwest_counties, midwest_counties$region == "indiana")
iowa_1890 <- subset(midwest_counties, midwest_counties$region == "iowa")
ohio_1890 <- subset(midwest_counties, midwest_counties$region == "ohio"& midwest_counties$subregion != "athens" & midwest_counties$subregion != "belmont" & midwest_counties$subregion != "cuyahoga" & midwest_counties$subregion != "gallia" & midwest_counties$subregion != "geauga" & midwest_counties$subregion != "guernsey" &midwest_counties$subregion != "jefferson" & midwest_counties$subregion != "lake" &
                      midwest_counties$subregion != "lawrence" & midwest_counties$subregion != "meigs" &midwest_counties$subregion != "monroe" & midwest_counties$subregion != "morgan" & midwest_counties$subregion != "noble" & midwest_counties$subregion != "vinton") 
sd_1890 <- subset(midwest_counties, midwest_counties$region == "south dakota"& midwest_counties$subregion != "bennett" & midwest_counties$subregion != "butte" & midwest_counties$subregion != "campbell" & midwest_counties$subregion != "custer" & midwest_counties$subregion != "dewey" & midwest_counties$subregion != "fall river" &midwest_counties$subregion != "haakon" & midwest_counties$subregion != "harding" &
                      midwest_counties$subregion != "hyde" & midwest_counties$subregion != "jackson" &midwest_counties$subregion != "jones" & midwest_counties$subregion != "lawrence" & midwest_counties$subregion != "mcpherson" & midwest_counties$subregion != "meade" &
                  midwest_counties$subregion != "hyde" & midwest_counties$subregion != "mellette" &midwest_counties$subregion != "pennington" & midwest_counties$subregion != "perkins" & midwest_counties$subregion != "shannon" & midwest_counties$subregion != "ziebach") 
minn_1890 <- subset(midwest_counties, midwest_counties$region == "minnesota"& midwest_counties$subregion != "carlton" & midwest_counties$subregion != "cook" & midwest_counties$subregion != "lake" & midwest_counties$subregion != "st louis")
ne_1890   <- subset(midwest_counties, midwest_counties$region == "nebraska"& midwest_counties$subregion != "dawes" & midwest_counties$subregion != "grant" & midwest_counties$subregion != "hooker" & midwest_counties$subregion != "kimball")
miss_1890 <- subset(midwest_counties, midwest_counties$region == "missouri"& midwest_counties$subregion != "st louis city" )
kansas_1890 <- subset(midwest_counties, midwest_counties$region == "kansas")
counties_1890 <- rbind(ill_8190, in_8190, iowa_1890, kansas_1890, minn_1890, miss_1890, ne_1890, ohio_1890, sd_1890)
V11 <- paste(counties_1890$region,counties_1890$subregion,sep = ",")

# Making a map of corn belt for 1981-1990
yieldmap <- data.frame(state_names = counties_1890$region,county_names = counties_1890$subregion, yield = mid_8190_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois" | map.county$region=="kansas" | map.county$region =="nebraska" | map.county$region == "missouri" | map.county$region == "iowa" | map.county$region == "south dakota" | map.county$region == "minnesota" | map.county$region == "ohio", ])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]
ggplot(map.df, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ggtitle("Average Soybean Yields 1981-1990")


#need these counties removed from midwest counties for 9100
                      
V12[!(V12 %in% V3)]
miss_9100 <- subset(midwest_counties, midwest_counties$region == "missouri"& midwest_counties$subregion != "carter" & midwest_counties$subregion != "reynolds" & midwest_counties$subregion != "st louis city"
                    & midwest_counties$subregion != "christian" & midwest_counties$subregion != "dallas" & midwest_counties$subregion != "dent" & midwest_counties$subregion != "douglas" & midwest_counties$subregion != "ozark"
                    & midwest_counties$subregion != "itasca" & midwest_counties$subregion != "laclede" & midwest_counties$subregion != "iron" & midwest_counties$subregion != "phelps" & midwest_counties$subregion != "webster" 
                    & midwest_counties$subregion != "camden" & midwest_counties$subregion != "shannon" & midwest_counties$subregion != "pulaski" & midwest_counties$subregion != "howell"
                    & midwest_counties$subregion != "madison" & midwest_counties$subregion != "oregon" & midwest_counties$subregion != "texas" & midwest_counties$subregion != "washington"
                    & midwest_counties$subregion != "wright" & midwest_counties$subregion != "stone" & midwest_counties$subregion != "taney")
ill_9100 <- subset(midwest_counties, midwest_counties$region == "illinois")
in_9100 <- subset(midwest_counties, midwest_counties$region == "indiana")
iowa_9100 <- subset(midwest_counties, midwest_counties$region == "iowa")
ohio_9100 <- subset(midwest_counties, midwest_counties$region == "ohio" & midwest_counties$subregion != "belmont" & midwest_counties$subregion != "cuyahoga" & midwest_counties$subregion != "guernsey" &midwest_counties$subregion != "jefferson" & midwest_counties$subregion != "lake" &
                      midwest_counties$subregion != "noble" & midwest_counties$subregion != "vinton") 
sd_9100 <- subset(midwest_counties, midwest_counties$region == "south dakota"& midwest_counties$subregion != "butte" & midwest_counties$subregion != "custer" & midwest_counties$subregion != "harding" &
                     midwest_counties$subregion != "lawrence" & midwest_counties$subregion != "shannon" & midwest_counties$subregion != "fall river" & midwest_counties$subregion != "ziebach") 
minn_9100 <- subset(midwest_counties, midwest_counties$region == "minnesota"& midwest_counties$subregion != "beltrami" &midwest_counties$subregion != "carlton" & midwest_counties$subregion != "cook" & midwest_counties$subregion != "lake" & midwest_counties$subregion != "st louis" &
                    midwest_counties$subregion != "cass" &midwest_counties$subregion != "crow wing" & midwest_counties$subregion != "hubbard" & midwest_counties$subregion != "ramsey" & midwest_counties$subregion != "koochiching" & midwest_counties$subregion != "itasca")
ne_9100 <- subset(midwest_counties, midwest_counties$region == "nebraska"& midwest_counties$subregion != "dawes" & midwest_counties$subregion != "grant" & midwest_counties$subregion != "hooker" & midwest_counties$subregion != "kimball"
                   &midwest_counties$subregion != "arthur" & midwest_counties$subregion != "thomas" & midwest_counties$subregion != "scotts bluff" & midwest_counties$subregion != "sioux" & midwest_counties$subregion != "sheridan" & midwest_counties$subregion != "morrill" )
kansas_9100 <- subset(midwest_counties, midwest_counties$region == "kansas")
counties_9100 <- rbind(ill_9100, in_9100, iowa_9100, kansas_9100, minn_9100, miss_9100, ne_9100, ohio_9100, sd_9100)
V12 <- paste(counties_9100$region,counties_9100$subregion,sep = ",")

V3[!(V3 %in% V12)] 
#"south dakota,oglala lakota" is in the data but not one of the counties in V1 --take out oof yield data for that time period
mid_9100_avgyield <- mid_9100_avgyield[names(mid_9100_avgyield) != "south dakota,oglala lakota"]

# Making a map of corn belt for 1991-2000
yieldmap <- data.frame(state_names = counties_9100$region,county_names = counties_9100$subregion, yield = mid_9100_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois" | map.county$region=="kansas" | map.county$region =="nebraska" | map.county$region == "missouri" | map.county$region == "iowa" | map.county$region == "south dakota" | map.county$region == "minnesota" | map.county$region == "ohio", ])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]
ggplot(map.df, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ ggtitle("Average Soybean Yields 1991-2000")







#need to take these counties out of midwest counties for 0100 because we do not have data for it!
V13[!(V13 %in% V4)]
    
    

miss_0100 <- subset(midwest_counties, midwest_counties$region == "missouri"& midwest_counties$subregion != "carter" & midwest_counties$subregion != "reynolds" & midwest_counties$subregion != "st louis city"
                    & midwest_counties$subregion != "christian" & midwest_counties$subregion != "dent" & midwest_counties$subregion != "douglas" & midwest_counties$subregion != "ozark"
                    & midwest_counties$subregion != "iron"   & midwest_counties$subregion != "camden" & midwest_counties$subregion != "shannon" & midwest_counties$subregion != "mcdonald" & midwest_counties$subregion != "howell"
                    & midwest_counties$subregion != "crawford" & midwest_counties$subregion != "oregon" & midwest_counties$subregion != "texas" & midwest_counties$subregion != "washington"
                    & midwest_counties$subregion != "wright" & midwest_counties$subregion != "stone" & midwest_counties$subregion != "taney")
ill_0100 <- subset(midwest_counties, midwest_counties$region == "illinois")
in_0100 <- subset(midwest_counties, midwest_counties$region == "indiana" & midwest_counties$subregion != "brown" & midwest_counties$subregion != "crawford")
iowa_0100 <- subset(midwest_counties, midwest_counties$region == "iowa")
ohio_0100 <- subset(midwest_counties, midwest_counties$region == "ohio" & midwest_counties$subregion != "belmont" & midwest_counties$subregion != "cuyahoga" &midwest_counties$subregion != "jefferson" & midwest_counties$subregion != "lake" &
                      midwest_counties$subregion != "noble" & midwest_counties$subregion != "vinton" & midwest_counties$subregion != "monroe") 
sd_0100 <- subset(midwest_counties, midwest_counties$region == "south dakota"& midwest_counties$subregion != "butte" &  midwest_counties$subregion != "harding" &
                    midwest_counties$subregion != "lawrence" & midwest_counties$subregion != "shannon" & midwest_counties$subregion != "fall river" ) 
minn_0100 <- subset(midwest_counties, midwest_counties$region == "minnesota" &midwest_counties$subregion != "carlton" & midwest_counties$subregion != "cook" & midwest_counties$subregion != "lake" & midwest_counties$subregion != "st louis" &
                       midwest_counties$subregion != "ramsey"  & midwest_counties$subregion != "itasca")
ne_0100 <- subset(midwest_counties, midwest_counties$region == "nebraska"& midwest_counties$subregion != "dawes" & midwest_counties$subregion != "arthur" & midwest_counties$subregion != "hooker" & midwest_counties$subregion != "kimball" & midwest_counties$subregion != "grant"
                  &midwest_counties$subregion != "banner" & midwest_counties$subregion != "mcpherson" & midwest_counties$subregion != "box butte" & midwest_counties$subregion != "blaine" & midwest_counties$subregion != "scotts bluff" & midwest_counties$subregion != "sioux" & midwest_counties$subregion != "thomas" & midwest_counties$subregion != "morrill" )
kansas_0100 <- subset(midwest_counties, midwest_counties$region == "kansas" & midwest_counties$subregion != "hamilton" & midwest_counties$subregion != "stanton")
counties_0100 <- rbind(ill_0100, in_0100, iowa_0100, kansas_0100, minn_0100, miss_0100, ne_0100, ohio_0100, sd_0100)
V13 <- paste(counties_0100$region,counties_0100$subregion,sep = ",")







V4[!(V4 %in% V13)]
#perfect praise the lord!

   

# Making a map of corn belt for 2001-2010
yieldmap <- data.frame(state_names = counties_0100$region,county_names = counties_0100$subregion, yield = mid_0100_avgyield )
map.county <- data.table(map.county[map.county$region=="indiana" | map.county$region=="illinois" | map.county$region=="kansas" | map.county$region =="nebraska" | map.county$region == "missouri" | map.county$region == "iowa" | map.county$region == "south dakota" | map.county$region == "minnesota" | map.county$region == "ohio", ])
setkey(map.county,region,subregion)
yieldmap <- data.table(yieldmap)
setkey(yieldmap,state_names,county_names)
map.df <- map.county[yieldmap]
ggplot(map.df, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ggtitle("Average Soybean Yields 2001-2010")
