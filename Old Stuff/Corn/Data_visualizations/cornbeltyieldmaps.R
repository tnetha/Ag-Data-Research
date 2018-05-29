# load packages
library(ggplot2)
library(data.table)
library(maps)

# Get data for corn belt yields and make a map for 3 decades: 1981-1990, 1991-2000, 2001-2010
# Corn belt is Indiana, Illinois, Ohio, Wisconsin, Minnesota, and Iowa

corn <- read.table("/scratch/mentors/dbuckmas/corn_data.txt", header = TRUE, sep = "\t", fill = TRUE)
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME == 'INDIANA' | field_corn$STATE_NAME == 'ILLINOIS' | field_corn$STATE_NAME == 'OHIO'
                            | field_corn$STATE_NAME == 'WISCONSIN' | field_corn$STATE_NAME == 'MINNESOTA' | field_corn$STATE_NAME == 'IOWA', ] 
cornbelt_corn <- cornbelt_corn[cornbelt_corn$AGG_LEVEL_DESC == 'COUNTY',]
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
cornbelt_corn$STATE_NAME <- factor(cornbelt_corn$STATE_NAME)
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]
yields$statecounty <- data.frame(paste(yields$STATE_NAME, yields$COUNTY_NAME, sep = ",")) 
yields_80s <- yields[ yields$YEAR >= '1981' & yields$YEAR <= '1990',] 
yields_90s <- yields[ yields$YEAR >= '1991' & yields$YEAR <= '2000',] 
yields_00s <- yields[ yields$YEAR >= '2001' & yields$YEAR <= '2010',] 

# List of counties
map.county <- map_data('county')
map.county <- map.county[,5:6]
cornbelt_counties <- map.county[map.county$region == "indiana" | map.county$region == "illinois" | 
                                  map.county$region == "ohio" | map.county$region == "wisconsin" |
                                  map.county$region == "minnesota" | map.county$region == "iowa",]
cornbelt_counties <- cornbelt_counties[order(cornbelt_counties$region),]
cornbelt_counties <- cornbelt_counties[!duplicated(cornbelt_counties),]
# Average yields for 1980s 
cornyieldmean <- tapply(as.numeric(as.character(gsub(",", ".", yields_80s$VALUE))), yields_80s$statecounty, mean)
w <- names(cornyieldmean)
names(cornyieldmean) <- NULL
cornyieldmean <- data.frame(county=w,yield=cornyieldmean)
cornyieldmean <- cornyieldmean[-c(cornyieldmean$county=='ILLINOIS,OTHER (COMBINED) COUNTIES'),]
cornyieldmean <- cornyieldmean[-c(cornyieldmean$county=='INDIANA,OTHER (COMBINED) COUNTIES'),]
cornyieldmean <- cornyieldmean[-c(cornyieldmean$county=='IOWA,OTHER (COMBINED) COUNTIES'),]
cornyieldmean <- cornyieldmean[-c(cornyieldmean$county=='MINNESOTA,OTHER (COMBINED) COUNTIES'),]
cornyieldmean <- cornyieldmean[-c(cornyieldmean$county=='OHIO,OTHER (COMBINED) COUNTIES'),]
cornyieldmean <- cornyieldmean[-c(cornyieldmean$county=='WISCONSIN,OTHER (COMBINED) COUNTIES'),]


V1 <- paste(cornbelt_counties$region,cornbelt_counties$subregion,sep = ",")
V2 <- tolower(cornyieldmean$county)
cornyieldmean
missing_counties <- V1[!(V1 %in% V2)]
missing_counties
# making a map of the cornbelt
yieldmap <- data.frame(state=cornbelt_counties$region, county=cornbelt_counties$subregion, yield = cornyieldmean)


