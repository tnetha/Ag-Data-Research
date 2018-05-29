#reads data into workspace
beans <- read.table("/scratch/mentors/dbuckmas/soybeans_data.txt", header = TRUE, sep = "\t", fill = TRUE)
#removes unneeded columns from data
cleanedbeans <-within(beans, rm(SOURCE_DESC, SECTOR_DESC, GROUP_DESC, COMMODITY_DESC, CLASS_DESC, STATE_ANSI, STATE_FIPS_CODE, STATE_NAME, ASD_CODE, COUNTY_ANSI,CONGR_DISTRICT_CODE, COUNTRY_CODE, COUNTRY_NAME, BEGIN_CODE, END_CODE, WEEK_ENDING))
#just bartholomew county 
bartbeans <- subset(cleanedbeans, cleanedbeans$COUNTY_NAME == "BARTHOLOMEW")

#averaging bean yields from 2000-2009 for each Indiana County (making subsets)
IN_beans <- subset(beans, beans$STATE_NAME == "INDIANA")
INbeans00_09 <- subset(IN_beans, IN_beans$YEAR >= 2000 & IN_beans$YEAR <= 2009)

#numerator
beanproduction <- subset(INbeans00_09, INbeans00_09$SHORT_DESC == "SOYBEANS - PRODUCTION, MEASURED IN BU" & INbeans00_09$SOURCE_DESC == "SURVEY")
beanprodbycounty <- subset(beanproduction, beanproduction$AGG_LEVEL_DESC == "COUNTY")
beanprodbycounty$COUNTY_NAME <- factor(beanprodbycounty$COUNTY_NAME)
tapply(as.numeric(gsub(",", "", beanprodbycounty$VALUE )), beanprodbycounty$COUNTY_NAME, sum, na.rm = T) #correct

#denominator
beanharvest <- subset(INbeans00_09, INbeans00_09$SHORT_DESC == "SOYBEANS - ACRES HARVESTED" & INbeans00_09$SOURCE_DESC == "SURVEY")
beanharvbycounty <- subset(beanharvest, beanharvest$AGG_LEVEL_DESC == "COUNTY")
beanharvbycounty$COUNTY_NAME <- factor(beanharvbycounty$COUNTY_NAME)
tapply(as.numeric(gsub(",", "",beanharvbycounty$VALUE)), beanharvbycounty$COUNTY_NAME, sum, na.rm = T) #correct
avgbuacrepercounty <- tapply(as.numeric(gsub(",", "", beanprodbycounty$VALUE )), beanprodbycounty$COUNTY_NAME, sum, na.rm = T) / tapply(as.numeric(gsub(",", "",beanharvbycounty$VALUE)), beanharvbycounty$COUNTY_NAME, sum, na.rm = T)





#45 to 54 range
#trying to make a map to color code according to bu/acre yield
#https://stackoverflow.com/questions/33045785/color-in-counties-on-a-map
yielddf <- data.frame(names(avgbuacrepercounty), avgbuacrepercounty)
yield <- subset(yielddf, yielddf$names.avgbuacrepercounty. != "OTHER (COMBINED) COUNTIES")
library(ggplot2)
install.packages("ggplot2")
install.packages("maps")
install.packages("choroplethr")
set.seed(1)
map.county <- map_data('county')
counties <- unique(map.county[,5:6])
incounties <- counties[counties$region=="indiana",]
newincounties <- subset(incounties, incounties$subregion != "crawford" & incounties$subregion != "brown")
yield_map_00to09 <- data.frame(state_names=newincounties$region, county_names=newincounties$subregion, yield = yield$avgbuacrepercounty )
library(data.table)
map.county <- data.table(map.county[map.county$region=="indiana",])
setkey(map.county,region,subregion)
yield_map_00to09 <- data.table(yield_map_00to09)
setkey(yield_map_00to09,state_names,county_names)
map.df <- map.county[yield_map_00to09]
ggplot(map.df, aes(x=long, y=lat, group=group, fill=yield)) + geom_polygon()+coord_map()
##########################################################################################################



library(jsonlite)
install.packages("jsonlite")
#weatherDF <- data.frame(fromJSON("~/R/bartmayoct.json"))
twodim <-weatherDF$data.data

#dot chart of the avg temperatures for bart 2010, with predict line
avgtbartDF <- data.frame(fromJSON("~/R/bart2010avgt.json"))
avgtemp2010bart <- avgtbartDF$data.data
avgtemp2010bartholomewDF <- data.frame(matrix(unlist(avgtemp2010bart), nrow=365, byrow=T), stringsAsFactors=FALSE)
plot(avgtemp2010bartholomewDF)  #plotting the average temperatures from the day we get a downward facing parabola which makes sense because it is cold on the ends of the year and hot during the summer
tempnorm <- (as.numeric(avgtemp2010bartholomewDF$matrix.unlist.avgtemp2010bart...nrow...365..byrow...T.)) 
y <- tempnorm
x <- 1:365
plot(tempnorm)
lines(predict(lm(y~x+I(x^2))))

#adding up the total rainfall for batholomew county for 2010
pcpnbartDF <- data.frame(fromJSON("~/R/bart2010pcpn.json"))
avgpcpn2010bart <- pcpnbartDF$data.data
avgpcpn2010bartholomewDF <- data.frame(matrix(unlist(avgpcpn2010bart), nrow=365, byrow=T), stringsAsFactors=FALSE)
avgpcpn2010bartholomewDF[avgpcpn2010bartholomewDF== "T"] <- NA
data.matrix(avgpcpn2010bartholomewDF, rownames.force = NA)
colSums(data.matrix(avgpcpn2010bartholomewDF, rownames.force = NA), na.rm = TRUE, dims =1) #37.92 inches is the total rainfall for the year
class(avgpcpn2010bart)
zyield <- data.frame(array)

##########################SCRATCH
#how to make a histogram with a best fit line, the data is bogus 
myhist <- hist(tapply(as.numeric(beanyieldbycounty$VALUE), beanyieldbycounty$COUNTY_NAME, median, na.rm = T))
multiplier <- myhist$counts / myhist$density
mydensity <- density(tapply(as.numeric(beanyieldbycounty$VALUE), beanyieldbycounty$COUNTY_NAME, median, na.rm = T))
mydensity$y <- mydensity$y * multiplier[1]
plot(myhist)
lines(mydensity)
