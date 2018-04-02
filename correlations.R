library(tibble)
library(plyr)
tsuccess <- as.data.frame(t(success))
myDF <- tibble(year=1970:2010,corn_yields = rush_yields_70_10$VALUE, # yields are in BU/AC
               mean40year = laply(tsuccess,mean, na.rm=T),
               sum40year = laply(tsuccess,sum,na.rm=T))
laply(tsuccess,mean, na.rm=T)
colMeans(tsuccess, na.rm=T)
class(tsuccess$V1)
myvec <- paste('V',1:41,sep = '')
tsuccess[myvec] <- sapply(tsuccess[myvec],as.character)
tsuccess[myvec] <- sapply(tsuccess[myvec],as.numeric)
median(tsuccess$V1[tsuccess$V1 > 0], na.rm=T)

# import corn yields
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME == 'INDIANA',] 
cornbelt_corn <- cornbelt_corn[cornbelt_corn$AGG_LEVEL_DESC == 'COUNTY',]
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
cornbelt_corn$STATE_NAME <- factor(cornbelt_corn$STATE_NAME)
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]
rush_yields <- yields[yields$COUNTY_NAME == 'RUSH',]
rush_yields_70_10 <- rush_yields[rush_yields$YEAR >= 1970 & rush_yields$YEAR <= 2010,]

normal_yield <- diff(as.numeric(as.character(rush_yields_70_10$VALUE)))
plot(1970:2010,as.numeric(as.character(rush_yields_70_10$VALUE)))

lm(corn_yields ~ mean40year, myDF)