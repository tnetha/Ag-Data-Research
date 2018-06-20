library(pls)
library(tidyverse)
IN_fips <- pull_state_codes('IN')

IN_fips_name <- paste('V', IN_fips,sep = '')

IN_df <- cbind(date = pcpn_df$date, pcpn_df[,IN_fips_name])
#IN_df$date <- as.factor(format(IN_df$date,'%Y'))

tall_IN_df <- IN_df %>% gather(County, Value, -date)

colnames(tall_IN_df)[4] <- 'Precip'
#IN_sums <- tall_IN_df %>% group_by(date, County) %>% summarise(sum = sum(Value, na.rm =T))
#View(IN_sums)

tall_IN_df$County <- as.numeric(gsub('V([0-9]+)', '\\1', tall_IN_df$County ,perl = T))

# import corn yields
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME == 'INDIANA',] 
cornbelt_corn <- cornbelt_corn[cornbelt_corn$AGG_LEVEL_DESC == 'COUNTY',]
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
cornbelt_corn$STATE_NAME <- factor(cornbelt_corn$STATE_NAME)
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]
View(yields)

yields <- yields[order(as.Date(as.character(yields$YEAR),'%Y'), yields$COUNTY_CODE),]
yields <- yields[yields$YEAR >= 1970 & yields$YEAR <= 2015,]

yields$COUNTY_CODE <- sprintf('%03d', as.numeric(as.character(yields$COUNTY_CODE)))
yields$fips <- paste(yields$STATE_FIPS_CODE,yields$COUNTY_CODE, sep = '')

yields_df <- data.frame(year = yields$YEAR, County = yields$fips, yields = yields$VALUE)

tall_IN_df <- cbind(year = format(tall_IN_df$date, '%Y'), tall_IN_df)
combined_IN_df <- merge(tall_IN_df, yields_df, by = c('year', 'County'), all.x = T)
combined_IN_df$Precip <- as.numeric(as.character(combined_IN_df$Precip))


write.csv(combined_IN_df, '/scratch/mentors/dbuckmas/IN_corn_rain_df.csv')
