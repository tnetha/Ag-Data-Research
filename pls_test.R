library(pls)
library(tidyverse)
IN_fips <- pull_state_codes('IN')

IN_fips_name <- paste('V', IN_fips,sep = '')

IN_df <- cbind(date = pcpn_df$date, pcpn_df[,IN_fips_name])
View(IN_df)
IN_df$date <- as.factor(format(IN_df$date,'%Y'))

tall_IN_df <- IN_df %>% gather(County, Value, -date)

IN_sums <- tall_IN_df %>% group_by(date, County) %>% summarise(sum = sum(Value, na.rm =T))
View(IN_sums)

IN_sums$County <- as.numeric(gsub('V([0-9]+)', '\\1', IN_sums$County ,perl = T))

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

yields_df <- data.frame(date = yields$YEAR, County = yields$fips, value = yields$VALUE)
combined_IN_df <- merge(IN_sums, yields_df, by = c('date', 'County'), all.x = T)
class(combined_IN_df$value)
combined_IN_df$value <- as.numeric(as.character(combined_IN_df$value))
