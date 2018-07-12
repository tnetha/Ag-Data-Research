# pulling fips codes from the Census Bureau
counties1 <- read.table('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt',header = F, sep = ',', 
                        fill = T, stringsAsFactors = F, quote = '')

# pull out just the midwest ones
midwest_counties <- counties1[counties1$V1 == 'OH' | counties1$V1 == 'IN' | counties1$V1 == 'IL'
                              | counties1$V1 == 'IA' | counties1$V1 == 'MO' | counties1$V1 == 'KS' |
                                counties1$V1 == 'NE' | counties1$V1 == 'SD' | counties1$V1 == 'MN',]

# keep the 0s that R wants to delete
midwest_counties$V2 <- sprintf('%02d',midwest_counties$V2)
midwest_counties$V3 <- sprintf('%03d',midwest_counties$V3)

# combine the state and county fips codes
fips_code <- paste(midwest_counties$V2,midwest_counties$V3,sep = '')
fips_code = as.numeric(as.character(fips_code))

fips_code <- fips_code[!(fips_code == 17087 | fips_code == 46113 | fips_code == 29019)]

# pcpn
pcpn_dates <- data.frame(date = seq(as.Date('1970-01-01'),as.Date('2015-12-31'),by='days'))

pcpn_mat <- sapply(fips_code, function(x) read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',x,'.csv',sep = ''))[[2]],simplify = F)

pcpn_df <- do.call(data.frame,pcpn_mat)
pcpn_names <- paste('V',fips_code,sep = '')
names(pcpn_df) <- pcpn_names

# average temp

avgt_mat <- sapply(fips_code, function(x) read.csv(paste('/scratch/mentors/dbuckmas/avgt_means/',x,'.csv',sep = ''))[[2]],simplify = F)
avgt_df <- do.call(data.frame,avgt_mat)
avgt_names <- paste('V',fips_code,sep = '')
names(avgt_df) <- avgt_names




library(tidyverse)

fips_name <- paste('V', fips_code,sep = '')

midwest_pcpn_df <- cbind(date = pcpn_dates, pcpn_df)
#IN_df$date <- as.factor(format(IN_df$date,'%Y'))

tall_df <- midwest_pcpn_df %>% gather(County, Value, -date)


midwest_avgt_df <- cbind(date = pcpn_dates, avgt_df)
#IN_df$date <- as.factor(format(IN_df$date,'%Y'))

tall_avgt_df <- midwest_avgt_df %>% gather(County, Value, -date)

colnames(tall_df)[3] <- 'precip'

tall_df$avgt <- tall_avgt_df$Value 
#IN_sums <- tall_IN_df %>% group_by(date, County) %>% summarise(sum = sum(Value, na.rm =T))
#View(IN_sums)

tall_df$County <- as.numeric(gsub('V([0-9]+)', '\\1', tall_df$County ,perl = T))

# import corn yields
corn <- read.table('/scratch/mentors/dbuckmas/corn_data.txt', header = T, sep = '\t', fill = T)
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME %in% c('INDIANA','OHIO','ILLINOIS','IOWA','MISSOURI','KANSAS','KANSAS','NEBRASKA','SOUTH DAKOTA','MINNESOTA'),] 
cornbelt_corn <- cornbelt_corn[cornbelt_corn$AGG_LEVEL_DESC == 'COUNTY',]
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
cornbelt_corn$STATE_NAME <- factor(cornbelt_corn$STATE_NAME)
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]

yields <- yields[order(as.Date(as.character(yields$YEAR),'%Y'), yields$COUNTY_CODE),]
yields <- yields[yields$YEAR >= 1970 & yields$YEAR <= 2015,]

yields$COUNTY_CODE <- sprintf('%03d', as.numeric(as.character(yields$COUNTY_CODE)))
yields$fips <- paste(yields$STATE_FIPS_CODE,yields$COUNTY_CODE, sep = '')

yields_df <- data.frame(year = yields$YEAR, County = yields$fips, yields = yields$VALUE)

tall_df <- cbind(year = format(tall_df$date, '%Y'), tall_df)
combined_df <- merge(tall_df, yields_df, by = c('year', 'County'), all.x = T)
combined_df$precip <- as.numeric(as.character(combined_df$precip))


write.csv(combined_df, '/scratch/mentors/dbuckmas/midwest_corn_rain_avgt_df.csv')
 