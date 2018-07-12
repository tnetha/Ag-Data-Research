#library(tidyverse)
library(NOAAcounty)
state_fips('IN')
counties <- read.table('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt',header = F, sep = ',', 
                       fill = T, stringsAsFactors = F, quote = '')

midwest_counties <- counties[counties$V1 == 'OH' | counties$V1 == 'IN' | counties$V1 == 'IL'
                             | counties$V1 == 'IA' | counties$V1 == 'MO' | counties$V1 == 'KS' |
                               counties$V1 == 'NE' | counties$V1 == 'SD' | counties$V1 == 'MN',]

midwest_counties$V2 <- sprintf('%02d',midwest_counties$V2)
midwest_counties$V3 <- sprintf('%03d',midwest_counties$V3)
fips_code <- paste(midwest_counties$V2,midwest_counties$V3,sep = '')
fips_code <- fips_code[!fips_code %in% c(17087, 46113, 29019)]
fips_code <- as.numeric(fips_code)

pcpn_command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=pcpn&meta=ll\' > /scratch/mentors/dbuckmas/json_files/', x,'.json', sep = ''))
sapply(fips_code, pcpn_command)

avgt_command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=avgt&meta=ll\' > /scratch/mentors/dbuckmas/avgt_files/', x,'.json', sep = ''))
sapply(fips_code, avgt_command)

maxt_command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=maxt&meta=ll\' > /scratch/mentors/dbuckmas/maxt_files/', x,'.json', sep = ''))
sapply(fips_code, maxt_command)

gdd32_command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=gdd32&meta=ll\' > /scratch/mentors/dbuckmas/gdd32_files/', x,'.json', sep = ''))
sapply(fips_code, gdd32_command)
