install.packages('tidyverse')
library(tidyverse)
counties <- read.table('https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt',header = F, sep = ',', 
                       fill = T, stringsAsFactors = F, quote = '')
nrow(counties[counties$V1 == 'IN',])

midwest_counties <- counties[counties$V1 == 'OH' | counties$V1 == 'IN' | counties$V1 == 'IL'
                             | counties$V1 == 'IA' | counties$V1 == 'MO' | counties$V1 == 'KS' |
                               counties$V1 == 'NE' | counties$V1 == 'SD' | counties$V1 == 'MN',]

midwest_counties$V2 <- sprintf('%02d',midwest_counties$V2)
midwest_counties$V3 <- sprintf('%03d',midwest_counties$V3)
fips_code <- paste(midwest_counties$V2,midwest_counties$V3,sep = '')
fips_code <- as.numeric(fips_code)
class(fips_code)
write.table(as.numeric(fips_code),file = '~/Desktop/fips_code.txt',sep = '\n')

command <- function(x) system(paste('curl \'http://data.rcc-acis.org/MultiStnData?county=',x,'&sdate=1970-01-01&edate=2015-12-31&elems=pcpn&meta=ll\' > /scratch/mentors/dbuckmas/json_files/', x,'.json', sep = ''))
sapply(fips_code, command)
 