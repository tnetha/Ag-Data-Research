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


pcpn_dates <- data.frame(date = seq(as.Date('1970-01-01'),as.Date('2015-12-31'),by='days'))

pcpn_mat <- sapply(fips_code, function(x) read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',x,'.csv',sep = ''))[[2]],simplify = F)

pcpn_df <- do.call(data.frame,pcpn_mat)
pcpn_names <- paste('V',fips_code,sep = '')
names(pcpn_df) <- pcpn_names

pcpn_df <- cbind(pcpn_dates,pcpn_df)


