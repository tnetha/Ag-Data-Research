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

# data frame of all the precipitation for all 844 counties
pcpn_df <- cbind(pcpn_dates,pcpn_df)

<<<<<<< HEAD
# getting the data for the 1980-2010 growing seasons for wheat
df1 = subset(pcpn_df, date >= '1979-09-01' & date <= '2010-07-31' & format(date, "%m") != '08')

# making a column with the growing season the date falls under
growYear = c()
for (i in 1980:2010) {
  if (i%%4 == 0) {
    growYear = c(growYear, rep(i, times = 335))
  } else {
    growYear = c(growYear, rep(i, times = 334))
  }
}

# adds growing season to the df with precip data
df1 = cbind(growYear, df1)

# just taking growing season, year, and one county for now
df2 = data.frame(df1$growYear, df1$date, df1$V17001)

totals = as.numeric(c())
biweekTotals = function(rowNum, nrows, dataFrame) {
  # if the row is more than two weeks from the end of the season
  if (rowNum < (nrows-13)) {
    total = c(totals, sum(dataFrame$totalPrecip[rowNum:(rowNum+13)], na.rm = T))
  } else {
    total = c(totals, sum(dataFrame$totalPrecip[rowNum:nrows], na.rm = T))
  }
  return(total)
}

findDroughts = function(year) {
  df_new = df2[df2$df1.growYear == year,]
  colnames(df_new) = c('growYear', 'date', 'totalPrecip')
  rownames(df_new) = c(1:nrow(df_new))
  total = sapply(1:nrow(df_new), biweekTotals, nrow(df_new), df_new)
  df_new = cbind(df_new, total)
  return(df_new)
}
# works fine with just one year, still working on multiple years
a = sapply(1980, findDroughts, simplify = F)
a = as.data.frame(a)

# just taking 1980 (test)
df = df2[df2$df1.growYear == 1981,]
colnames(df) = c('growYear', 'date', 'totalPrecip')
=======
>>>>>>> 4f9b1cc679fac65847b9fa25a37ba4b4d563eb44

