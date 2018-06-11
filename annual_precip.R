t = colSums(pcpn_df, na.rm = T)
z = 1970
total = 0
newDF = data.frame()
colnames(newDF) = c(colnames(pcpn_df)[2:845])
pcpn_names <- paste('V',1970:2015,sep = '')
rownames(newDF) <- pcpn_names
rownames(newDF) = c(paste('V',1970:2015,sep=''))
getYear = function(year) {
  a = pcpn_df[format(pcpn_df$date,"%Y") == year,]
  totals = colSums(a[,2:845], na.rm = T)
  newDF = rbind(newDF, totals)
  
  
  # if(format(pcpn_df[pcpn_df$date == as.Date(datE),], "%Y") == z) {
  #   newDF[z, fips] = newDF[z, fips] + pcpn_df[date, fips]
  # }  
  # z = z + 1
}

newDF = sapply(1970:2015, getYear)
newDF = as.data.frame(t(newDF))

newDF$X54.0288333333333 = as.numeric(newDF$X54.0288333333333)
class(newDF$X54.0288333333333)
sapply(pcpn_df$date, getYear, colnames(pcpn_df))
sum(pcpn_df$V17003, na.rm = T)
sum(newDF$X54.0288333333333)
class(newDF)

newDF = cbind(c(1970:2015),newDF)
class(newDF$X54.0288333333333)
t = getYear('1970-01-01', 'V17003')

pcpn_df[pcpn_df$date == as.Date('1970-01-02'),'V17001']
 
class(newDF$X1)
head(a)

format(pcpn_df$date, "%Y")
a = pcpn_df[format(pcpn_df$date,"%Y") == 1970,]
totals = colSums(a[,2:845], na.rm = T)
