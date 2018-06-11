
pcpn_dates <- data.frame(date = seq(as.Date('1970-01-01'),as.Date('2015-12-31'),by='days'))

fips_code <- fips_code[-494]

pcpn_mat <- sapply(fips_code, function(x) read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',x,'.csv',sep = ''))[[2]],simplify = F)

pcpn_df <- do.call(data.frame,pcpn_mat)
pcpn_names <- paste('V',fips_code,sep = '')
names(pcpn_df) <- pcpn_names

pcpn_df <- cbind(pcpn_dates,pcpn_df)

