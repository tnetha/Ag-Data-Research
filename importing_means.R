test_mean <- read.csv('/scratch/mentors/dbuckmas/pcpn_means/17147.csv')
View(test_mean)
test_mean$X <- NULL

pcpn_df <- data.frame(date = seq(as.Date('1970-01-01'),as.Date('2015-12-31'),by='days'))

fips_code <- fips_code[-494]

pcpn_mat <- sapply(fips_code, function(x) read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',x,'.csv',sep = ''))[[2]],simplify = F)

pcpn_df <- do.call(data.frame,pcpn_mat)
pcpn_names <- paste('V',fips_code,sep = '')
names(pcpn_df) <- pcpn_names
head(pcpn_df)
View(pcpn_df)
