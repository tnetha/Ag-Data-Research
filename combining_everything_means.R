# load libraries
library(tidyverse)
library(data.table)
library(stringr)
library(jsonlite)

#### building a data frame of just the elements we're concerened with right now ####

# make a list of the 'state,county' to use as a cross reference with other data sets
counties = map_data('county')
counties$statecounty = paste(counties$region, counties$subregion, sep = ',')

# just the midwest ones
midwestCounties = counties[counties$region %in% c("ohio", "indiana", "illinois", "iowa", "missouri", "kansas", "nebraska", "south dakota", "minnesota"),]

# this is used to cross reference counties with their lats and lons
groups = unique(midwestCounties$group)

# pull each county once since the previous county data has multiple entries of each county
statecounty = unique(midwestCounties$statecounty)

# start the df that will eventually contain all of the data
centroids = data.frame(groups, statecounty)

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

finding_averages <- function(fips) {
  library(jsonlite)
  json_init <- fromJSON(paste('/scratch/mentors/dbuckmas/json_files/',fips,'.json',sep = ''))
  json_mat <- matrix(NA,nrow = 16801,ncol = length(json_init$data$data))
  for (i in 1:length(json_init$data$data)) {
    json_mat[,i] <- as.numeric(as.character(json_init$data$data[[i]])) 
  }
  json_mat[json_mat == 'M'] <- NA
  json_mat[json_mat == 'T'] <- .005
  json_mean <- rowMeans(json_mat, na.rm = T)
  write.csv(json_mean, paste('/scratch/mentors/dbuckmas/pcpn_means/',fips,'.csv',sep = ''))
}

<<<<<<< HEAD

new_fips <- fips_code[-(1:494)]
=======
fips_code <- fips_code[!(fips_code == 17087 | fips_code == 46113 | fips_code == 29019)]
>>>>>>> 4f9b1cc679fac65847b9fa25a37ba4b4d563eb44

sapply(fips_code, finding_averages)

# making column for two week periods of precip
s = c()
getSumPcpn = function(file) {
  for (i in 1:length(file$pcpn)) {
    a = sum(file$pcpn[i:(i+13)], na.rm = T)
    s = rbind(s,a)
  }
  s
}
writeTwoWeekTotals = function(fips) {
  r = read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',fips,'.csv', sep = ''), header = T, sep = ',')
  r$date = seq(as.Date('1970-01-01'), as.Date('2015-12-31'), by = 'days')
  r = r[,c(2:3)]
  names(r) = c('pcpn', 'date')
  p = getSumPcpn(r)
  r = cbind(r, p)
  write.csv(r, paste('/scratch/mentors/dbuckmas/pcpn_means/',fips,'.csv', sep = ''))
}

sapply(fips_code, writeTwoWeekTotals)

finding_averages_avgt <- function(fips) {
  library(jsonlite)
  json_init <- fromJSON(paste('/scratch/mentors/dbuckmas/avgt_files/',fips,'.json',sep = ''))
  json_mat <- matrix(NA,nrow = 16801,ncol = length(json_init$data$data))
  for (i in 1:length(json_init$data$data)) {
    json_mat[,i] <- as.numeric(as.character(json_init$data$data[[i]])) 
  }
  json_mat[json_mat == 'M'] <- NA
  json_mat[json_mat == 'T'] <- .005
  json_mean <- rowMeans(json_mat, na.rm = T)
  write.csv(json_mean, paste('/scratch/mentors/dbuckmas/avgt_means/',fips,'.csv',sep = ''))
}


sapply(fips_code, finding_averages_avgt)

finding_averages_maxt <- function(fips) {
  library(jsonlite)
  json_init <- fromJSON(paste('/scratch/mentors/dbuckmas/maxt_files/',fips,'.json',sep = ''))
  json_mat <- matrix(NA,nrow = 16801,ncol = length(json_init$data$data))
  for (i in 1:length(json_init$data$data)) {
    json_mat[,i] <- as.numeric(as.character(json_init$data$data[[i]])) 
  }
  json_mat[json_mat == 'M'] <- NA
  json_mat[json_mat == 'T'] <- .005
  json_mean <- rowMeans(json_mat, na.rm = T)
  write.csv(json_mean, paste('/scratch/mentors/dbuckmas/maxt_means/',fips,'.csv',sep = ''))
}

sapply(fips_code, finding_averages_maxt)

finding_averages_gdd32 <- function(fips) {
  library(jsonlite)
  json_init <- fromJSON(paste('/scratch/mentors/dbuckmas/gdd32_files/',fips,'.json',sep = ''))
  json_mat <- matrix(NA,nrow = 16801,ncol = length(json_init$data$data))
  for (i in 1:length(json_init$data$data)) {
    json_mat[,i] <- as.numeric(as.character(json_init$data$data[[i]])) 
  }
  json_mat[json_mat == 'M'] <- NA
  json_mat[json_mat == 'T'] <- .005
  json_mean <- rowMeans(json_mat, na.rm = T)
  write.csv(json_mean, paste('/scratch/mentors/dbuckmas/gdd32_means/',fips,'.csv',sep = ''))
}
sapply(fips_code, finding_averages_gdd32)
