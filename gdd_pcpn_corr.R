library(plyr)
gdd_dubois = read.csv('/scratch/mentors/dbuckmas/gdd32_means_cummulative/18037.csv')
pcpn_dubois = read.csv('/scratch/mentors/dbuckmas/pcpn_means/18037.csv')
dubois = join(gdd_dubois, pcpn_dubois, by = 'date', type = 'inner', match = 'all')
dubois = dubois[, c(2:5, 7:8)]

table(dubois$p)[1]
s = data.frame(fips = fips_code)
t = c()

# function that finds the number of two week periods with no rain
getDroughts = function(fips) {
  gdd_county = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',fips,'.csv', sep = ''))
  pcpn_county = read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',fips,'.csv', sep = ''))
  county = join(gdd_county, pcpn_county, by = 'date', type = 'inner', match = 'all')
  county = county[, c(2:5, 7:8)]
  h = county$a[county$date == '1980-06-30']
  q = tryCatch({table(county$t[county$year == 1980])[['0']]}, error = function(e) 0)
  t = rbind(t, q)
}

y = sapply(fips_code[1:295], getDroughts)
r = sapply(fips_code[296:844], getDroughts)

o = c(y,r)
s = cbind(s, o)

l = data.frame(fips = fips_code)
m = c()

getGDD = function(fips) {
  gdd_county = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',fips,'.csv', sep = ''))
  h = gdd_county$a[gdd_county$date == '1980-06-30']
  m = rbind(m,h)
}

a = sapply(fips_code, getGDD)
l = cbind(l, a)
l = join(l, wheat80, by = 'fips', type = 'left', match = 'all')
summary(lm(l$a[l$a > 2000 & l$VALUE > 30]~l$VALUE[l$a > 2000 & l$VALUE > 30]))
# gdd_county = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',17017,'.csv', sep = ''))
# pcpn_county = read.csv(paste('/scratch/mentors/dbuckmas/pcpn_means/',17017,'.csv', sep = ''))
# county = join(gdd_county, pcpn_county, by = 'date', type = 'inner', match = 'all')
# county = county[, c(2:5, 7:8)]
# 
# table(county$p[county$year == 1980])[['0']]

getYields = function(cropYieldColumn) {
  crop = read.delim("/scratch/mentors/dbuckmas/head.txt", header=T, sep ="\t")
  crop = subset(crop, SOURCE_DESC == 'SURVEY')
  crop = subset(crop, AGG_LEVEL_DESC == 'COUNTY')
  crop = subset(crop, SHORT_DESC == cropYieldColumn) ## change this to your crop's yield column name
  crop = crop[, c(15, 21, 17, 22, 31, 38)]
  crop$STATE_FIPS_CODE = sprintf('%02d',crop$STATE_FIPS_CODE)
  crop$COUNTY_CODE = sprintf('%03d',crop$COUNTY_CODE)
  crop$fips = paste(crop$STATE_FIPS_CODE,crop$COUNTY_CODE, sep = '')
  crop$VALUE = as.numeric(as.character(crop$VALUE))
  crop$statecounty = data.frame(paste(crop$STATE_NAME, crop$COUNTY_NAME, sep = ","))
  crop$statecounty = sapply(crop$statecounty, tolower)
  crop = subset(crop, YEAR >= 1980 & YEAR <= 2010)
  cropCut = subset(crop, STATE_NAME %in% c("OHIO","INDIANA","ILLINOIS","IOWA","MISSOURI",
                                           "NEBRASKA","KANSAS","SOUTH DAKOTA","MINNESOTA"))
  cropCut = cropCut[ ,c(5:8)]
  crop80 = subset(cropCut, YEAR >= 1981 & YEAR <= 1990)
  crop90 = subset(cropCut, YEAR >= 1991 & YEAR <= 2000)
  crop00 = subset(cropCut, YEAR >= 2001 & YEAR <= 2010)
  return(cropCut)
}

wheat = getYields('WHEAT - YIELD, MEASURED IN BU / ACRE')

wheat80 = wheat[wheat$YEAR == 1980,c(6,7)]
o = c(y,r)
s = cbind(s, o)
s = join(s, wheat80, by = 'fips', type = 'left', match = 'all')
plot(s$o[s$o<100], s$VALUE[s$o<100])
summary(lm(s$o[!s$o %in% c(365,366)]~s$VALUE[!s$o %in% c(365,366)]))$r.squared


y = read.csv('/scratch/mentors/dbuckmas/pcpn_means/17007.csv')
table(y$pcpn, useNA = 'always')
y = y[format(as.Date(y$date), '%Y') >= 1980 & format(as.Date(y$date), '%Y') <= 2007,]

# gets cummulated gdd for a specific day & fips
u = c()
read = function(fips, m, d) {
  y = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',fips,'.csv', sep = ''))
  h = y[format(as.Date(y$dates), '%m-%d') == paste(m,'-',d, sep = ''),4:5]
  u = rbind(u,h)
  u
}

# oneDate = function(fips, m, d) {
#   oneYear = sapply(1980:2007, read, fips, m, d)
# }
library(data.table)
format(as.Date('1980-01-01'), '%m-%d')
fips_code

p = rbindlist(sapply(fips_code[1:101],read, '07','31', simplify = F))
sum(is.na(p$k))
# l = read.csv(paste('/scratch/mentors/dbuckmas/gdd32_means_cummulative/',17001,'.csv', sep = ''))
# l$k[format(as.Date(l$dates), '%Y') == 1980 & format(as.Date(l$dates), '%m') == '04' & format(as.Date(l$dates), '%d') == '01']
# unique(l$ssn)
# l$q[l$date == '1981-03-31']

wheat$VALUE[wheat$fips == 17001]
j = rep(fips_code, times = 28)
j = sort(j)
year = data.frame(YEAR = rep(1980:2007, times = 844), fips = j)
year = year[order(year$fips),]
wheat = merge(year, wheat, by = c('YEAR','fips'), all.x = T)
wheat = wheat[order(wheat$fips, wheat$YEAR),]
plot(p$k[p$k > 3000], wheat$VALUE[wheat$fips %in% fips_code[1:101] & p$k > 3000])
abline(a = 87.656239, b = -.014553)
summary(lm(wheat$VALUE~p$k))
# x<-seq(3000,8000,by=0.1)
# plot(dnorm(x, mean = 5500, sd = 1000),type="l")
# lines(x, dnorm(x,mean = 5500, sd = 1000),col="red")
# y = dnorm(x,mean = 5500, sd = 500)
# plot(sort(wheat$VALUE))
# plot(density(wheat$VALUE, na.rm = T))
# plot(ecdf(p$k))
# qqnorm(wheat$VALUE)
# shapiro.test(p$k[1:5000])

df = data.frame(yield = wheat$VALUE, gdd = p$k)
library(ggplot2)
ggplot(data = df[df$gdd > 3000,][2829:(193*28),], aes(x = gdd, y = yield)) + geom_point() + geom_smooth()
