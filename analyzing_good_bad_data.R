goodData = readRDS('goodData')

badData = readRDS('badData')

library(ggplot2)

cummGDDBad = badData[format(as.Date(badData$date), '%m-%d') == '07-31',]

cummGDDgood = goodData[format(as.Date(goodData$date), '%m-%d') == '07-31',]

ggplot() + geom_point(data = cummGDDBad, aes(x = state, y = yield, color = rating)) + 
  geom_point(data = cummGDDgood, aes(x = state, y = yield, color = rating))

length(goodData$fips[goodData$state == 'IL'])

mean(cummGDDBad$rain, na.rm = T)
  