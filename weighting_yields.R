library(tidyverse)

midwestdf <- readRDS('~/Dropbox/Data/midwestdf.RDS')

colnames(midwestdf)

midwestdf <- as.tibble(midwestdf)

set.seed(1111)
ggplot(midwestdf[sample(1:nrow(midwestdf), 1000),], aes(x = year, y = yields)) + geom_point() + stat_smooth(method = 'lm')

county_avg <- midwestdf %>% group_by(County) %>% summarise(avg_yield = mean(yields, na.rm = T))

midwestdf <- merge(county_avg, midwestdf, by = 'County', all = T)

midwestdf <- midwestdf[,c(1,3:9,2,10)]

midwestdf <- midwestdf %>% mutate( adjusted_yields = yields / avg_yield )

ggplot(midwestdf[sample(1:nrow(midwestdf), 1000),], aes(x = year, y = adjusted_yields)) + geom_point() + stat_smooth(method = 'lm')

yearlydf <- midwestdf %>% group_by(County, year, state) %>% 
  summarise(total_precip = sum(precip, na.rm = T), yields = mean(yields, na.rm=T), 
            adjusted_yields = mean(adjusted_yields, na.rm = T)) 
  
junedf <- midwestdf[as.numeric(midwestdf$month) == 6,] %>% group_by(County, year, state) %>% 
  summarise(total_precip = sum(precip, na.rm = T), yields = mean(yields, na.rm=T), 
            adjusted_yields = mean(adjusted_yields, na.rm = T)) 

yearly_yields_lm <- lm(yields ~ total_precip, yearlydf)

june_yields_lm <- lm(yields ~ total_precip, junedf)

summary(yearly_yields_lm)
summary(june_yields_lm)


yearly_adjusted_yields_lm <- lm(adjusted_yields ~ total_precip, yearlydf)

june_adjusted_yields_lm <- lm(yields ~ total_precip, junedf)

summary(yearly_adjusted_yields_lm)
summary(june_adjusted_yields_lm)


acres_harvested <- read.csv('~/Downloads/A8D7B3A1-30B6-3E3D-ADCF-7E992345FDF0.csv')

acres_harvested$State.ANSI <- sprintf('%02d', acres_harvested$State.ANSI)

acres_harvested$County.ANSI <- sprintf('%03d', acres_harvested$County.ANSI)

acres_harvested$County_fips <- paste(acres_harvested$State.ANSI, acres_harvested$County.ANSI, sep = '')

acres_harvested <- acres_harvested[,c('Year', 'County_fips', 'Value')]

midwestdf <- merge(midwestdf, acres_harvested, by.x = c('year', 'County'), by.y = c('Year', 'County_fips'))

midwestdf <- midwestdf[!is.na(midwestdf$month),]

colnames(midwestdf)[12] <- 'Acres Harvested'
midwestdf$`Acres Harvested` <- gsub(',', '', midwestdf$`Acres Harvested`, perl = T)
midwestdf$`Acres Harvested` <- as.numeric(as.character(midwestdf$`Acres Harvested`))

avg_harvested <- midwestdf %>% group_by(County) %>% summarise(avg_acres_harvested = mean(`Acres Harvested`, na.rm = T))

midwestdf <- merge(midwestdf, avg_harvested, by = 'County', all = T)
head(midwestdf)

midwestdf <- midwestdf %>% mutate(normalized_acres_harvested = `Acres Harvested` / avg_harvested)

midwestdf <- midwestdf %>% mutate(adjusted_weighted_yields = adjusted_yields * normalized_acres_harvested)
head(midwestdf)

ggplot(midwestdf[sample(1:nrow(midwestdf), 3000),], aes(x = year, y = adjusted_weighted_yields)) + geom_point() + stat_smooth(method = 'lm')
