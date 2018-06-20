library(tidyverse)

# add column for state
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
fips_code <- data.frame(state = midwest_counties$V1,County = paste(midwest_counties$V2,midwest_counties$V3,sep = ''))
fips_code$County = as.numeric(as.character(fips_code$County))

####


midwestdf <- read.csv('/scratch/mentors/dbuckmas/midwest_corn_rain_avgt_df.csv')[-1]
midwestdf <- merge(midwestdf, fips_code, by = 'County')

#summary(lm(yields ~ year, midwestdf))

yearlydf <- midwestdf %>% group_by(year, County, state) %>% summarise(tot_precip = sum(precip, na.rm = T), avgt = mean(avgt, na.rm = T), yields = mean(yields))

set.seed(101)
train = sample(1:nrow(yearlydf), 30000)
train.lm <- yearlydf[train,]
test.lm <- yearlydf[-train,]
model.lm <- lm(yields ~ year + tot_precip + avgt + state, train.lm)
test.lm$test_predict <- predict(model.lm, test.lm)

ggplot(test.lm) + geom_histogram(aes(x= test.lm$yields - test.lm$test_predict), bins = 100) + scale_x_continuous(limits=c(-100,100))



# Random Forest Model  
library(randomForest)

yearlydf.rf <- randomForest(yields ~ year + tot_precip + avgt + state, data = yearlydf, subset = train, na.action = na.omit, importance = T)
plot(yearlydf.rf)
yearlydf.rf

test.rf <- yearlydf[-(train),]
test.rf$test_predict <- predict(yearlydf.rf, test.rf)
ggplot(test.rf) + geom_histogram(aes(x= test.rf$yields - test.rf$test_predict), bins = 100) + scale_x_continuous(limits=c(-100,100))
varImpPlot(yearlydf.rf)