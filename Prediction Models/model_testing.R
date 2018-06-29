library(tidyverse)

# Load dataframe with all of the data
midwestdf <- readRDS('midwestdf.RDS')

# Summarise to get only yearly data
yearlydf <- midwestdf %>% group_by(year, County, state) %>% summarise(tot_precip = sum(precip, na.rm = T), avgt = mean(avgt, na.rm = T), yields = mean(yields), GDD50 = sum(GDD50))

# Set seed so results are reproducible
set.seed(101)

# Generate random numbers to use as training data
train = sample(1:nrow(yearlydf), 30000)


##### Linear Regression

# Create training and test sets for linear regression
train.lm <- yearlydf[train,]
test.lm <- yearlydf[-train,]

# Linear Regression for yields with all explanatory variables
model.lm <- lm(yields ~ year + tot_precip + avgt + state + GDD50, train.lm)

# Predictions of yields for test data based on linear regression model
test.lm$test_predict <- predict(model.lm, test.lm)

# Histogram showing the error of the predicted yields
ggplot(test.lm) + geom_histogram(aes(x= test.lm$yields - test.lm$test_predict), bins = 100) + scale_x_continuous(limits=c(-100,100)) + ggtitle('Linear Regression Error')

# Calculate the error for predicted yields
error.lm <- test.lm$yields - test.lm$test_predict

# Mean and Standard Deviation for predicted yield error
mean(error.lm, na.rm=T)
sd(error.lm, na.rm=T)

#####


##### Random Forest Model  
library(randomForest)

# Generate randomForest model for yields versus all explanatory variables
yearlydf.rf <- randomForest(yields ~ year + tot_precip + avgt + state + GDD50, data = yearlydf, subset = train, na.action = na.omit, importance = T)

# Prediction of test yields based on RandomForest model
test.rf <- yearlydf[-(train),]
test.rf$test_predict <- predict(yearlydf.rf, test.rf)

# Histogram of prediction error for RandomForest Model
ggplot(test.rf) + geom_histogram(aes(x= test.rf$yields - test.rf$test_predict), bins = 100) + scale_x_continuous(limits=c(-100,100)) + ggtitle('Random Forest Error')

# Importance plot of explanatory models
varImpPlot(yearlydf.rf)

# Mean and standard deviation of prediction error
error.rf <- test.rf$yields - test.rf$test_predict
mean(error.rf, na.rm =T)
sd(error.rf, na.rm =T)

##### Plots

# Predicted Yields and Actual Yields over Time (Linear Regression)
ggplot(test.lm) + geom_point(aes(x = year, y = yields, color = 'Actual')) + 
  geom_point(aes(x = year, y = test_predict, color = 'Predicted')) + 
  scale_color_manual(values = c('Actual' = 'red', 'Predicted' = 'blue')) +
  ggtitle('Predicted Yields and Actual Yields over Time (Linear Regression)') + xlab('Year') +
  ylab('Yields (BU/AC)')

# Predicted Yields and Actual Yields over Time (Random Forest)
ggplot(test.rf) + geom_point(aes(x = year, y = yields, color = 'Actual')) + 
  geom_point(aes(x = year, y = test_predict, color = 'Predicted')) + 
  scale_color_manual(values = c('Actual' = 'red', 'Predicted' = 'blue')) +
  ggtitle('Predicted Yields and Actual Yields over Time (Random Forest)') + xlab('Year') +
  ylab('Yields (BU/AC)')


# Model Error
ggplot() + geom_histogram(data=test.rf, aes(x= test.rf$yields - test.rf$test_predict, color = 'Random Forest', fill = 'Random Forest'), bins = 50, alpha = .6) + 
  geom_histogram(data=test.lm, aes(x= test.lm$yields - test.lm$test_predict, color = 'Linear Regression', fill = 'Linear Regression'), bins = 50, alpha = .4) + 
  scale_x_continuous(limits=c(-100,100)) + xlab('Error') + ggtitle('Model Error') + 
  scale_color_manual(name = 'Models', values = c('Random Forest' = 'red', 'Linear Regression' = 'blue')) +
  scale_fill_manual(name = 'Models', values = c('Random Forest' = 'red', 'Linear Regression' = 'blue'))

