#creates a trendline for the soybean yields from 1981-2010 
#gives an equation for the line as well
library(ggplot2)
beans <- read.table("/scratch/mentors/dbuckmas/soybeans_data.txt", header = TRUE, sep = "\t", fill = TRUE)
midwest_beans <- subset(beans, beans$SOURCE_DESC == "SURVEY")

# Now for states wanted, Corn belt = Indiana, Illinois, Iowa, Missouri, eastern Nebraska, and eastern Kansas
mid_beans <- subset(midwest_beans, midwest_beans$STATE_NAME == "ILLINOIS" | midwest_beans$STATE_NAME == "INDIANA" | midwest_beans$STATE_NAME == "IOWA" | midwest_beans$STATE_NAME == "MISSOURI" | midwest_beans$STATE_NAME == "NEBRASKA" | midwest_beans$STATE_NAME == "KANSAS" | midwest_beans$STATE_NAME == "SOUTH DAKOTA" | midwest_beans$STATE_NAME == "MINNESOTA" | midwest_beans$STATE_NAME == "OHIO" )

trend_beans <- subset(mid_beans, mid_beans$SHORT_DESC == "SOYBEANS - YIELD, MEASURED IN BU / ACRE")

trend_beans <- subset(trend_beans, trend_beans$YEAR >= 1981 & trend_beans$YEAR <= 2010)
trend_beans$VALUE <- factor(trend_beans$VALUE)
trend_beans$YEAR <- factor(trend_beans$YEAR)
trend
trend <- tapply(as.numeric(gsub(",","", trend_beans$VALUE)), trend_beans$YEAR, mean)
table(trend_beans$VALUE)
as.numeric(gsub(",",".", trend_beans$VALUE))
trend <- as.data.frame(trend)
trend$year <- 1981:2010
qplot(trend$year, trend$trend)
fit1 <- lm(trend ~ year, data = trend)
summary(fit1)
abline(fit1)
ggplot(trend, aes(x = year, y = trend)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
