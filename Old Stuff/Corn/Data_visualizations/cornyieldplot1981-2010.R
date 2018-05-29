# Load packages
install.packages("ggplot2")
install.packages("devtools")
library(devtools)
library(ggplot2)

#Getting corn yields
corn <- read.table("C:\\Users\\nethe\\Documents\\Research\\corn_data.txt", header = TRUE, sep = "\t", fill = TRUE)
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
IN_field_corn <- subset(field_corn, STATE_NAME == 'INDIANA')
IN_field_corn <- IN_field_corn[IN_field_corn$YEAR >= 1981 & IN_field_corn$YEAR <= 2010, ]
IN_field_corn <- IN_field_corn[ IN_field_corn$SOURCE_DESC == "SURVEY", ]
IN_field_cornyields <- IN_field_corn[ IN_field_corn$SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", ]
IN_field_cornyields <- IN_field_cornyields[IN_field_cornyields$AGG_LEVEL_DESC == "COUNTY",]
IN_field_cornyields$COUNTY_NAME <- factor(IN_field_cornyields$COUNTY_NAME)

# Plotting the yield data
yields <- data.frame(year=IN_field_cornyields$YEAR,yield= 
                     as.numeric(as.character(gsub(",",".", IN_field_cornyields$VALUE))))
ggplot(yields, aes(year,yield)) + geom_point() + geom_smooth(method = lm) + ggtitle("Corn Yields (BU / ACRE)") 
                         
# Statistical summary of the data                                                           
summary(lm(yields$yield ~ yields$year, yields))                                                                                    
# Linear regression line is 
# yield = 1.593 * year + (-3.051e+03)