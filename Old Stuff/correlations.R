library(tibble)
library(plyr)
library(ggplot2)
tsuccess <- as.data.frame(t(success))
<<<<<<< HEAD
myDF <- tibble(year=as.numeric(1970:2010),corn_yields = as.numeric(as.character(rush_yields_70_10$VALUE)), # yields are in BU/AC
               mean40year = as.numeric(as.character(laply(tsuccess,mean, na.rm=T))),
               sum40year = as.numeric(as.character(laply(tsuccess,sum,na.rm=T))))
myvec <- paste('V',1:41,sep = '')
tsuccess[myvec] <- sapply(tsuccess[myvec],as.character)
tsuccess[myvec] <- sapply(tsuccess[myvec],as.numeric)

<<<<<<< HEAD
# get yields for Rush County

=======
# import corn yields
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME == 'INDIANA',] 
cornbelt_corn <- cornbelt_corn[cornbelt_corn$AGG_LEVEL_DESC == 'COUNTY',]
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
cornbelt_corn$STATE_NAME <- factor(cornbelt_corn$STATE_NAME)
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]
rush_yields <- yields[yields$COUNTY_NAME == 'RUSH',]
rush_yields_70_10 <- rush_yields[rush_yields$YEAR >= 1970 & rush_yields$YEAR <= 2010,]

normal_yield <- diff(as.numeric(as.character(rush_yields_70_10$VALUE)))
plot(1970:2010,as.numeric(as.character(rush_yields_70_10$VALUE)))

# write a function that adds a column for every possible two weeks eg 04/01-04/14,04/02-04/15,... 
# and gives a zero or one for if it rained above a certain threshold
# there are 184 weeks so let's do it in terms of 1:184
tsuccess$days <- 1:184
add_week_columns <- function() {
  # to use a for loop or not to use a for loop, that is the question
   
}

# add 178 new columns representing every possible week filled with NAs
new_col <- paste('V',1:178,sep = '')
new_col
for (i in new_col) {
  myDF[i] <- rep(NA,41)
}
# I just committed a terrible sin.
# for every year, find where there have been droughts of a week
drought <- function(vector) {
  vector[vector > 0.1] <- 1
  vector[vector <= .1] <- 0
  vector[is.na(vector)] <- 0
  df <- tibble(day=1:184,rain=vector)
  ind <- myfunc(df$day[df$rain == 0])
  vec <- 1:178
  vec[ind] <- 1
  vec[vec > 1] <- 0 
  vec
}
drought41 <- laply(tsuccess,drought)
dim(as.data.frame(drought41))
df <- data.frame(myDF, as.data.frame(drought41))
df$X1 <- NULL
df_cor <- cor(df)

rep('x',2)
pic_df <- data.frame(x=rep(df$year,each=2),y=as.vector(rbind(df$corn_yields,df$sum40year))
                     ,Type=as.vector(rbind(rep('Corn Yields BU/AC',41),rep('Summer Precipitation Total (in)',41))))
                   #  ,as.vector(rbind(rep('Corn Yields BU/AC',41),rep('Summer Precipitation Total'),41)))

ggplot(data = pic_df) + geom_line(aes(x=x,y=y,color=Type)) + ylab('Precipitation (in) and Yields (BU/AC)') + xlab('Year') + ggtitle('Rush County Corn Yields and Summer Precipitation from 1970-2010')
ggplot(data=data.frame(x=yield_cor)) + geom_histogram(aes(x=x),bins = 10) + xlab('Correlation') + ggtitle('Histogram of Correlations between Corn Yields and Week Long Droughts')
yield_cor <- df_cor[2,6:178]
