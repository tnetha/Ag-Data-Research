# code for finding R^2 values for various time intervals of different weather elements

library(tidyverse)
# loads necessary functions
source('find_date_interval.R')
source('regression_finder.R')

midwestdf <- readRDS('midwestdf.RDS')

summer_midwestdf <- midwestdf[midwestdf$month >= 04 & midwestdf$month <= 10,]

dates <- format(seq(from = as.Date('04/01','%m/%d'), to = as.Date('10/31', '%m/%d'), by = 'days'), '%m-%d')

dates_df_14 <- find_date_interval(dates, 14)
midwest_r2_df_2week <- lapply(dates_df$start, regression_finder, summer_midwestdf, 14)
midwest_r2_df_2week <- do.call(rbind, midwest_r2_df_2week)
ggplot(midwest_r2_df_2week) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total rainfall vs. yields in the Midwest over 2 week intervals')

dates_df_7 <- find_date_interval(dates, 7)
midwest_r2_df_1week <- lapply(dates_df_7$start, regression_finder, summer_midwestdf, 7)
midwest_r2_df_1week <- do.call(rbind, midwest_r2_df_1week)
ggplot(midwest_r2_df_1week) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total rainfall vs. yields in the Midwest over 1 week intervals')

dates_df_21 <- find_date_interval(dates, 21)
midwest_r2_df_3week <- lapply(dates_df$start, regression_finder, summer_midwestdf, 21)
midwest_r2_df_3week <- do.call(rbind, midwest_r2_df_3week)
ggplot(midwest_r2_df_3week) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total rainfall vs. yields in the Midwest over 2 week intervals')

dates_df_14 <- find_date_interval(dates, 14)
midwest_r2_14 <- lapply(dates_df_14$start, regression_finder, summer_midwestdf, 14)
midwest_r2_14 <- do.call(rbind, midwest_r2_14)
ggplot(midwest_r2_14) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total Growing Degree Days (50 F) vs. yields in the Midwest over 2 week intervals')

dates_df_7 <- find_date_interval(dates, 7)
midwest_r2_7 <- lapply(dates_df_7$start, regression_finder, summer_midwestdf, 7)
midwest_r2_7 <- do.call(rbind, midwest_r2_7)
ggplot(midwest_r2_7) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total Growing Degree Days (50 F) vs. yields in the Midwest over 1 week intervals')


