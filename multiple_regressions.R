 library(tidyverse)

INdf <- read.csv('/scratch/mentors/dbuckmas/IN_corn_rain_df.csv')[,-1]
INdf <- INdf[order(INdf$date),]
INdf$date <- as.Date(as.character(INdf$date))
# Get rid of everything not summer
INdf$month <- as.numeric(format(INdf$date, '%m'))
summer_INdf <- INdf[INdf$month >= 04 & INdf$month <= 10,]

dates <- format(seq(from = as.Date('04/01','%m/%d'), to = as.Date('10/31', '%m/%d'), by = 'days'), '%m-%d')

find_date_interval <- function(dates_vec, interval) {
  dates_df <- data.frame(start = dates_vec[-((length(dates_vec)-(interval-2)):length(dates_vec))], end = dates_vec[-(1:(interval-1))])
  dates_df
} 
dates_df <- find_date_interval(dates, 14)

regression_finder <- function(date_row, yield_df, interval) {
  start_date <- as.Date(as.character(date_row[[1]]), '%m-%d')
  end_date <- as.Date(as.character(date_row[[1]]), '%m-%d') + interval
  yield_df$month_day <- as.Date(format(yield_df$date, '%m-%d'), '%m-%d')
  regress_df <- yield_df[(yield_df$month_day >= start_date & yield_df$month_day <= end_date),] %>% dplyr::group_by(year, County) %>% dplyr::summarise(rain_sum = sum(Precip), yield = mean(yields))
  yield_regress <- lm(yield ~ rain_sum + year, regress_df)
  regress_summary <- summary(yield_regress)
  regress_vec <- data.frame(start_date=format(start_date, '%m-%d'), end_date=format(end_date, '%m-%d'), r_squared=regress_summary$r.squared)
  regress_vec
}

IN_r2_df_2week <- lapply(dates_df$start, regression_finder, summer_INdf, 14)
IN_r2_df_2week <- do.call(rbind, IN_r2_df_2week)
ggplot(IN_r2_df_2week) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total rainfall over 2 week intervals')

<<<<<<< HEAD
IN_r2_df <- do.call(rbind, IN_r2_df)
max(IN_r2_df$r_squared)
=======
IN_r2_df_1week <- lapply(dates_df$start, regression_finder, summer_INdf, 7)
IN_r2_df_1week <- do.call(rbind, IN_r2_df_1week)
ggplot(IN_r2_df_1week) + geom_histogram(aes(x=r_squared)) + ggtitle('Frequency of R^2 values for total rainfall over 1 week intervals')

>>>>>>> a8eb961049b7cf687fdf60a982ffabcd9f4b7ea6




