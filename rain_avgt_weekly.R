library(tidyverse)
combined_df <- read.csv('/scratch/mentors/dbuckmas/midwest_corn_rain_avgt_df.csv')[,-1]

combined_df$date <- as.Date(as.character(combined_df$date))

dates <- format(seq(from = as.Date('04/01','%m/%d'), to = as.Date('10/31', '%m/%d'), by = 'days'), '%m-%d')

find_date_interval <- function(dates_vec, interval) {
  dates_df <- data.frame(start = dates_vec[-((length(dates_vec)-(interval-2)):length(dates_vec))], end = dates_vec[-(1:(interval-1))])
  dates_df
} 
dates_df <- find_date_interval(dates, 7)

regression_finder <- function(date_row, yield_df, interval) {
  start_date <- as.Date(as.character(date_row[[1]]), '%m-%d')
  end_date <- as.Date(as.character(date_row[[1]]), '%m-%d') + interval
  yield_df$month_day <- as.Date(format(yield_df$date, '%m-%d'), '%m-%d')
  regress_df <- yield_df[(yield_df$month_day >= start_date & yield_df$month_day <= end_date),] %>% dplyr::group_by(year, County) %>% dplyr::summarise(rain_sum = sum(precip),avgt = mean(avgt), yield = mean(yields))
  yield_regress <- lm(yield ~ rain_sum + year + avgt, regress_df)
  regress_summary <- summary(yield_regress)
  regress_vec <- data.frame(start_date=format(start_date, '%m-%d'), end_date=format(end_date, '%m-%d'), r_squared=regress_summary$r.squared)
  regress_vec
}

midwest_r2_1week <- lapply(dates_df$start, regression_finder, combined_df, 7)
midwest_r2_1week <- do.call(rbind, midwest_r2_1week)

write.csv(midwest_r2_1week, '~/Desktop/midwest_r2_1week.csv', row.names = F)
   