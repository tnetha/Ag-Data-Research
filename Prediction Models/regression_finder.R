regression_finder <- function(start_date, yield_df, interval) {
  # Input: Start date, dataframe with dates, average temperature, total precipitation, Growing Degree Days (50 F), and yields (BU/AC) 
  # Output: Dataframe with single row containing the start date, end date, and R^2 value for regression of yields and a weather element
  
  start_date <- as.Date(as.character(start_date[[1]]), '%m-%d')
  end_date <- as.Date(as.character(start_date[[1]]), '%m-%d') + interval
  yield_df$month_day <- as.Date(format(yield_df$date, '%m-%d'), '%m-%d')
  
  # Create dataframe with only data that's between start date and end date
  regress_df <- yield_df[(yield_df$month_day >= start_date & yield_df$month_day <= end_date),] %>% 
    dplyr::group_by(year, County) %>% dplyr::summarise(rain_sum = sum(Precip), yield = mean(yields), GDD50_sum = sum(GDD50))
  
  # Linear Regression of yields and weather element
  yield_regress <- lm(yield ~ rain_sum, regress_df)
  regress_summary <- summary(yield_regress)
  regress_vec <- data.frame(start_date=format(start_date, '%m-%d'), end_date=format(end_date, '%m-%d'), r_squared=regress_summary$r.squared)
  regress_vec
}