find_date_interval <- function(dates_vec, interval) {
  # Input: A vector of continuous dates and an interval
  # Output: A dataframe of every possible continuous interval of that size in those dates
  ## Example:
  ## Input: find_date_interval(c('2018-01-01','2018-01-02), 7)
  ## Output: 
  ##  start             end
  ##  '2018-01-01'      '2018-01-07'
  ##  '2018-01-02'      '2018-01-08'
  dates_df <- data.frame(start = dates_vec[-((length(dates_vec)-(interval-2)):length(dates_vec))], end = dates_vec[-(1:(interval-1))])
  dates_df
} 