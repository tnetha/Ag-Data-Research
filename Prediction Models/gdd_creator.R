gdd_creator <- function(func_df, T_b) {
  # Adds a column with Growing Degree Days calculated from average temperature column
  # GDD = T_avg -T_b
  # if T_avg < T_b
  # then T_avg = T_b
  # Base temperatures: 
  # Corn: 50 F
  # Wheat: 32 F
  # Soybeans: 50 F
  base_index <- (func_df$avgt < T_b)
  func_df$avgt[base_index] <- T_b
  func_df[, paste('GDD',T_b,sep = '')] <- func_df$avgt - T_b
  return(func_df)
}


