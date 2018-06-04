library(jsonlite)
test <- fromJSON('/scratch/mentors/dbuckmas/json_files/46003.json')
View(test)

test$data$meta$ll[[1]]

get_loc_df <- function(json_file) {
  json_list <- fromJSON(json_file)
  loc_list <- json_list$data$meta$ll
  station_lat <- 0
  station_lon <- 0
  for (i in 1:length(loc_list)) {
    station_lat[i] <- loc_list[[i]][1]
    station_lon[i] <- loc_list[[i]][2]
  }
  loc_df <- data.frame(lat=station_lat,lon=station_lon)
  loc_df
}
library(stringr)
County_remove <- function(x) str_replace(x, ' County', '')
midwest_counties$V4 <- sapply(midwest_counties$V4, County_remove, USE.NAMES = F)


find_closest_station <- function(json_file) {
  loc_df <- get_loc_df(json_file)
  
}

