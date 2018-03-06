# Function is going to take JSON file and convert it into usable data frame
# As input, function needs 1) FIPS code, 2) start date, 3) end date, and 4) weather element

# FIPS can be found in the below dataframe
fips_codes <- read.csv("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", header = F)


# Data Frame format is as follows:
# Dates     Station1_location_elem     Station2_location_elem
# sdate     ###                        ###
# ...
# edate
jsontoR <- function(FIPS,sdate,edate,elem) { # dates must be in YEAR-MONTH-DAY format
  library(jsonlite)
  raw_json <- fromJSON(paste("http://data.rcc-acis.org/MultiStnData?county=",FIPS,"&sdate=",sdate,"&edate=",edate,"&elems=",elem, sep = ""))
  initial_DF <- as.data.frame(raw_json) # Converts JSON into tree data frame
  first_name <- initial_DF$data.meta$name[[1]]# Only want first station entry data
  firststationvec <- unlist(initial_DF$data.data[initial_DF$data.meta$name == first_name])
  date_range <- seq(as.Date(sdate),as.Date(edate),by='day')
  my_df <- data.frame(days=date_range,first_name = firststationvec)
  my_df$first_name[my_df$first_name == 'T'] <- NA
  my_df$first_name[my_df$first_name == 'M'] <- NA
  colnames(my_df)[2] <- paste(first_name,',',FIPS,sep = "")
  my_df
  # Need to make a data frame with the first column be the date range and the other columns are the data
  # One column per sublist
  # JSON data is divided into two sublists data.meta and data.data
  # Each column has the name of an element in data.meta and contains the data for that element in data.data
}
df <- jsontoR(18139,"2010-01-01","2010-12-31","pcpn")
df
