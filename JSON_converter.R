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
  raw_json <- fromJSON(cat("http://data.rcc-acis.org/MultiStnData?county=",FIPS,"&sdate=",sdate,"&edate=",edate,"&elems=",elem))
  initial_DF <- as.data.frame(raw_json) # Converts JSON into tree data frame
  # Need to make a data frame with the first column be the date range and the other columns are the data
  # One column per sublist
  # JSON data is divided into two sublists data.meta and data.data
  # Each column has the name of an element in data.meta and contains the data for that element in data.data
}