# In order to gather data from various counties, the FIPS code is needed to pull from API
fips_codes <- read.csv("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", header = F)

# For now, we are just going to look at Rush County, IN to avoid overcomplicating anything.
fips_codes[fips_codes$V4=="Rush County",]
# Rush County FIPS code -> 18139

library(jsonlite)

# Used Postman to download JSON file
rush_2010_pcpn <- fromJSON("http://data.rcc-acis.org/MultiStnData?county=18139&sdate=2010-01-01&edate=2010-12-01&elems=pcpn")

# Creates data frame from JSON
rush_2010_pcpn <- as.data.frame(rush_2010_pcpn)

# Converts list of amounts to vector
myvec <- unlist(rush_2010_pcpn$data.data[rush_2010_pcpn$data.meta$name=='RUSHVILLE'])

# Creates data frame with dates and values
rushville_2010_pcpn <- data.frame(date=seq(as.Date("2010/1/1"),as.Date("2010/12/31"),"days"),precipitation=myvec)

# Replaces 'M's and 'T's in the data with NAs
rushville_2010_pcpn$precipitation[rushville_2010_pcpn$precipitation=='M' | rushville_2010_pcpn$precipitation=='T'] <- NA

# Limits date to beginning of April to beginning of October
summer_pcpn <- rushville_2010_pcpn[rushville_2010_pcpn$date > as.Date('2010/04/01') & rushville_2010_pcpn$date < as.Date('2010/10/01'),] 


# finds dates that are continuous at a certain length (example is 7)
myfunc <- function(dates) {
  nn <- 1
  xx <- 1
  myvar <- 0
  while( xx <= length(dates) / 7 ) {
    if (is.na(dates[nn+6])==T) {
      break
    }
    if (dates[nn+6] - dates[nn] == 6) {
      myvar[xx] <- as.numeric(as.character(dates[nn])) 
      nn <- nn + 6
      xx <- xx + 1 
    }
    else {
      nn <- nn + 1
    }
  }
  myvar
}
myfunc(summer_pcpn$date)

length(myfunc(1:184))
myfunc(seq(from=1,to=184,by=2))
