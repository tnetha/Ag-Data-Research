#function for making a data frame of all the precipitation over the last 41 years for one county 
#the years are the columns and the days are the rows
#the headers are messy but we can fix that later if we need
library(jsonlite)
getdata <- function(year){
full_url <- paste0("http://data.rcc-acis.org/MultiStnData?county=18005&sdate=", year, "-04-01&edate=", year,"-10-01&elems=pcpn")
 bart_pcpn_new <- fromJSON(full_url)
 bart_pcpn_new <- as.data.frame(bart_pcpn_new)
 myvec_bart_new <- unlist(bart_pcpn_new$data.data[bart_pcpn_new$data.meta$name=='COLUMBUS'])
 columbus_pcpn_new <- data.frame(1:184 ,precipitation=myvec_bart_new)
 columbus_pcpn_new$precipitation[columbus_pcpn_new$precipitation=='M' | columbus_pcpn_new$precipitation=='T'] <- NA
  return(columbus_pcpn_new)
}
rainfallDf <- getdata(1970) #you can't bind an empty data frame together so you have to just do 1970 at first 
years = 1971:2010
for (year in years){ # takes too long to run 
  yearDf <- getdata(year)
  rainfallDf <- rbind(rainfallDf, yearDf)
}
head(rainfallDf$precipitation, 184)
success <- as.data.frame(split(rainfallDf$precipitation, 1:184))
dim(success)
