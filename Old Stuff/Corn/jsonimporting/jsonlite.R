library(jsonlite)
install.packages('curl')
weather <- fromJSON("http://data.rcc-acis.org/StnMeta?county=36001&meta=name,valid_daterange&elems=maxt,mint")
weather
example <- fromJSON("example.json")
exampleDF <- data.frame(example)
example

rush2010 <- data.frame(fromJSON("2010rushtemp.json"))
rush2010
mean(as.numeric(rush2010$data.data))
rush2010[rush2010 == "M"] <- NA
avgtemprushville2010 <- rush2010$data.data
avgtemprushville2010 <- data.frame(matrix(unlist(avgtemprushville2010), nrow=365, byrow=T),stringsAsFactors = F)
avgtemprushville2010
mean(
is.na(rush2010) <- !rush2010
length(rush2010$data.data)
class(rush2010$data.data)
rush2010
class(exampleDF$data.data)
rush2010$data.data <- unlist(rush2010$data.data)
