library(pls)
library(tidyverse)
IN_fips <- pull_state_codes('IN')

IN_fips_name <- paste('V', IN_fips,sep = '')

IN_df <- cbind(date = pcpn_df$date, pcpn_df[,IN_fips_name])
View(IN_df)
IN_df$date <- as.factor(format(IN_df$date,'%Y'))

tall_IN_df <- IN_df %>% gather(County, Value, -date)

IN_sums <- tall_IN_df %>% group_by(date, County) %>% summarise(sum = sum(Value, na.rm =T))
View(IN_sums)

IN_sums$County <- as.numeric(gsub('V([0-9]+)', '\\1', IN_sums$County ,perl = T))


