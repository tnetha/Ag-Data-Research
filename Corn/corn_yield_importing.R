corn <- read.table("/scratch/mentors/dbuckmas/corn_data.txt", header = TRUE, sep = "\t", fill = TRUE)
field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME == 'INDIANA' | field_corn$STATE_NAME == 'ILLINOIS' | field_corn$STATE_NAME == 'OHIO'
                             | field_corn$STATE_NAME == 'WISCONSIN' | field_corn$STATE_NAME == 'MINNESOTA' | field_corn$STATE_NAME == 'IOWA', ] 
cornbelt_corn <- cornbelt_corn[cornbelt_corn$AGG_LEVEL_DESC == 'COUNTY',]
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
cornbelt_corn$STATE_NAME <- factor(cornbelt_corn$STATE_NAME)
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]
