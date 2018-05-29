# load libraries
library(ggplot2)
library(data.table)
library(maps)

# read in wheat data
wheat = read.delim("/scratch/mentors/dbuckmas/head.txt", header=T, sep ="\t")

# subset data just from survey
midwest_wheat = subset(wheat, wheat$SOURCE_DESC == "SURVEY")

# subset data from just midwest states
mdw_wheat = subset(midwest_wheat, STATE_NAME %in% c("OHIO","INDIANA","ILLINOIS","IOWA","MISSOURI",
                                                    "NEBRASKA","KANSAS","SOUTH DAKOTA","MINNESOTA"))

# subset yield data meausred in bu/acre
mdw_yields = subset(mdw_wheat, SHORT_DESC == 'WHEAT - YIELD, MEASURED IN BU / ACRE')

# subset yield data for observations measured by county
mdw_county = subset(mdw_yields, AGG_LEVEL_DESC == 'COUNTY')

# separate data into decades
mdw_wheat_80 = subset(mdw_county, YEAR >= 1981 & YEAR <= 1990)
mdw_wheat_90 = subset(mdw_county, YEAR >= 1991 & YEAR <= 2000)
mdw_wheat_00 = subset(mdw_county, YEAR >= 2001 & YEAR <= 2010)

# get list of all US counties
map.county = map_data('county')

# subset just the states and counties
counties = unique(map.county[,5:6])

# subset just the midwest states' counties
midwest_counties = subset(counties, region %in% c("indiana","illinois","iowa","missouri","kansas",
                                                  "nebraska","south dakota","minnesota","ohio"))

# order the data by state
midwest_counties = midwest_counties[order(midwest_counties$region),]

# create a new statecounty column in the data
# so we can distinguish between states if two counties
# have the same name, convert to lowercase
mdw_wheat_80$statecounty = data.frame(paste(mdw_wheat_80$STATE_NAME, mdw_wheat_80$COUNTY_NAME, sep = ","))
mdw_wheat_80$statecounty = sapply(mdw_wheat_80$statecounty, tolower)
mdw_wheat_90$statecounty = data.frame(paste(mdw_wheat_90$STATE_NAME, mdw_wheat_90$COUNTY_NAME, sep = ","))
mdw_wheat_90$statecounty = sapply(mdw_wheat_90$statecounty, tolower)
mdw_wheat_00$statecounty = data.frame(paste(mdw_wheat_00$STATE_NAME, mdw_wheat_00$COUNTY_NAME, sep = ","))
mdw_wheat_00$statecounty = sapply(mdw_wheat_00$statecounty, tolower)

# get average yield per county
mdw_avgyield80 = tapply(as.numeric(gsub(",",".", mdw_wheat_80$VALUE)), mdw_wheat_80$statecounty, mean, na.rm = T)
mdw_avgyield90 = tapply(as.numeric(gsub(",",".", mdw_wheat_90$VALUE)), mdw_wheat_90$statecounty, mean, na.rm = T)
mdw_avgyield00 = tapply(as.numeric(gsub(",",".", mdw_wheat_00$VALUE)), mdw_wheat_00$statecounty, mean, na.rm = T)

# take out other (combined) counties for each decade
mdw_avgyield80 = subset(mdw_avgyield80, 
                        !(names(mdw_avgyield80) %in% c('missouri,other (combined) counties','kansas,other (combined) counties',
                                                       'nebraska,other (combined) counties','indiana,other (combined) counties',
                                                       'ohio,other (combined) counties','illinois,other(combined) counties',
                                                       'iowa,other (combined) counties','minnesota,other (combined) counties',
                                                       'south dakota,other (combined) counties')))

mdw_avgyield90 = subset(mdw_avgyield90, 
                        !(names(mdw_avgyield90) %in% c('missouri,other (combined) counties','kansas,other (combined) counties',
                                                       'nebraska,other (combined) counties','indiana,other (combined) counties',
                                                       'ohio,other (combined) counties','illinois,other (combined) counties',
                                                       'iowa,other (combined) counties','minnesota,other (combined) counties',
                                                       'south dakota,other (combined) counties')))

mdw_avgyield00 = subset(mdw_avgyield00, 
                        !(names(mdw_avgyield00) %in% c('missouri,other (combined) counties','kansas,other (combined) counties',
                                                       'nebraska,other (combined) counties','indiana,other (combined) counties',
                                                       'ohio,other (combined) counties','illinois,other (combined) counties',
                                                       'iowa,other (combined) counties','minnesota,other (combined) counties',
                                                       'south dakota,other (combined) counties')))

# check which counties we don't have data for
V1 = paste(midwest_counties$region,midwest_counties$subregion,sep = ",")
V2 = names(mdw_avgyield80) 
V3 = names(mdw_avgyield90)
V4 = names(mdw_avgyield00)
V2[!(V2 %in% V1)] #what counties are in our data but not in the mapdata database --> keep V1 constant, change V2 to V3,V4
V1[!(V1 %in% V2)] #what counties are in the mapdata database but not in our data --> keep V1 constant, change V2 to V3,V4

# take out counties that are in the mapdata database but not in our data
oh_80 = subset(midwest_counties,region == "ohio" & 
              !(subregion %in% c("athens","cuyahoga","gallia","guernsey",
                                 "harrison","jefferson","lake","lawrence",
                                 "meigs","monroe","noble","vinton")))
il_80 = subset(midwest_counties, region == "illinois")
in_80 = subset(midwest_counties, region == "indiana" & subregion != "st joseph")               
ia_80 = subset(midwest_counties, region == "iowa" & 
              !(subregion %in% c("emmet","grundy","hamilton","hancock",
                                 "humboldt","obrien","pocahontas","worth",
                                 "wright")))
sd_80 = subset(midwest_counties, region == "south dakota" & subregion != "shannon")
mn_80 = subset(midwest_counties, region == "minnesota" & 
              !(subregion %in% c("cook","lake","ramsey","st louis")))
ne_80 = subset(midwest_counties, region == "nebraska" & subregion != "hooker")
miz_80 = subset(midwest_counties, region == "missouri" & subregion != "st louis city")
ka_80 = subset(midwest_counties, region == "kansas")
counties_80 = rbind(oh_80, il_80, in_80, ia_80, sd_80, mn_80, ne_80, miz_80, ka_80)


oh_90 = subset(midwest_counties, region == "ohio" & 
              !(subregion %in% c("athens","belmont","cuyahoga","gallia",
                                 "guernsey","hamilton","harrison","hocking",
                                 "jefferson","lake","lawrence","monroe",
                                 "morgan","noble","summit","vinton")))
il_90 = subset(midwest_counties, region == "illinois")
in_90 = subset(midwest_counties, region == "indiana" & subregion != "st joseph")               
ia_90 = subset(midwest_counties, region == "iowa" & 
              !(subregion %in% c("audubon","black hawk","bremer","buena vista",
                                   "calhoun", "cerro gordo","clay","emmet","franklin",
                                   "hamilton","hancock","hardin","humboldt","kossuth",
                                   "mitchell" , "obrien","palo alto","pocahontas",
                                   "webster","worth","wright")))
sd_90 = subset(midwest_counties, region == "south dakota" & subregion != "shannon")
mn_90 = subset(midwest_counties, region == "minnesota" & 
              !(subregion %in% c("carlton","cass","chisago","cook","crow wing",
                                 "dodge","fillmore","freeborn","houston","itasca",
                                 "jackson","lake","martin","mille lacs","mower",
                                 "olmsted","ramsey","rock","st louis","wabasha",
                                 "wadena","watonwan","winona")))
ne_90 = subset(midwest_counties,region == "nebraska" & !(subregion %in% c("hooker","grant")))
miz_90 = subset(midwest_counties, region == "missouri" & 
                !(subregion %in% c("camden","carter","dent","douglas","iron",
                                   "madison","ozark","phelps","pulaski","reynolds",
                                   "st louis city","shannon","stone","taney",
                                   "texas","washington")))
ka_90 = subset(midwest_counties, region == "kansas")
counties_90 = rbind(oh_90, il_90, in_90, ia_90, sd_90, mn_90, ne_90, miz_90, ka_90)



oh_00 = subset(midwest_counties, region == "ohio" & 
                !(subregion %in% c("belmont","cuyahoga", "gallia","guernsey","meigs",
                                   "hamilton","harrison","hocking","jefferson","lake",
                                   "lawrence","monroe","morgan","noble","summit", "vinton")))
il_00 = subset(midwest_counties, region == "illinois" & 
                !(subregion %in% c("cook","de witt", "du page","hardin","pope",
                                   "rock island")))
in_00 = subset(midwest_counties, region == "indiana" & 
                !(subregion %in% c("benton","blackford", "brown","crawford","dearborn",
                                   "fayette","floyd","greene","jasper","lawrence","marion",
                                   "martin","monroe","morgan","newton","ohio","perry","pike",
                                   "putnam","st joseph","scott","starke","switzerland","union",
                                   "vermillion","vigo","warren")))
ia_00 = subset(midwest_counties, region == "iowa" & 
                !(subregion %in% c("adair","adams","allamakee","audubon","benton","black hawk",
                                   "boone","bremer","buchanan","buena vista","butler","calhoun",
                                   "carroll","cass","cedar","cerro gordo","cherokee","chickasaw",
                                   "clay","clinton","crawford","dallas","dickinson","dubuque",
                                   "emmet","fayette","floyd","franklin","fremont","greene","grundy",
                                   "guthrie","hamilton","hancock","hardin","howard","humboldt","ida",
                                   "jackson","jasper","jones","kossuth","linn","lucas","lyon","madison",
                                   "marion","marshall","mitchell","monona","montgomery","obrien","osceola",
                                   "palo alto","plymouth","pocahontas","polk","poweshiek","sac","scott",
                                   "shelby","sioux","story","tama","taylor","union","warren","webster",
                                   "winnebago","winneshiek","woodbury","worth","wright")))
sd_00 = subset(midwest_counties, region == "south dakota" & subregion != "shannon" & subregion != "union")
mn_00 = subset(midwest_counties, region == "minnesota" & 
                !(subregion %in% c("blue earth","carlton","cass","chisago","cook","crow wing","dodge",
                                   "faribault","fillmore","freeborn","hennepin","houston" ,"itasca","jackson",
                                   "lake" ,"martin","mille lacs","mower","nobles","olmsted","pine","ramsey",
                                   "rice","rock","st louis","sherburne","steele","wabasha","waseca",
                                   "washington","watonwan","winona")))
ne_00 = subset(midwest_counties, region == "nebraska" & 
                !(subregion %in% c("burt","dakota","dixon","garfield","grant","hooker","loup","thomas")))
miz_00 = subset(midwest_counties, region == "missouri" & 
                !(subregion %in% c("camden","carter","crawford","dent","douglas","iron","mcdonald",
                                   "madison","oregon","ozark","pulaski","reynolds","st louis city","shannon",
                                   "stone","texas","washington","wright")))
ka_00 = subset(midwest_counties, region == "kansas")
counties_00 = rbind(oh_00, il_00, in_00, ia_00, sd_00, mn_00, ne_00, miz_00, ka_00)


# taking out counties in the data but not in the mapdata database
mdw_avgyield80 = subset(mdw_avgyield80, 
                        !(names(mdw_avgyield80)%in% c("indiana,st. joseph","iowa,o brien",
                                                      "minnesota,st. louis", "missouri,ste. genevieve",
                                                      "south dakota,oglala lakota")))

mdw_avgyield90 = subset(mdw_avgyield90, 
                        !(names(mdw_avgyield90)%in% c("indiana,st. joseph","iowa,o brien",
                                                      "minnesota,st. louis", "missouri,ste. genevieve",
                                                      "south dakota,oglala lakota")))

mdw_avgyield00 = subset(mdw_avgyield00, 
                        !(names(mdw_avgyield00)%in% c("indiana,st. joseph","south dakota,oglala lakota")))

# making yield map for 1981-1990, old map
yieldmap80 = data.frame(state_names = counties_80$region,county_names = counties_80$subregion, yield = mdw_avgyield80 )
map.county1 = data.table(subset(map.county, region %in% 
                                  c("indiana","illinois","kansas","nebraska","missouri","iowa",
                                    "south dakota","minnesota","ohio")))
setkey(map.county1,region,subregion)
yieldmap80 = data.table(yieldmap80)
setkey(yieldmap80,state_names,county_names)
map.df80 = map.county1[yieldmap80]
a = scale_fill_gradient(low = "white", high = "black", limits = c(15,85)) # my gradient scale
ggplot(map.df80, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ggtitle("Average Wheat Yields 1981-1990") + a

# create base layer for new map --> creates foundation for borders
# not really sure why it works but it works so i won't question it
# only needs to be done once
states = map_data('state')
mdw_states = subset(states, region %in% c("ohio", "indiana", "illinois", "iowa", 
                                          "minnesota", "missouri", "kansas", "nebraska",
                                          "south dakota"))
mdw_base = ggplot(data = mdw_states, mapping = aes(x = long, y=lat, group=group)) + coord_fixed(1.3) + geom_polygon(color = 'black', fill = NA, size = 0.2)

# add county yields on top of base layer, note change in data for geom_polygon
# and on top of that add the state borders again
mdw_base + geom_polygon(data = map.df80, aes(fill = yield)) + geom_polygon(color = "black", fill = NA, size = 0.2) + ggtitle("Average Wheat Yields 1981-1990") + a


# now do the same for 1991-2000 - old map
yieldmap90 = data.frame(state_names = counties_90$region,county_names = counties_90$subregion, yield = mdw_avgyield90 )
setkey(map.county1,region,subregion)
yieldmap90 = data.table(yieldmap90)
setkey(yieldmap90,state_names,county_names)
map.df90 = map.county1[yieldmap90]
ggplot(map.df90, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ ggtitle("Average Wheat Yields 1991-2000") + a

# new map
mdw_base + theme_minimal() + geom_polygon(data = map.df90, aes(fill = yield)) + geom_polygon(color = "black", fill = NA, size = 0.2) + ggtitle("Average Wheat Yields 1991-2000")+ a

# now the same for 2001-2010
yieldmap00 = data.frame(state_names = counties_00$region,county_names = counties_00$subregion, yield = mdw_avgyield00 )
setkey(map.county1,region,subregion)
yieldmap00 = data.table(yieldmap00)
setkey(yieldmap00,state_names,county_names)
map.df00 = map.county1[yieldmap00]
ggplot(map.df00, aes(x=long, y=lat, group = group, fill=yield)) + geom_polygon()+coord_map()+ ggtitle("Average Wheat Yields 2001-2010") + a

#new map
mdw_base + theme_minimal() + geom_polygon(data = map.df00, aes(fill = yield)) + geom_polygon(color = "black", fill = NA, size = 0.2) + ggtitle("Average Wheat Yields 2001-2010") + a
